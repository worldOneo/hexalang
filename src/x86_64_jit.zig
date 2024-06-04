const jit = @import("./generic_jit.zig");
const std = @import("std");

const YieldStatus = enum(u8) {
    RequireMoreStack = 0,
    EndOfExecution = 1,
    ExecutionPanick = 2,
};

// Use RAX, RCX, RDX, RBX, RSI, RDI, R8, R9, R10 (0,1,2,3,6,7,8,9,10) as temporary registers
// Use RSP and RBP (4,5) as stack registers but stack grows upwards for resizing
// Use R11 as stack end
// Use R12 for temporary values
// Use R13 as yield address
// Use R14 as yield status register
// Use R15 as yield continue register
//
//
// Stack layout
// ...
// nextFn local 1
// nextFn Arg N
// ...
// nextFn Arg 1
// nextFn ret N
// ...
// nextFn ret 1
// return addr
// prevFn local N
// ...
const X86_64jit = struct {
    const TempCPURegisters = [_]u64{ 0, 1, 2, 3, 6, 7, 8, 9, 10 };
    const Registers = jit.LRURegisterAllocator(TempCPURegisters.len, TempCPURegisters);

    registers: Registers = Registers.init(),

    pub fn init(allocator: std.mem.Allocator) std.mem.Allocator.Error!*X86_64jit {
        var ptr = try allocator.create(X86_64jit);
        ptr.* = X86_64jit{};
        return ptr;
    }

    fn maskRegister(reg: u64, part: jit.RegisterPart, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        // const onlylower32bits = jit.instgen("{@1[3-4]=(01000|@1[3-4]|00)}x89(11|@1[0-3]|@1[0-3])"){};
        const onlylower16bits = jit.instgen("x66{1[3-4]=(01000|@1[3-4]|00)}x89(11|@1[0-3]|@1[0-3])"){};
        const onlylower8bits = jit.instgen("{1[3-4]=(01000|@1[3-4]|00)}x88(11|@1[0-3]|@1[0-3])"){};
        const shr = jit.instgen("(0100100|@1[3-4])xc1(11101|@1[0-3])$2[0-1]"){};

        if (part == jit.RegisterPart.QW) {
            return;
        }

        if (part.popCount() == 1) {
            const shiftBits = part.ctz() * 8;
            if (shiftBits != 0) {
                _ = try shr.write(out, .{ reg, shiftBits });
            }
            if (shiftBits != 56) {
                _ = try onlylower8bits.write(out, .{reg});
            }
        }

        if (part.popCount() == 2) {
            const shiftBits = part.ctz() * 8;
            if (shiftBits != 0) {
                _ = try shr.write(out, .{ reg, shiftBits });
            }
            if (shiftBits != 48) {
                _ = try onlylower16bits.write(out, .{reg});
            }
        }

        if (part.popCount() == 4) {
            const shiftBits = part.ctz() * 8;
            if (shiftBits != 0) {
                _ = try shr.write(out, .{ reg, shiftBits });
            }
        }
    }

    pub fn loadVirtualRegister(self: *X86_64jit, register: jit.Register, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {
        const mov = jit.instgen("(01001|@1[3-4]|00)x8b(10|@1[0-3]|101)$2[0-4]"){};

        const claim = self.registers.claimRegister(register.part, register.number);
        const destRegister = claim.claimed;

        if (claim.mustStore) |old| {
            try self.storeVirtualRegister(jit.Register{
                .function = register.function,
                .number = old.vreg,
                .part = old.part,
            }, claim.claimed, out);
        }
        if (claim.loaded) {
            return destRegister;
        }

        const f = register.function;
        const offset = -@as(i64, @intCast((f.registers_required + f.max_call_arg_registers + f.max_call_return_registers + 1 - register.number))) * 8;
        const u64offset = @as(u64, @bitCast(offset));
        _ = try mov.write(out, .{ destRegister, u64offset });
        try maskRegister(destRegister, register.part, out);
        return destRegister;
    }

    pub fn storeVirtualRegister(_: *X86_64jit, register: jit.Register, loaded_at: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        const movQW = jit.instgen("(01001|@1[3-4]|00)x89(10|@1[0-3]|101)$2[0-4]"){};
        const movDW = jit.instgen("{1[3-4]=(01000|@1[3-4]|00)}x89(10|@1[0-3]|101)$2[0-4]"){};
        const movW = jit.instgen("x66{1[3-4]=(01000|@1[3-4]|00)}x89(10|@1[0-3]|101)$2[0-4]"){};
        const movB = jit.instgen("{1[3-4]=(01000|@1[3-4]|00)}x88(10|@1[0-3]|101)$2[0-4]"){};
        const f = register.function;
        var offset = -@as(i64, @intCast((f.registers_required + f.max_call_arg_registers + f.max_call_return_registers + 1 - register.number))) * 8;
        offset += @intCast(register.part.clz());
        const u64offset = @as(u64, @bitCast(offset));
        _ = switch (register.part.popCount()) {
            1 => try movB.write(out, .{ loaded_at, u64offset }),
            2 => try movW.write(out, .{ loaded_at, u64offset }),
            4 => try movDW.write(out, .{ loaded_at, u64offset }),
            8 => try movQW.write(out, .{ loaded_at, u64offset }),
            else => unreachable,
        };
    }

    pub fn loadSavedVirtualRegister(self: *X86_64jit, register: jit.Register, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {
        return self.loadVirtualRegister(register, out);
    }

    pub fn restoreVirtualRegisters(_: *X86_64jit, _: jit.FnData, _: *std.ArrayList(u8)) std.mem.Allocator.Error!void {}
    pub fn saveVirtualRegisters(_: *X86_64jit, _: jit.FnData, _: *std.ArrayList(u8)) std.mem.Allocator.Error!void {}
    pub fn allocateReturnArgRegisters(_: *X86_64jit, _: jit.FnData, _: jit.FnData, _: *std.ArrayList(u8)) std.mem.Allocator.Error!void {}

    pub fn loadFromReturnRegister(self: *X86_64jit, currentFn: jit.FnData, _: jit.FnData, part: jit.RegisterPart, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {
        return self.loadVirtualRegister(jit.Register{
            .function = currentFn,
            .number = currentFn.registers_required + 1 + number,
            .part = part,
        }, out);
    }

    pub fn storeToArgRegister(self: *X86_64jit, currentFn: jit.FnData, callFn: jit.FnData, part: jit.RegisterPart, loaded_at: u64, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        return self.storeVirtualRegister(jit.Register{
            .part = part,
            .function = currentFn,
            .number = currentFn.registers_required + 1 + callFn.return_value_registers + number,
        }, loaded_at, out);
    }

    pub fn storeToReturnRegister(_: *X86_64jit, currentFn: jit.FnData, part: jit.RegisterPart, loaded_at: u64, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        const movQW = jit.instgen("(01001|@1[3-4]|00)x89(10|@1[0-3]|101)$2[0-4]"){};
        const movDW = jit.instgen("{1[3-4]=(01000|@1[3-4]|00)}x89(10|@1[0-3]|101)$2[0-4]"){};
        const movW = jit.instgen("x66{1[3-4]=(01000|@1[3-4]|00)}x89(10|@1[0-3]|101)$2[0-4]"){};
        const movB = jit.instgen("{1[3-4]=(01000|@1[3-4]|00)}x88(10|@1[0-3]|101)$2[0-4]"){};
        const f = currentFn;

        var offset = -@as(i64, @intCast(f.max_call_return_registers + f.registers_required + f.max_call_arg_registers + 1 + number + 1));
        offset += @intCast(part.clz());
        const u64offset: u64 = @bitCast(offset * 8);
        _ = switch (part.popCount()) {
            1 => try movB.write(out, .{ loaded_at, u64offset }),
            2 => try movW.write(out, .{ loaded_at, u64offset }),
            4 => try movDW.write(out, .{ loaded_at, u64offset }),
            8 => try movQW.write(out, .{ loaded_at, u64offset }),
            else => unreachable,
        };
    }

    pub fn loadconst(self: *X86_64jit, val: u64, register: jit.Register, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {
        const movabsq = jit.instgen("(0100100|@1[3-4])(10111|@1[0-3])$2[0-8]"){};
        const claim = self.registers.claimRegister(register.part, register.number);
        if (claim.mustStore) |old| {
            try self.storeVirtualRegister(jit.Register{
                .function = register.function,
                .number = old.vreg,
                .part = old.part,
            }, claim.claimed, out);
        }
        _ = try movabsq.write(out, .{ claim.claimed, val });
        return claim.claimed;
    }

    pub fn biop(self: *X86_64jit, operation: jit.BiOp, part: jit.OpSize, left: u64, right: u64, dest: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        self.registers.touchRegister(left);
        self.registers.touchRegister(right);
        self.registers.touchRegister(dest);

        if (dest != left) {
            const movregreg = jit.instgen("(01001|@1[3-4]|0|@2[3-4])x89(11|@2[0-3]|@1[0-3])"){};
            _ = try movregreg.write(out, .{ dest, left });
        }

        const opcode: u64 = switch (operation) {
            jit.BiOp.ADD => 0x01,
            jit.BiOp.AND => 0x21,
            jit.BiOp.XOR => 0x32,
            jit.BiOp.SUB => 0x29,
            jit.BiOp.OR => 0x09,
            else => unreachable, // TODO: Add the ops where intel was high while designing
        };

        const isWord: u64 = if (part == jit.OpSize.W) 1 else 0;
        const maybeByteOp: u64 = if (part == jit.OpSize.B) opcode - 1 else opcode;
        //                         |Byte 0x66 ||REX =    64bit+ExtL?+ExtR?      | ExtL?+ExtR?           ||ALU-OP||RM L+R            |
        const aluOp = jit.instgen("{1[0-8]=x66}{2[7-8]=(01001|@4[3-4]|0|@3[3-4]):(01000|@4[3-4]|0|@3[3-4])}@2[0-1](11|@4[0-3]|@3[0-3])"){};
        _ = try aluOp.write(out, .{ isWord, maybeByteOp, dest, right });
    }

    pub fn jmpif(self: *X86_64jit, label: jit.Label, reg: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?jit.LabelFix {
        const testreg = jit.instgen("(01001@1[3-4]0@1[3-4])x85(11@1[0-3]@1[0-3])"){};
        const skipbranch = jit.instgen("x74x0d"){};
        _ = try testreg.write(out, .{reg});
        _ = try skipbranch.write(out, .{});
        return self.jmp(label, out);
    }

    pub fn jmp(_: *X86_64jit, label: jit.Label, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?jit.LabelFix {
        const jmpr12 = jit.instgen("x41xffxe4"){};
        const movabsq = jit.instgen("(0100100|@1[3-4])(10111|@1[0-3])$2[0-8]"){};
        const offset = out.items.len;
        _ = try movabsq.write(out, .{ 12, label.known_at_offset orelse 0 });
        _ = try jmpr12.write(out, .{});

        if (label.known_at_offset) |_| {
            return null;
        } else {
            return jit.LabelFix{
                .at_location = offset,
                .compiler_info = 0,
                .name = label.name,
            };
        }
    }

    pub fn call(self: *X86_64jit, currentFn: jit.FnData, callFn: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?jit.LabelFix {
        // lea r12, continue
        // mov [return], r12
        // sub rbp, notneeded
        // movabsq r12, function
        // jmp r12
        // continue:
        // add rbp, notneeded
        const lear12rip = jit.instgen("x4cx8dx25x24x00x00x00"){};
        _ = try lear12rip.write(out, .{});
        _ = try self.storeVirtualRegister(jit.Register{ .function = currentFn, .part = jit.RegisterPart.QW, .number = currentFn.registers_required + 1 }, 12, out);

        const notneeded = ((currentFn.max_call_arg_registers + currentFn.max_call_return_registers) - (callFn.return_value_registers)) * 8;
        const subrbp = jit.instgen("x48x81xed$1[0-4]"){};
        _ = try subrbp.write(out, .{notneeded});

        const addrbp = jit.instgen("x48x81xc5$1[0-4]"){};
        const jmpr12 = jit.instgen("x41xffxe4"){};
        const movabsq = jit.instgen("(0100100@1[3-4])(10111|@1[0-3])$2[0-8]"){};
        const offset = out.items.len;
        _ = try movabsq.write(out, .{ 12, 0 });
        _ = try jmpr12.write(out, .{});
        _ = try addrbp.write(out, .{notneeded});

        return jit.LabelFix{
            .at_location = offset,
            .compiler_info = 1,
            .name = callFn.name,
        };
    }

    pub fn fnreturn(_: *X86_64jit, currentFn: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?jit.LabelFix {
        const movQW = jit.instgen("(01001|@1[3-4]|00)x8b(10|@1[0-3]|100)x24$2[0-4]"){};
        const jmpr12 = jit.instgen("x41xffxe4"){};

        const offset = @as(u64, @bitCast(-@as(i64, @intCast(currentFn.return_value_registers + 1)) * 8));
        _ = try movQW.write(out, .{ 12, offset });
        _ = try jmpr12.write(out, .{});
        return null;
    }

    pub fn yield(_: *X86_64jit, _: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        const lear15ripadd3 = jit.instgen("x4cx8dx3dx03x00x00x00"){};
        const jmpr13 = jit.instgen("x41xffxe5"){};
        _ = try lear15ripadd3.write(out, .{});
        _ = try jmpr13.write(out, .{});
    }

    pub fn fnprologue(_: *X86_64jit, data: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        // this is the prologue to ensure enough stackspace
        // try_enter:           ; try_enter:
        //   mov r12, rbp       ;   r12 = rbp
        //   add r12, 1234123   ;   r12 += offset
        //   cmp r12, r11       ;   if(r12 > stack_end)
        //   jbe enter          ;   {
        //   xor r14, r14       ;      yield_status = require_more_stack
        //   lea r15, [rip+3]   ;      yield_return = retry
        //   jmp r13            ;      yield
        //   jmp try_enter      ;      retry: goto try_enter
        // enter:               ;   }
        // add rbp, 1234123     ;   rbp += offset
        const prologue = jit.instgen("x49x89xec|x49x81xc4$1[0-4]|x4dx39xdc|x76x0f|x4dx31xf6|x4cx8dx3dx03x00x00x00|x41xffxe5|xebxe2|x48x81xc5$1[0-4]"){};
        _ = try prologue.write(out, .{(data.registers_required + data.max_call_arg_registers + data.max_call_return_registers + 1) * 8});
    }
    pub fn fnepilogue(_: *X86_64jit, data: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        const subrbp = jit.instgen("x48x81xed$1[0-4]"){};
        _ = try subrbp.write(out, .{(data.registers_required + data.max_call_arg_registers + data.max_call_return_registers + 1) * 8});
    }

    pub fn invalidateLoadedReg(self: *X86_64jit, currentfn: jit.FnData, reg: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        var iter = self.registers.iter();
        while (iter.next()) |n| {
            if (n.vreg == reg) {
                const returned = self.registers.returnRegister(n.idx);
                _ = try self.storeVirtualRegister(jit.Register{
                    .function = currentfn,
                    .number = reg,
                    .part = returned.part,
                }, n.idx, out);
            }
        }
    }
    // fn fixjmp(self: *X86_64jit, jmpfix: jit.LabelFix, absolutreallocation: u64) void {}
};

pub fn create_x86_64_arch(allocator: std.mem.Allocator) std.mem.Allocator.Error!jit.Arch {
    return jit.createJITFrom(X86_64jit, try X86_64jit.init(allocator));
}
