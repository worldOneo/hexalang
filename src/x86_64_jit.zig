const jit = @import("./generic_jit.zig");
const std = @import("std");

// Use RAX, RCX, RDX, RBX, RSI, RDI, R8, R9, R10 (0,1,2,3,6,7,8,9,10) as temporary registers
// Use RSP and RBP (4,5) as stack registers but stack grows upwards for resizing
// Use R11 as yield payload
// Use R12 for temporary values
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
    const TempCPURegisters = []u64{ 0, 1, 2, 3, 6, 7, 8, 9, 10 };
    const Registers = jit.LRURegisterAllocator(TempCPURegisters.len, TempCPURegisters);

    registers: Registers = Registers.init(),

    fn maskRegister(reg: u64, part: jit.PartialRegister, out: *std.ArrayList(u8)) std.mem.Allocator!void {
        // const onlylower32bits = jit.instgen("{@1[3-4]=(01000|@1[3-4]|00)}x89(11|@1[0-3]|@1[0-3])"){};
        const onlylower16bits = jit.instgen("x66{@1[3-4]=(01000|@1[3-4]|00)}x89(11|@1[0-3]|@1[0-3])"){};
        const onlylower8bits = jit.instgen("{@1[3-4]=(01000|@1[3-4]|00)}x88(11|@1[0-3]|@1[0-3])"){};
        const shr = jit.instgen("(0100100|@1[3-4])xc1(11101|@1[0-3])$2[0-1]"){};

        if (part == jit.PartialRegister.QW) {
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

    fn stackRegisterOffset(number: u64) u64 {
        return number * 8;
    }

    pub fn loadVirtualRegister(self: *X86_64jit, part: jit.PartialRegister, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {
        const mov = jit.instgen("(01001|@1[3-4]|00)x8b(10|@1[0-3]|101)$2[0-4]"){};

        const claim = self.registers.claimRegister(part, number);
        const destRegister = claim.claimed;

        if (claim.mustStore) |old| {
            try self.storeVirtualRegister(old.part, destRegister, old.vreg, out);
        }
        if (claim.loaded) {
            return destRegister;
        }

        const offset = stackRegisterOffset(number);
        _ = try mov.write(out, .{ destRegister, offset });
        try maskRegister(destRegister, part, out);
        return destRegister;
    }

    pub fn storeVirtualRegister(_: *X86_64jit, part: jit.PartialRegister, loaded_at: u64, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        const movQW = jit.instgen("(01001|@1[3-4]|00)x89(10|@1[0-3]|101)$2[0-4]"){};
        const movDW = jit.instgen("{@1[3-4]=(01000|@1[3-4]|00)}x89(10|@1[0-3]|101)$2[0-4]"){};
        const movW = jit.instgen("x66{@1[3-4]=(01000|@1[3-4]|00)}x89(10|@1[0-3]|101)$2[0-4]"){};
        const movB = jit.instgen("{@1[3-4]=(01000|@1[3-4]|00)}x88(10|@1[0-3]|101)$2[0-4]"){};

        var offset = stackRegisterOffset(number);
        offset += (7 - part.ctz());
        _ = switch (part.popCount()) {
            1 => try movB.write(out, .{ loaded_at, offset }),
            2 => try movW.write(out, .{ loaded_at, offset }),
            4 => try movDW.write(out, .{ loaded_at, offset }),
            8 => try movQW.write(out, .{ loaded_at, offset }),
        };
    }

    pub fn loadSavedVirtualRegister(self: *X86_64jit, part: jit.PartialRegister, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {
        return self.loadVirtualRegister(part, number, out);
    }

    pub fn restoreVirtualRegisters(_: *X86_64jit, _: u64, _: *std.ArrayList(u8)) std.mem.Allocator!void {}
    pub fn saveVirtualRegisters(_: *X86_64jit, _: u64, _: *std.ArrayList(u8)) std.mem.Allocator!void {}

    pub fn allocateReturnRegisters(_: *X86_64jit, _: jit.FnData, callFn: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        const addRsp = jit.instgen("x48x81xc4@[0-4]"){};
        _ = try addRsp.write(out, .{(callFn.return_value_registers) * 8});
    }

    pub fn loadFromReturnRegister(self: *X86_64jit, currentFn: jit.FnData, _: jit.FnData, part: jit.PartialRegister, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {
        return self.loadVirtualRegister(part, currentFn.registers_required + 1 + number, out);
    }

    pub fn storeToArgRegister(self: *X86_64jit, currentFn: jit.FnData, callFn: jit.FnData, part: jit.PartialRegister, loaded_at: u64, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        return self.storeVirtualRegister(part, loaded_at, currentFn.registers_required + 1 + callFn.return_value_registers + number, out);
    }

    pub fn storeToReturnRegister(_: *X86_64jit, currentFn: jit.FnData, part: jit.PartialRegister, loaded_at: u64, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        const movQW = jit.instgen("(01001|@1[3-4]|00)x89(10|@1[0-3]|100)x24$2[0-4]"){};
        const movDW = jit.instgen("{@1[3-4]=(01000|@1[3-4]|00)}x89(10|@1[0-3]|100)x24$2[0-4]"){};
        const movW = jit.instgen("x66{@1[3-4]=(01000|@1[3-4]|00)}x89(10|@1[0-3]|100)x24$2[0-4]"){};
        const movB = jit.instgen("{@1[3-4]=(01000|@1[3-4]|00)}x88(10|@1[0-3]|100)x24$2[0-4]"){};

        var offset = -@as(i64, @intCast(currentFn.return_value_registers)) + number;
        offset += @intCast(7 - part.ctz());
        const u64offset: u64 = @bitCast(offset);
        _ = switch (part.popCount()) {
            1 => try movB.write(out, .{ loaded_at, u64offset }),
            2 => try movW.write(out, .{ loaded_at, u64offset }),
            4 => try movDW.write(out, .{ loaded_at, u64offset }),
            8 => try movQW.write(out, .{ loaded_at, u64offset }),
        };
    }

    pub fn loadconst(self: *X86_64jit, val: u64, part: jit.PartialRegister, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator!u64 {
        const movabsq = jit.instgen("(01001|@1[3-4]|00)(10111|@1[0-3])$2[0-8]"){};
        const claim = self.registers.claimRegister(part, number);
        if (claim.mustStore) |old| {
            try self.storeVirtualRegister(old.part, claim.claimed, old.vreg, out);
        }
        _ = try movabsq.write(out, .{val});
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
            _ => unreachable, // TODO: Add the ops where intel was high while designing
        };

        const isWord: u64 = if (part == jit.OpSize.W) 1 else 0;
        const maybeByteOp: u64 = if (part == jit.OpSize.B) opcode - 1 else opcode;
        //                         |Byte 0x66  ||REX =     64bit+ExtL?+ExtR?                  ExtL+ExtR?                             ||ALU-OP||RM L+R            |
        const aluOp = jit.instgen("{@1[0-8]=x66}{@2[7-8]=(01001|@4[3-4]|0|@3[3-4]):{@4[3-4]=(0100010|@3[3-4]):{@3[3-4]=(01000001):()}}}@2[0-1](11|@4[0-3]|@3[0-3])"){};
        _ = try aluOp.write(out, .{ isWord, @as(u64, @intCast(@intFromEnum(part))), dest, right, opcode, maybeByteOp });
    }

    fn jmpif(self: *X86_64jit, label: jit.Label, reg: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?jit.LabelFix {
        const testreg = jit.instgen("(01001@1[3-4]0@1[3-4])x85(11@1[0-3]@1[0-3])"){};
        const skipbranch = jit.instgen("x74x0d"){};
        _ = try testreg.write(out, .{reg});
        _ = try skipbranch.write(out, .{});
        return self.jmp(label, out);
    }

    fn jmp(_: *X86_64jit, label: jit.Label, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?jit.LabelFix {
        const jmpr12 = jit.instgen("x41xffxe4"){};
        const movabsq = jit.instgen("(01001|@1[3-4]|00)(10111|@1[0-3])$2[0-8]"){};
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

    fn call(self: *X86_64jit, currentFn: jit.FnData, callFn: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?jit.LabelFix {
        const lear12rip = jit.instgen("x4cx8dx25x16x00x00x00"){};
        _ = try lear12rip.write(out, .{});
        _ = try self.storeVirtualRegister(jit.PartialRegister.QW, 12, currentFn.registers_required + 1, out);
        const jmpr12 = jit.instgen("x41xffxe4"){};
        const movabsq = jit.instgen("(01001|@1[3-4]|00)(10111|@1[0-3])$2[0-8]"){};
        const offset = out.items.len;
        _ = try movabsq.write(out, .{ 12, 0 });
        _ = try jmpr12.write(out, .{});
        return jit.LabelFix{
            .at_location = offset,
            .compiler_info = 1,
            .name = callFn.name,
        };
    }

    fn fnreturn(_: *X86_64jit, currentFn: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?jit.LabelFix {
        const movQW = jit.instgen("(01001|@1[3-4]|00)x8b(10|@1[0-3]|100)x24$2[0-4]"){};
        const jmpr12 = jit.instgen("x41xffxe4"){};

        const offset = @as(u64, @bitCast(-@as(i32, @intCast(currentFn.return_value_registers)) - 1));
        _ = try movQW.write(out, .{ 12, offset });
        _ = try jmpr12.write(out, .{});
        return null;
    }

    fn yield(self: *X86_64jit, callback: *const fn (*anyopaque) void, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {}
    fn fnprologue(self: *X86_64jit, data: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {}
    fn fnepilogue(self: *X86_64jit, data: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {}
    fn fixjmp(self: *X86_64jit, jmpfix: jit.LabelFix, absolutreallocation: u64) void {}
};

fn create_x86_64_arch(allocator: std.mem.Allocator) jit.Arch {
    return jit.createJITFrom(X86_64jit, allocator.create(X86_64jit));
}