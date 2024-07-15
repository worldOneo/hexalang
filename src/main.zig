const std = @import("std");
const builtin = @import("builtin");

fn alloc(size: usize) ![]u8 {
    if (builtin.os.tag == .windows) {
        const buff = try std.os.windows.VirtualAlloc(
            null,
            size,
            std.os.windows.MEM_COMMIT,
            std.os.windows.PAGE_READWRITE,
        );

        var nothing: u32 = 0;
        try std.os.windows.VirtualProtect(
            buff,
            size,
            std.os.windows.PAGE_EXECUTE_READWRITE,
            &nothing,
        );
        var ptr: [*]u8 = @ptrCast(buff);
        return ptr[0..size];
    } else if (builtin.os.tag == .linux) {
        const buff = std.os.linux.mmap(
            null,
            size,
            std.os.linux.PROT.EXEC | std.os.linux.PROT.READ | std.os.linux.PROT.WRITE,
            std.os.linux.MAP.ANONYMOUS | std.os.linux.MAP.PRIVATE,
            -1,
            0,
        );
        if (buff == 0) {
            return error.AllocFailed;
        }
        return @as([*]u8, @ptrFromInt(buff))[0..size];
    }
}

const IRFn = struct {
    name: u64,
    registers: u64,
    max_call_registers: u64,
    return_registers: u64,
    body: []IR,
};

const IR = union(enum) {
    Set: struct {
        dest: u64,
        value: u64,
    },
    Copy: struct {
        from: u64,
        to: u64,
    },
    Add: struct {
        dest: u64,
        left: u64,
        right: u64,
    },
    Sub: struct {
        dest: u64,
        left: u64,
        right: u64,
    },
    Mul: struct {
        dest: u64,
        left: u64,
        right: u64,
    },
    Jmp: struct {
        label: u64,
    },
    JmpIf: struct {
        label: u64,
        condition: u64,
    },
    Label: struct {
        name: u64,
    },
    CallFn: struct {
        args: []const u64,
        label: u64,
        current_fn_registers: u64,
        return_registers: []const u64,
    },
    Return: struct {
        registers: []const u64,
    },
    Yield,
};

fn start_function(stack: []u8, machinecode: *const fn () void) void {
    const len = stack.len - 24;
    const ptr = stack.ptr;
    asm volatile (
        \\ movq %rbp, %r11
        \\ movq %rsp, %r12
        \\ movq %rax, %rbp
        \\ movq %rax, %rsp
        \\ leaq .start_function_finished, %r15
        \\ addq $8, %rsp
        \\ movq %r15, (%rsp)
        \\ jmpq *%rcx
        \\.start_function_finished:
        \\ movq %r11, %rbp
        \\ movq %r12, %rsp
        :
        : [len] "{r13}" (len),
          [ptr] "{rax}" (ptr),
          [machinecode] "{rcx}" (machinecode),
        : "memory"
    );
}

const Compiler = struct {
    const JmpFix = struct {
        label: u64,
        fixat: u64,

        fn fixjmp(jmpfix: *const Compiler.JmpFix, location: u64, code: *std.ArrayList(u8)) void {
            code.items[jmpfix.fixat + 0] = 0x49; // REX.WB
            code.items[jmpfix.fixat + 1] = 0xbf; // MOVABS r15
            const num = @as(*const [8]u8, @ptrCast(&location)).*;
            code.items[jmpfix.fixat + 2] = num[0];
            code.items[jmpfix.fixat + 3] = num[1];
            code.items[jmpfix.fixat + 4] = num[2];
            code.items[jmpfix.fixat + 5] = num[3];
            code.items[jmpfix.fixat + 6] = num[4];
            code.items[jmpfix.fixat + 7] = num[5];
            code.items[jmpfix.fixat + 8] = num[6];
            code.items[jmpfix.fixat + 9] = num[7];
        }
    };

    fn defaultyield(p: *anyopaque) void {
        const r14 = asm volatile (""
            : [ret] "={r14}" (-> usize),
        );
        const r15 = asm volatile (""
            : [ret] "={r14}" (-> usize),
        );
        const rdx = asm volatile ("andq %rax, %rax"
            : [ret] "={rdx}" (-> usize),
        );
        const rbx = asm volatile ("andq %rax, %rax"
            : [ret] "={rbx}" (-> usize),
        );
        std.debug.print("Yielded: {?}, rdx={?}, rbx={?}\n", .{ p, rdx, rbx });
        asm volatile ("andq %rax, %rax"
            :
            : [r14] "{r14}" (r14),
              [r15] "{r15}" (r15),
        );
    }

    ir: *std.ArrayList(IRFn),
    machinecode: std.ArrayList(u8),
    jmpfixes: std.ArrayList(JmpFix),
    jmplabelmap: std.AutoHashMap(u64, u64),
    yieldfn: *const fn (*anyopaque) void = defaultyield,
    yieldarg: *anyopaque = @constCast(&defaultyield),

    fn new(allocator: std.mem.Allocator, ir: *std.ArrayList(IRFn)) Compiler {
        return Compiler{
            .ir = ir,
            .machinecode = std.ArrayList(u8).init(allocator),
            .jmpfixes = std.ArrayList(JmpFix).init(allocator),
            .jmplabelmap = std.AutoHashMap(u64, u64).init(allocator),
        };
    }

    fn compile(compiler: *Compiler) !void {
        for (compiler.ir.items) |ir| {
            switch (ir) {
                IR.Label => |label| {
                    try compiler.jmplabelmap.put(label.name, compiler.machinecode.items.len);
                },
                else => {
                    const fix = try compiler.comp(ir, &compiler.machinecode);
                    if (fix) |fixat| {
                        try compiler.jmpfixes.append(fixat);
                    }
                },
            }
        }
    }

    fn push(register: u8, out: *std.ArrayList(u8)) !void {
        try out.appendSlice(&[_]u8{
            0x48, 0x83, 0xc4, 0x08, // add rsp, 8
        });
        var b = X64InstructionBuilder.new(MOV).r64bit().reg(register).rm(RSP).rmmode(X64InstructionBuilder.RMMode.SIB).index(RSP).base(RSP);
        if (register > 7) {
            b = b.extreg();
        }
        try b.encode_mr(out);
    }

    fn pop(register: u8, out: *std.ArrayList(u8)) !void {
        var b = X64InstructionBuilder.new(MOV).r64bit().reg(register).rm(RSP).rmmode(X64InstructionBuilder.RMMode.SIB).index(RSP).base(RSP);
        if (register > 7) {
            b = b.extreg();
        }
        try b.encode_rm(out);
        try out.appendSlice(&[_]u8{
            0x48, 0x83, 0xec, 0x08, // sub rsp, 8
        });
    }

    fn call(register: u8, out: *std.ArrayList(u8)) !void {
        if (register == R14) {
            try out.appendSlice(&[_]u8{
                0x4c, 0x8d, 0x35, 0x0b, 0x00, 0x00, 0x00, // lea r15, rip
            });
            try push(R15, out);
        } else {
            try out.appendSlice(&[_]u8{
                0x4c, 0x8d, 0x35, 0x0b, 0x00, 0x00, 0x00, // lea r14, rip
            });
            try push(R14, out);
        }
        if (register > 7) {
            try out.append(0x41); // REX.B
        }
        try out.appendSlice(&[_]u8{
            0xff, (0b11100000 | (register & 0b111)), // jmp register
        });
    }

    fn append_return(out: *std.ArrayList(u8)) !void {
        try pop(R14, out);
        try out.appendSlice(&[_]u8{
            0x41, 0xff, 0xe6, // jmp r14
        });
    }

    fn compfn(compiler: *Compiler, func: *const IRFn, out: *std.ArrayList(u8)) !void {
        try compiler.jmplabelmap.put(func.name, out.items.len);
        try push(RBP, out);

        try out.appendSlice(&[_]u8{
            0x48, 0x89, 0xe5, // mov rbp, rsp
        });
        const realRegs = func.return_registers + if (func.registers > MaxCPUReg + 2) (func.registers - MaxCPUReg + 2) else 0;
        if (realRegs > 0) {
            try X64InstructionBuilder.new(SUB_ADD_IMM32).r64bit().reg(0).rm(RSP).disp32(@intCast(realRegs * 8)).encode_rm(out);
        }

        for (func.body) |ir| {
            switch (ir) {
                IR.Return => |cvalue| {
                    for (cvalue.registers, 0..) |reg, i| {
                        const now = try get_or_load_register14(reg, out);
                        try X64InstructionBuilder.new(MOV)
                            .extreg()
                            .r64bit()
                            .rmmode(X64InstructionBuilder.RMMode.SIB_DISP32)
                            .reg(now)
                            .rm(RBP)
                            .disp32(-u64_to_i32_offset((func.return_registers - i) * 8))
                            .encode_mr(out);
                    }
                    try pop(R14, out);
                    try out.appendSlice(&[_]u8{ 0x41, 0xff, 0xe6 }); // jmp r14
                },
                else => try compiler.comp(ir, out),
            }
        }
    }

    fn comp(compiler: *Compiler, ir: IR, out: *std.ArrayList(u8)) !?Compiler.JmpFix {
        // const MOVQ_HALF: u8 = 0b10111000;

        switch (ir) {
            IR.Copy => |cvalue| {
                const value = cvalue;
                try X64InstructionBuilder.new(MOV).reg(value.to).rm(value.from).encode_virtual(value.to, out);
            },
            IR.Jmp => |cvalue| {
                const value = cvalue;
                const offset = out.items.len;

                try out.appendSlice(&[_]u8{
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // movabs r15, smthsmthsmth
                    0x41, 0xff, 0xe7, // jmp r15
                });

                return Compiler.JmpFix{
                    .fixat = offset,
                    .label = value.label,
                };
            },
            IR.Add => |cvalue| {
                const value = cvalue;
                try X64InstructionBuilder.new(ADD).reg(value.left).rm(value.right).encode_virtual(value.dest, out);
            },
            IR.Sub => |cvalue| {
                const value = cvalue;
                try X64InstructionBuilder.new(SUB).reg(value.left).rm(value.right).encode_virtual(value.dest, out);
            },
            IR.Mul => |cvalue| {
                const value = cvalue;
                try X64InstructionBuilder.new(MUL).reg(value.left).rm(value.right).encode_virtual(value.dest, out);
            },
            IR.FnEpilogue => |cvalue| {
                const value = cvalue;
                if (value.registers > MaxCPUReg + 2) {
                    try X64InstructionBuilder.new(SUB_ADD_IMM32).r64bit().reg(5).rm(RSP).disp32(@intCast((value.registers - MaxCPUReg + 2) * 8)).encode_rm(out);
                }
                try out.appendSlice(&[_]u8{
                    0x48, 0x89, 0xec, // mov rsp, rbp
                });
                try pop(RBP, out);
            },
            IR.Set => |cvalue| {
                try out.appendSlice(&[_]u8{
                    X64InstructionBuilder.REX_BYTE | @intFromEnum(X64InstructionBuilder.Flag.B) | @intFromEnum(X64InstructionBuilder.Flag.W), // REX.WB
                    0b10111000 | (0b110), // movabs r14,
                });
                const slice: *const [8]u8 = @ptrCast(&cvalue.value);
                try out.appendSlice(slice);
                try restore_if_needed(cvalue.dest, R14, out);
            },
            IR.Yield => {
                try push(RAX, out);
                try push(RCX, out);
                try out.appendSlice(&[_]u8{
                    X64InstructionBuilder.REX_BYTE | @intFromEnum(X64InstructionBuilder.Flag.W), // REX.W
                    0b10111000 | (RAX), // movabs rax,
                });
                var slice: *const [8]u8 = @ptrCast(&compiler.yieldfn);
                try out.appendSlice(slice);
                try out.appendSlice(&[_]u8{
                    X64InstructionBuilder.REX_BYTE | @intFromEnum(X64InstructionBuilder.Flag.W), // REX.W
                    0b10111000 | (RCX), // movabs rcx,
                });
                slice = @ptrCast(&compiler.yieldarg);
                try out.appendSlice(slice);

                try out.appendSlice(&[_]u8{
                    0x49, 0x89, 0xEE, // mov r14, rbp
                    0x49, 0x89, 0xE7, // mov r15, rsp
                    0x4C, 0x89, 0xDD, // mov rbp, r11
                    0x4C, 0x89, 0xE4, // mov rsp, r12
                    0xFF, 0xD0, // call rax
                    0x4C, 0x89, 0xF5, // mov rbp, r14
                    0x4C, 0x89, 0xFC, // mov rsp, r15
                });

                try pop(RCX, out);
                try pop(RAX, out);
            },
            IR.JmpIf => |cvalue| {
                try X64InstructionBuilder.new(TEST).reg(cvalue.condition).rm(cvalue.condition).encode_virtual_nodest(out);
                const offset = out.items.len;
                try out.appendSlice(&[_]u8{
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // movabs r15, smthsmthsmth
                });
                try out.appendSlice(&[_]u8{
                    0x4c, 0x8d, 0x35, 0x07, 0x00, 0x00, 0x00, // lea r14, [rip+7]
                });
                try out.appendSlice(&[_]u8{
                    0x4d, 0x0f, 0x45, 0xf7, // cmovne r14, r15
                    0x41, 0xff, 0xe6, // jmp r14
                });
                return Compiler.JmpFix{ .fixat = offset, .label = cvalue.label };
            },
            IR.CallFn => |cvalue| {
                for (0..cvalue.current_fn_registers) |reg| {
                    const rreg = to_real_reg(reg);
                    if (rreg > MaxCPUReg) {
                        break;
                    }
                    try push(@intCast(rreg), out);
                }

                // stack is now:
                // [stack reg 1]
                // [stack reg 2]
                // ...
                // [stack reg N]
                // [cpu reg 0]
                // ...
                // [cpu reg N]

                // Increase stack to fit return registers
                try X64InstructionBuilder.new(SUB_ADD_IMM32)
                    .r64bit()
                    .reg(5) // 5 = add... somehow
                    .rm(RSP)
                    .disp32(@intCast(cvalue.return_registers * 8))
                    .encode_rm(out);

                // stack is now:
                // [stack reg 1]
                // [stack reg 2]
                // ...
                // [stack reg N]
                // [cpu reg 0]
                // ...
                // [cpu reg N]
                // [return reg 0]
                // ...
                // [return reg N]

                // Insert args below return registers
                for (cvalue.args, 0..) |arg, i| {
                    const rreg = to_real_reg(arg);
                    const destreg = to_real_reg(i);

                    // read reg from old fn frame
                    if (rreg > MaxCPUReg) {
                        try X64InstructionBuilder.new(MOV).r64bit().extreg()
                            .reg(R14).rm(RBP).rmmode(X64InstructionBuilder.RMMode.SIB_DISP32)
                            .disp32(u64_to_i32_offset((cvalue.return_registers + rreg - MaxCPUReg - 1) * 8)).encode_rm(out);
                    } else {
                        try X64InstructionBuilder.new(MOV).r64bit().extreg()
                            .reg(R14).rm(RBP).rmmode(X64InstructionBuilder.RMMode.SIB_DISP32)
                            .disp32(u64_to_i32_offset((cvalue.return_registers + cvalue.current_fn_registers - UsableCPURegs + 1 + arg) * 8)).encode_rm(out); // read pushed var
                    }

                    // insert into new fn frame
                    if (destreg > MaxCPUReg) {
                        try X64InstructionBuilder.new(MOV).r64bit().extreg()
                            .reg(R14).rm(RBP).rmmode(X64InstructionBuilder.RMMode.SIB_DISP32)
                            .disp32(u64_to_i32_offset((cvalue.return_registers + cvalue.current_fn_registers + 1 + i) * 8)).encode_mr(out); // move to new stack
                    } else {
                        var builder = X64InstructionBuilder.new(MOV).r64bit().extRM()
                            .reg(destreg).rm(R14).rmmode(X64InstructionBuilder.RMMode.RM); // mov to new register
                        if (destreg > 7) {
                            builder = builder.extreg();
                        }
                        try builder.encode_rm(out);
                    }
                }
                const offset = out.items.len;
                try out.appendSlice(&[_]u8{
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // movabs r15, smthsmthsmth
                });
                try call(R15, out);
                // TODO: Write return value into current registers

                // restore CPU registers
                for (0..cvalue.current_fn_registers) |regnum| {
                    if (regnum >= UsableCPURegs) {
                        break;
                    }
                    const reg = @min(UsableCPURegs, cvalue.current_fn_registers) - regnum - 1;
                    try pop(@intCast(to_real_reg(reg)), out);
                }
                return JmpFix{ .label = cvalue.label, .fixat = offset };
            },
            else => @panic("Not yet supported"),
        }
        return null;
    }

    fn machinecodesize(compiler: *Compiler) u64 {
        return compiler.machinecode.items.len;
    }

    fn fixToBaseLocation(compiler: *Compiler, base: u64) void {
        for (compiler.jmpfixes.items) |fix| {
            fix.fixjmp(base + (compiler.jmplabelmap.get(fix.label) orelse unreachable), &compiler.machinecode);
        }
    }
};

// Who thought RAX, RCX, RDX, RBX is the right order?
// It should be RAX, RBX, RCX, RDX ofc...
const X86_64_REG_REMAPPING = [_]u8{ 0, 3, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };

const RAX: u8 = 0b000;
const RCX: u8 = 0b001;
const RSP: u8 = 0b100;
const RBP: u8 = 0b101;
const R13: u8 = 0b1101;
const R14: u8 = 0b1110;
const R15: u8 = 0b1111;

const MaxCPUReg = 10;
const UsableCPURegs = 9;
const MOV_MIMM8: u8 = 0xC6;
const MOV_RR_MR: u8 = 0x89;
const MOV_RR_RM: u8 = 0x8B;
const ADD_MR: u8 = 0x01;
const ADD_RM: u8 = 0x03;
const SUB_RM: u8 = 0x2b;
const SUB_MR: u8 = 0x29;

const TEST = X64Instruction{
    .opcode_rm = &[_]u8{0x85},
    .rex64bit = true,
};

const MOV = X64Instruction{
    .opcode_rm = &[_]u8{MOV_RR_RM},
    .opcode_mr = &[_]u8{MOV_RR_MR},
    .rex64bit = true,
};

const ADD = X64Instruction{
    .opcode_rm = &[_]u8{ADD_RM},
    .opcode_mr = &[_]u8{ADD_MR},
    .rex64bit = true,
};

const SUB = X64Instruction{
    .opcode_rm = &[_]u8{SUB_RM},
    .opcode_mr = &[_]u8{SUB_MR},
    .rex64bit = true,
};

const SUB_ADD_IMM32 = X64Instruction{
    .opcode_rm = &[_]u8{0x81},
    .opcode_mr = null,
    .rex64bit = true,
};

const MUL = X64Instruction{
    .opcode_rm = &[_]u8{ 0x0f, 0xaf },
    .opcode_mr = null,
    .rex64bit = true,
};

fn u64_to_i32_offset(int: u64) i32 {
    return @as(i32, @bitCast(@as(u32, @intCast(int))));
}

fn to_real_reg(r: u64) u64 {
    if (r > 3) {
        return r + 2;
    }
    return r;
}

fn copy_virtual_register_to_cpu_register(from: u64, to: u8, out: *std.ArrayList(u8)) !void {
    const reg = to_real_reg(from);

    if (reg > MaxCPUReg) {
        var b = X64InstructionBuilder.new(MOV)
            .extreg()
            .r64bit()
            .rmmode(X64InstructionBuilder.RMMode.SIB_DISP32)
            .reg(to)
            .rm(RBP)
            .disp32(u64_to_i32_offset(reg - MaxCPUReg - 1) * 8);
        if (to > 7) {
            b = b.extreg();
        }
        try b.encode_rm(out);
    } else {
        var b = X64InstructionBuilder.new(MOV)
            .extreg()
            .r64bit()
            .rmmode(X64InstructionBuilder.RMMode.RM)
            .reg(to)
            .rm(reg);
        if (to > 7) {
            b = b.extreg();
        }
        if (reg > 7) {
            b = b.extRM();
        }
        try b.encode_rm(out);
    }
}

// TODO: Check if R12 must be skipped....

fn copy_cpu_register_to_virtual_register(to: u64, from: u8, out: *std.ArrayList(u8)) !void {
    const reg = to_real_reg(to);

    if (reg > MaxCPUReg) {
        var b = X64InstructionBuilder.new(MOV)
            .extreg()
            .r64bit()
            .rmmode(X64InstructionBuilder.RMMode.SIB_DISP32)
            .reg(from)
            .rm(RBP)
            .disp32(u64_to_i32_offset(reg - MaxCPUReg - 1) * 8);
        if (from > 7) {
            b = b.extreg();
        }
        try b.encode_mr(out);
    } else {
        var b = X64InstructionBuilder.new(MOV)
            .extreg()
            .r64bit()
            .rmmode(X64InstructionBuilder.RMMode.RM)
            .reg(from)
            .rm(reg);
        if (from > 7) {
            b = b.extreg();
        }
        if (reg > 7) {
            b = b.extRM();
        }
        try b.encode_mr(out);
    }
}

fn get_or_load_register14(reg: u64, out: *std.ArrayList(u8)) !u8 {
    return get_or_load_to_register(reg, R14, out);
}

fn get_or_load_register15(reg: u64, out: *std.ArrayList(u8)) !u8 {
    return get_or_load_to_register(reg, R15, out);
}

fn get_or_load_to_register(reg: u64, realreg: u8, out: *std.ArrayList(u8)) !u8 {
    const rreg = to_real_reg(reg);

    if (rreg > MaxCPUReg) {
        _ = try X64InstructionBuilder.new(MOV)
            .extreg()
            .r64bit()
            .rmmode(X64InstructionBuilder.RMMode.SIB_DISP32)
            .reg(realreg)
            .rm(RBP)
            .disp32(u64_to_i32_offset(rreg - MaxCPUReg - 1) * 8)
            .encode_rm(out);
        return realreg;
    }
    return @intCast(rreg);
}

fn restore_if_needed(corgreg: u64, reg: u8, out: *std.ArrayList(u8)) !void {
    const orgreg = to_real_reg(corgreg);

    if (orgreg > MaxCPUReg) {
        _ = try X64InstructionBuilder.new(MOV)
            .extreg()
            .r64bit()
            .rmmode(X64InstructionBuilder.RMMode.SIB_DISP32)
            .reg(reg)
            .rm(RBP)
            .disp32(u64_to_i32_offset((orgreg - MaxCPUReg - 1) * 8))
            .encode_mr(out);
    }
}

const X64Instruction = struct {
    opcode_rm: ?[]const u8 = null,
    opcode_mr: ?[]const u8 = null,
    rex64bit: bool,
};

const X64InstructionBuilder = struct {
    const RMMode = enum(u8) {
        SIB = 0b00000000,
        SIB_DISP8 = 0b01000000,
        SIB_DISP32 = 0b10000000,
        RM = 0b11000000,
    };
    const REX = struct {
        r64bit: bool = false,
        extReg: bool = false,
        extSib: bool = false,
        extRm: bool = false,
    };
    const SIB = struct {
        scale: Scale = Scale.TIMES1,
        base: ?u8 = null,
        index: ?u8 = null,
    };
    const Scale = enum(u8) {
        TIMES1 = 0b00000000,
        TIMES2 = 0b01000000,
        TIMES4 = 0b10000000,
        TIMES8 = 0b11000000,
    };
    const REX_BYTE: u8 = 0b01000000;
    const Flag = enum(u8) {
        /// 64bit operand size
        W = 0b00001000,
        /// Extended MODRM.reg
        R = 0b00000100,
        /// Extend SIB.index
        X = 0b00000010,
        /// Extended MODRM.rm
        B = 0b00000001,
    };
    const ModRM = struct {
        mode: RMMode = RMMode.RM,
        modreg: ?u64 = null,
        modrm: ?u64 = null,
    };
    instruction: X64Instruction,
    _modrm: ?ModRM = null,
    _rex: ?REX = null,
    _sib: ?SIB = null,
    _disp32: ?i32 = null,

    fn new(instruction: X64Instruction) X64InstructionBuilder {
        return .{ .instruction = instruction };
    }

    fn modrm(old: *const X64InstructionBuilder) X64InstructionBuilder {
        var v = old.*;
        if (v._modrm) |_| {
            return v;
        }
        v._modrm = .{};
        return v;
    }

    fn rmmode(old: *const X64InstructionBuilder, mode: RMMode) X64InstructionBuilder {
        var v = old.modrm();
        if (v._modrm) |*_m| {
            _m.mode = mode;
        }
        return v;
    }

    fn rex(old: *const X64InstructionBuilder) X64InstructionBuilder {
        var v = old.*;
        if (v._rex) |_| {
            return v;
        }
        v._rex = .{};
        return v;
    }

    fn r64bit(old: *const X64InstructionBuilder) X64InstructionBuilder {
        var v = old.rex();
        if (v._rex) |*r| {
            r.r64bit = true;
        }
        return v;
    }

    fn extreg(old: *const X64InstructionBuilder) X64InstructionBuilder {
        var v = old.rex();
        if (v._rex) |*r| {
            r.extReg = true;
        }
        return v;
    }

    fn extSIB(old: *const X64InstructionBuilder) X64InstructionBuilder {
        var v = old.rex();
        if (v._rex) |*r| {
            r.extSib = true;
        }
        return v;
    }

    fn extRM(old: *const X64InstructionBuilder) X64InstructionBuilder {
        var v = old.rex();
        if (v._rex) |*r| {
            r.extRm = true;
        }
        return v;
    }

    fn sib(old: *const X64InstructionBuilder) X64InstructionBuilder {
        var v = old.*;
        if (v._sib) |_| {
            return v;
        }
        v._sib = .{};
        return v;
    }

    fn index(old: *const X64InstructionBuilder, idx: u8) X64InstructionBuilder {
        var v = old.sib();
        if (v._sib) |*s| {
            s.index = idx;
        }
        return v;
    }

    fn scale(old: *const X64InstructionBuilder, sc: Scale) X64InstructionBuilder {
        var v = old.sib();
        if (v._sib) |*s| {
            s.scale = sc;
        }
        return v;
    }

    fn base(old: *const X64InstructionBuilder, b: u8) X64InstructionBuilder {
        var v = old.sib();
        if (v._sib) |*s| {
            s.base = b;
        }
        return v;
    }

    fn reg(old: *const X64InstructionBuilder, r: u64) X64InstructionBuilder {
        var v = old.modrm();
        if (v._modrm) |*m| {
            m.modreg = r;
        }
        return v;
    }

    fn rm(old: *const X64InstructionBuilder, r: u64) X64InstructionBuilder {
        var v = old.modrm();
        if (v._modrm) |*m| {
            m.modrm = r;
        }
        return v;
    }

    fn disp32(old: *const X64InstructionBuilder, offset: i32) X64InstructionBuilder {
        var v = old.*;
        v._disp32 = offset;
        return v;
    }

    fn encode_virtual_nodest(cb: *const X64InstructionBuilder, out: *std.ArrayList(u8)) !void {
        var b = cb.extreg().extRM();
        if (b.instruction.rex64bit) {
            b = b.r64bit();
        }
        if (b._modrm) |m| {
            try copy_virtual_register_to_cpu_register(m.modreg orelse 0, R14, out);
            if (m.modreg == m.modrm) {
                try b.reg(R14).rm(R14).encode_rm(out);
                return;
            }
            try copy_virtual_register_to_cpu_register(m.modrm orelse 0, R15, out);
            try b.reg(R14).rm(R15).encode_rm(out);
        }
    }

    fn encode_virtual(cb: *const X64InstructionBuilder, destination: u64, out: *std.ArrayList(u8)) !void {
        var b = cb.*;
        if (b.instruction.rex64bit) {
            b = b.r64bit();
        }
        if (b._modrm) |m| {
            const rreg = to_real_reg(m.modreg orelse 0);

            if (rreg > 7) {
                b = b.extreg();
            }
            const rrm = to_real_reg(m.modrm orelse 0);
            if (rrm > 7) {
                b = b.extRM();
            }
            const rdest = to_real_reg(destination);

            if (rreg <= MaxCPUReg and rrm <= MaxCPUReg) {
                if (rdest == rreg) {
                    try b.reg(rreg).rm(rrm).encode_mr(out);
                } else {
                    try copy_virtual_register_to_cpu_register(m.modreg orelse 0, R14, out);
                    try b.reg(R14).rm(rrm).encode_rm(out);
                }
            } else if (rreg <= MaxCPUReg) {
                try copy_virtual_register_to_cpu_register(m.modrm orelse 0, R15, out);
                if (rdest == rreg) {
                    try b.reg(rreg).rm(R15).encode_mr(out);
                } else {
                    try copy_virtual_register_to_cpu_register(m.modreg orelse 0, R14, out);
                    try b.reg(R14).rm(R15).encode_rm(out);
                    try copy_cpu_register_to_virtual_register(destination, R14, out);
                }
            } else if (rrm <= MaxCPUReg) {
                try copy_virtual_register_to_cpu_register(m.modreg orelse 0, R14, out);
                try b.reg(R14).rm(rrm).encode_rm(out);
                try copy_cpu_register_to_virtual_register(destination, R14, out);
            } else {
                try copy_virtual_register_to_cpu_register(m.modreg orelse 0, R14, out);
                try copy_virtual_register_to_cpu_register(m.modrm orelse 0, R15, out);
                try b.reg(R14).rm(R15).encode_rm(out);
                try copy_cpu_register_to_virtual_register(destination, R14, out);
            }
        }
    }

    fn encode_rm(b: *const X64InstructionBuilder, out: *std.ArrayList(u8)) !void {
        try b.encode_real_rex(out);
        try out.appendSlice(b.instruction.opcode_rm orelse &[_]u8{0});
        try b.encode_real_mod_sib_disp(out);
    }

    fn encode_mr(b: *const X64InstructionBuilder, out: *std.ArrayList(u8)) !void {
        try b.encode_real_rex(out);
        try out.appendSlice(b.instruction.opcode_mr orelse &[_]u8{0});
        try b.encode_real_mod_sib_disp(out);
    }

    fn encode_real_rex(b: *const X64InstructionBuilder, out: *std.ArrayList(u8)) !void {
        if (b._rex) |r| {
            var byte: u8 = REX_BYTE;
            if (r.r64bit) {
                byte |= @intFromEnum(Flag.W);
            }
            if (r.extSib) {
                byte |= @intFromEnum(Flag.X);
            }
            if (r.extReg) {
                byte |= @intFromEnum(Flag.R);
            }
            if (r.extRm) {
                byte |= @intFromEnum(Flag.B);
            }
            try out.append(byte);
        }
    }

    fn encode_real_mod_sib_disp(b: *const X64InstructionBuilder, out: *std.ArrayList(u8)) !void {
        if (b._modrm) |m| {
            var byte: u8 = @intFromEnum(m.mode);
            byte |= @intCast((((m.modreg orelse 0) & 0b111) << 3));
            byte |= @intCast((m.modrm orelse 0) & 0b111);
            try out.append(byte);
        }

        if (b._sib) |s| {
            var byte: u8 = @intFromEnum(s.scale);
            byte |= @intCast(((s.index orelse 0) & 0b111) << 3);
            byte |= @intCast(((s.base orelse 0) & 0b111));
            try out.append(byte);
        }

        if (b._disp32) |*d| {
            const data: *const [4]u8 = @ptrCast(d);
            try out.appendSlice(data);
        }
    }
};

fn compile(allocator: std.mem.Allocator, relative_offset: u64) !std.ArrayList(u8) {
    _ = relative_offset;
    const list = try std.ArrayList(u8).init(allocator);
    _ = list;
}

pub fn main() !void {
    const code = [_]u8{
        0x89, 0xC8, // mov eax,ecx
        0x83, 0xC0, 0x05, // add eax, 5
        0xC3, // ret
    };
    _ = code;
    const buff = try alloc(4096);
    var ir: std.ArrayList(IR) = std.ArrayList(IR).init(std.heap.page_allocator);
    try ir.append(.{ .Label = .{ .name = 1 } });
    try ir.append(.{ .FnPrologue = .{ .registers = 123, .max_call_regs = 4 } });
    try ir.append(.{ .Set = .{ .dest = 21, .value = 123 } });
    try ir.append(.{ .Set = .{ .dest = 22, .value = 456 } });
    try ir.append(.{ .Set = .{ .dest = 23, .value = 0 } });
    try ir.append(.{ .JmpIf = .{ .label = 2, .condition = 23 } });
    try ir.append(.Yield);
    try ir.append(.{ .Label = .{ .name = 2 } });
    try ir.append(.Yield);
    try ir.append(.{ .Add = .{ .left = 21, .right = 22, .dest = 0 } });
    try ir.append(.{ .CallFn = .{ .label = 3, .current_fn_registers = 123, .args = &[_]u64{ 0, 0, 0, 22 } } });
    try ir.append(.Yield);
    try ir.append(.{ .FnEpilogue = .{ .registers = 123 } });
    try ir.append(.Yield);
    try ir.append(.Return);
    try ir.append(.{ .Label = .{ .name = 3 } });
    try ir.append(.Yield);
    try ir.append(.Return);

    var compiler = Compiler.new(std.heap.page_allocator, &ir);
    try compiler.compile();
    compiler.fixToBaseLocation(@intFromPtr(buff.ptr));

    @memset(buff, 0);
    std.mem.copy(u8, buff, compiler.machinecode.items);
    // for (compiler.machinecode.items) |item| {
    //     std.debug.print("{x:0>2} ", .{item});
    // }
    const f: *fn () void = @ptrCast(buff);
    const stack = try alloc(1 << 10);
    std.debug.print("\nStack: {*}\n", .{stack.ptr});
    start_function(stack, f);
    std.debug.print("Worked\n", .{});
}
