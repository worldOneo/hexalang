const std = @import("std");
const builtin = @import("builtin");

fn alloc(size: usize) ![]u8 {
    if (builtin.os.tag == .windows) {
        var buff = try std.os.windows.VirtualAlloc(
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
        var buff = std.os.linux.mmap(
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

const IR = union(enum) {
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
    Label: struct {
        name: u64,
    },
    FnEpilogue: struct {
        registers: u64,
    },
    FnPrologue: struct {
        registers: u64,
    },
    Return,
};

// Who thought RAX, RCX, RDX, RBX is the right order?
// It should be RAX, RBX, RCX, RDX ofc...
const X86_64_REG_REMAPPING = [_]u8{ 0, 3, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };

const ModRM = struct {
    const Mode = enum(u8) {
        SIB = 0b00000000,
        SIB_DISP8 = 0b01000000,
        SIB_DISP32 = 0b10000000,
        RM = 0b11000000,
    };

    data: u8,

    fn new() ModRM {
        return .{ .data = 0 };
    }

    fn mode(modrm: *const ModRM, m: Mode) ModRM {
        return .{ .data = (modrm.data & 0b00111111) | @intFromEnum(m) };
    }

    fn reg(modrm: *const ModRM, r: u8) ModRM {
        return .{ .data = (modrm.data & 0b11000111) | ((X86_64_REG_REMAPPING[r] & 0b111) << 3) };
    }

    fn rm(modrm: *const ModRM, r: u8) ModRM {
        return .{ .data = (modrm.data & 0b11111000) | (X86_64_REG_REMAPPING[r] & 0b111) };
    }

    fn byte(modrm: *const ModRM) u8 {
        return modrm.data;
    }
};

fn encodeModRM(reg: u8, rm: u8) u8 {
    return ModRM.new()
        .mode(ModRM.Mode.RM)
        .reg(reg)
        .rm(rm)
        .byte();
}

const SIB = struct {
    const Scale = enum(u8) {
        TIMES1 = 0b00000000,
        TIMES2 = 0b01000000,
        TIMES4 = 0b10000000,
        TIMES8 = 0b11000000,
    };

    data: u8,

    fn new() SIB {
        return .{ .data = 0 };
    }
    fn nothing() SIB {
        return .{ .data = 0b00100100 }; // rsp + rsp*1 = nothing apparently
    }

    fn scale(sib: *const SIB, s: Scale) SIB {
        return .{ .data = (sib.data & 0b00111111) | @intFromEnum(s) };
    }

    fn index(sib: *const SIB, r: u8) SIB {
        return .{ .data = (sib.data & 0b11000111) | ((X86_64_REG_REMAPPING[r] & 0b111) << 3) };
    }

    fn base(sib: *const SIB, r: u8) SIB {
        return .{ .data = (sib.data & 0b11111000) | (X86_64_REG_REMAPPING[r]) };
    }

    fn byte(sib: *const SIB) u8 {
        return sib.data;
    }
};

const REX = struct {
    const Flag = enum(u8) {
        W = 0b00001000, // 64bit operand size
        R = 0b00000100, // Extended MODRM.reg
        X = 0b00000010, // Extend SIB.index
        B = 0b00000001, // Extended MODRM.rm
    };
    const PREF_64B: u8 = 0b01000000; // REX prefix

    data: u8,

    fn new() REX {
        return .{ .data = PREF_64B };
    }

    fn op64bit(r: *const REX) REX {
        return .{ .data = r.data | @intFromEnum(Flag.W) };
    }

    fn extReg(r: *const REX) REX {
        return .{ .data = r.data | @intFromEnum(Flag.R) };
    }

    fn extSIBIndex(r: *const REX) REX {
        return .{ .data = r.data | @intFromEnum(Flag.X) };
    }

    fn extRMandSIBBase(r: *REX) REX {
        return .{ .data = r.data | @intFromEnum(Flag.B) };
    }

    fn byte(r: *const REX) u8 {
        return r.data;
    }
};

fn X64InstructionBuilder(comptime len: comptime_int) type {
    return struct {
        const Bundle = struct {
            rex: ?u8 = null,
            inst: ?[len]u8 = null,
            modrm: ?u8 = null,
            sib: ?u8 = null,
        };
        const Rexer = struct {
            fn rex(_: *const Rexer, r: REX) Inster {
                return .{ .bundle = .{ .rex = r.byte() } };
            }

            fn norex(_: *const Rexer) Inster {
                return .{};
            }
        };

        const Inster = struct {
            bundle: Bundle = .{},

            fn inst(inster: *const Inster, i: [len]u8) Modrmer {
                var m: Modrmer = .{ .bundle = inster.bundle };
                m.bundle.inst = i;
                return m;
            }
        };

        const Modrmer = struct {
            bundle: Bundle = .{},

            fn finish(mrm: *const Modrmer, out: *std.ArrayList(u8)) !u64 {
                var size: u64 = 0;
                if (mrm.bundle.rex) |rex| {
                    try out.append(rex);
                    size += 1;
                }
                if (mrm.bundle.inst) |inst| {
                    try out.appendSlice(&inst);
                    size += len;
                }
                return size;
            }

            fn modRM(mrm: *const Modrmer, rm: ModRM) Siber {
                var siber: Siber = .{ .bundle = mrm.bundle };
                siber.bundle.modrm = rm.byte();
                return siber;
            }
        };

        const Siber = struct {
            bundle: Bundle = .{},

            fn finish(siber: *const Siber, out: *std.ArrayList(u8)) !u64 {
                var size: u64 = 0;
                if (siber.bundle.rex) |rex| {
                    try out.append(rex);
                    size += 1;
                }
                if (siber.bundle.inst) |inst| {
                    try out.appendSlice(&inst);
                    size += len;
                }
                if (siber.bundle.modrm) |mrm| {
                    try out.append(mrm);
                    size += 1;
                }
                return size;
            }

            fn sib(siber: *const Siber, s: SIB) Disper {
                var disper: Disper = .{ .bundle = siber.bundle };
                disper.bundle.sib = s.byte();
                return disper;
            }

            fn nosib(siber: *const Siber) Disper {
                return .{ .bundle = siber.bundle };
            }
        };

        const Disper = struct {
            bundle: Bundle,

            fn finish(disper: *const Disper, out: *std.ArrayList(u8)) !u64 {
                var size: u64 = 0;
                if (disper.bundle.rex) |rex| {
                    try out.append(rex);
                    size += 1;
                }
                if (disper.bundle.inst) |inst| {
                    try out.appendSlice(&inst);
                    size += len;
                }
                if (disper.bundle.modrm) |mrm| {
                    try out.append(mrm);
                    size += 1;
                }
                if (disper.bundle.sib) |sib| {
                    try out.append(sib);
                    size += 1;
                }
                return size;
            }

            fn disp8(disper: *const Disper, disp: i8, out: *std.ArrayList(u8)) !u64 {
                var size: u64 = 0;
                if (disper.bundle.rex) |rex| {
                    try out.append(rex);
                    size += 1;
                }
                if (disper.bundle.inst) |inst| {
                    try out.appendSlice(&inst);
                    size += len;
                }
                if (disper.bundle.modrm) |mrm| {
                    try out.append(mrm);
                    size += 1;
                }
                if (disper.bundle.sib) |sib| {
                    try out.append(sib);
                    size += 1;
                }
                try out.append(@as(*u8, @ptrCast(&disp)).*);
                size += 1;
                return size;
            }

            fn disp32(disper: *const Disper, disp: i32, out: *std.ArrayList(u8)) !u64 {
                var size: u64 = 0;
                if (disper.bundle.rex) |rex| {
                    try out.append(rex);
                    size += 1;
                }
                if (disper.bundle.inst) |inst| {
                    try out.appendSlice(&inst);
                    size += len;
                }
                if (disper.bundle.modrm) |mrm| {
                    try out.append(mrm);
                    size += 1;
                }
                if (disper.bundle.sib) |sib| {
                    try out.append(sib);
                    size += 1;
                }
                try out.appendSlice(&(@as(*const [4]u8, @ptrCast(&disp)).*));
                size += 4;
                return size;
            }
        };

        fn new() Rexer {
            return .{};
        }
    };
}

const X64DestLeftRightBehaviour = enum {
    MovLeftApplyRight,
    ZeroDestApplyBoth,
    OneDestApplyBoth,
};

const X64LeftRightBehaviour = enum {
    ClearR15,
    MovLeft,
    Dirty,
};

fn X64IrToInstructionBuilder(comptime len: comptime_int) type {
    return struct {
        const RBP: u8 = 0b101;
        const R15: u8 = 0b1111;

        const MaxReg = 14;
        const MOV_MIMM8: u8 = 0xC6;
        const MOV_RR_MR: u8 = 0x89;
        const MOV_RR_RM: u8 = 0x8B;

        fn left_and_right(creg: u64, crm: u64, behaviour: X64LeftRightBehaviour, inst_rm: [len]u8, inst_mr: [len]u8, out: *std.ArrayList(u8)) !void {
            var reg = creg;
            var rm = crm;

            // Ignore RSP+RBP
            if (reg > 3) {
                reg += 2;
            }
            if (rm > 3) {
                rm += 2;
            }

            try left_and_right0(reg, rm, behaviour, inst_rm, inst_mr, out);
        }

        fn left_and_right0(creg: u64, crm: u64, behaviour: X64LeftRightBehaviour, inst_rm: [len]u8, inst_mr: [len]u8, out: *std.ArrayList(u8)) !void {
            var reg = creg;
            var rm = crm;
            // pick RegReg, RegMem, MemReg, or MemMem move
            if (reg <= MaxReg and rm <= MaxReg) {
                var rex = REX.new().op64bit();

                if (reg > 7) {
                    rex = rex.extReg();
                }
                if (rm > 7) {
                    rex = rex.extRMandSIBBase();
                }
                _ = try X64InstructionBuilder(1).new()
                    .rex(rex)
                    .inst(inst_rm)
                    .modRM(ModRM.new().reg(@intCast(reg)).rm(@intCast(rm)).mode(ModRM.Mode.RM)) // <reg> <- <reg>
                    .finish(out);
            } else if (reg > MaxReg and rm > MaxReg) {

                // ugly mem-mem case
                var offset_from = (reg - MaxReg) * 8;
                var offset_to = (rm - MaxReg) * 8;

                if (behaviour == .ClearR15) {
                    try out.appendSlice(&[_]u8{ 0x4D, 0x31, 0xFF }); // xor r15,r15
                } else if (behaviour == .MovLeft) {
                    // mov r15, [rsp - x]
                    _ = try X64InstructionBuilder(1).new()
                        .rex(REX.new().op64bit().extReg())
                        .inst(.{MOV_RR_RM})
                        .modRM(ModRM.new().reg(R15).rm(RBP).mode(ModRM.Mode.SIB_DISP32)) // rax <- rsp-offset
                        .nosib()
                        .disp32(-@as(i32, @intCast(offset_from)), out);
                } else if (behaviour == .Dirty) {
                    // inst r15, [rsp - x]
                    _ = try X64InstructionBuilder(len).new()
                        .rex(REX.new().op64bit().extReg())
                        .inst(inst_rm)
                        .modRM(ModRM.new().reg(R15).rm(RBP).mode(ModRM.Mode.SIB_DISP32)) // rax <- rsp-offset
                        .nosib()
                        .disp32(-@as(i32, @intCast(offset_from)), out);
                }

                // inst [rsp - y], r15
                _ = try X64InstructionBuilder(len).new()
                    .rex(REX.new().op64bit().extReg())
                    .inst(inst_mr)
                    .modRM(ModRM.new().reg(R15).rm(RBP).mode(ModRM.Mode.SIB_DISP32)) // rsp-offset <- rax
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset_to)), out);
            } else if (reg <= MaxReg) {
                var rex = REX.new().op64bit();
                if (reg > 7) {
                    rex = rex.extReg();
                }
                var offset_to = (rm - MaxReg) * 8;
                // inst [rsp - y], rax
                _ = try X64InstructionBuilder(1).new()
                    .rex(rex)
                    .inst(inst_rm)
                    .modRM(ModRM.new().reg(@intCast(reg)).rm(RBP).mode(ModRM.Mode.SIB_DISP32)) // rbp-offset <- <reg>
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset_to)), out);
            } else {
                var rex = REX.new().op64bit();
                if (rm > 7) {
                    rex = rex.extReg();
                }
                var offset_from = (reg - MaxReg) * 8;
                // inst rax, MOV [rsp - y]
                _ = try X64InstructionBuilder(len).new()
                    .rex(rex)
                    .inst(inst_mr)
                    .modRM(ModRM.new().reg(@intCast(rm)).rm(RBP).mode(ModRM.Mode.SIB_DISP32)) // <reg> <- rbp-offset
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset_from)), out);
            }
        }

        fn copy_to_r15(cfrom: u64, out: *std.ArrayList(u8)) !void {
            var from = cfrom;
            if (from > 3) {
                from += 2;
            }
            if (from <= MaxReg) {
                var rex = REX.new().op64bit().extReg();
                if (from > 7) {
                    rex = rex.extRMandSIBBase();
                }
                _ = try X64InstructionBuilder(1).new()
                    .rex(rex)
                    .inst(.{MOV_RR_RM})
                    .modRM(ModRM.new().reg(R15).rm(@intCast(from)).mode(ModRM.Mode.RM))
                    .nosib()
                    .finish(out);
            } else {
                var offset = (from - MaxReg) * 8;
                _ = try X64InstructionBuilder(1).new()
                    .rex(REX.new().op64bit().extReg())
                    .inst(.{MOV_RR_RM})
                    .modRM(ModRM.new().reg(R15).rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset)), out);
            }
        }

        fn copy_from_r15(cto: u64, out: *std.ArrayList(u8)) !void {
            var to = cto;
            if (to > 3) {
                to += 2;
            }
            if (to <= MaxReg) {
                var rex = REX.new().op64bit().extReg();
                if (to > 7) {
                    rex = rex.extRMandSIBBase();
                }
                _ = try X64InstructionBuilder(1).new()
                    .rex(rex)
                    .inst(.{MOV_RR_MR})
                    .modRM(ModRM.new().reg(R15).rm(@intCast(to)).mode(ModRM.Mode.RM))
                    .nosib()
                    .finish(out);
            } else {
                var offset = (to - MaxReg) * 8;
                _ = try X64InstructionBuilder(1).new()
                    .rex(REX.new().op64bit().extReg())
                    .inst(.{MOV_RR_MR})
                    .modRM(ModRM.new().reg(R15).rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset)), out);
            }
        }

        fn dest_left_right(cdest: u64, creg: u64, crm: u64, behaviour: X64DestLeftRightBehaviour, inst_rm: [len]u8, inst_mr: [len]u8, out: *std.ArrayList(u8)) !void {
            var dest = cdest;
            var reg = creg;
            var rm = crm;

            // Ignore RSP+RBP
            if (reg > 3) {
                reg += 2;
            }
            if (rm > 3) {
                rm += 2;
            }
            if (dest > 3) {
                dest += 2;
            }

            if (behaviour == .MovLeftApplyRight) {
                try @This().left_and_right0(dest, reg, .Dirty, .{MOV_RR_RM}, .{MOV_RR_MR}, out);
            } else if (behaviour == .OneDestApplyBoth or behaviour == .ZeroDestApplyBoth) {
                var disp: u8 = if (behaviour == .OneDestApplyBoth) 1 else 0;
                if (dest <= MaxReg) {
                    var rex = REX.new().op64bit();
                    if (dest > 7) {
                        rex = rex.extReg();
                    }
                    _ = try X64InstructionBuilder(1)
                        .new()
                        .rex(rex)
                        .inst(.{MOV_MIMM8})
                        .modRM(ModRM.new().rm(@intCast(dest)).mode(ModRM.Mode.RM))
                        .nosib()
                        .disp32(disp, out); // this is abusive
                } else {
                    var offset = (dest - MaxReg) * 8;
                    _ = try X64InstructionBuilder(1)
                        .new()
                        .norex()
                        .inst(.{MOV_MIMM8})
                        .modRM(ModRM.new().rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                        .sib(SIB.nothing())
                        .disp32(-@as(i32, @intCast(offset)), out);
                    try out.append(disp);
                }
            }

            // apply left
            if (behaviour == .OneDestApplyBoth or behaviour == .ZeroDestApplyBoth) {
                if (reg <= MaxReg and dest <= MaxReg) {
                    var rex = REX.new().op64bit();
                    if (reg > 7) {
                        rex = rex.extRMandSIBBase();
                    }
                    if (dest > 7) {
                        rex = rex.extReg();
                    }
                    _ = try X64InstructionBuilder(1).new()
                        .rex(rex)
                        .inst(inst_rm)
                        .modRM(ModRM.new().reg(@intCast(dest)).rm(@intCast(reg)).mode(ModRM.Mode.RM))
                        .finish(out);
                } else if (reg <= MaxReg) {
                    var rex = REX.new().op64bit();
                    if (reg > 7) {
                        rex = rex.extReg();
                    }
                    var offset = (dest - MaxReg) * 8;
                    _ = try X64InstructionBuilder(1)
                        .new()
                        .rex(rex)
                        .inst(inst_mr)
                        .modRM(ModRM.new().reg(@intCast(reg)).rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                        .nosib()
                        .disp32(-@as(i32, @intCast(offset)), out);
                } else if (dest <= MaxReg) {
                    var rex = REX.new().op64bit();
                    if (dest > 7) {
                        rex = rex.extReg();
                    }
                    var offset = (reg - MaxReg) * 8;
                    _ = try X64InstructionBuilder(1)
                        .new()
                        .rex(rex)
                        .inst(inst_rm)
                        .modRM(ModRM.new().reg(@intCast(dest)).rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                        .nosib()
                        .disp32(-@as(i32, @intCast(offset)), out);
                } else {
                    var reg_offset = (reg - MaxReg) * 8;
                    var dest_offset = (dest - MaxReg) * 8;
                    // mov r15, reg
                    _ = try X64InstructionBuilder(1).new()
                        .rex(REX.new().op64bit().extReg())
                        .inst(.{MOV_RR_RM}).modRM(ModRM.new().reg(R15).rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                        .nosib()
                        .disp32(-@as(i32, @intCast(reg_offset)), out);
                    // inst dest, r15
                    _ = try X64InstructionBuilder(len).new()
                        .rex(REX.new().op64bit().extReg())
                        .inst(inst_mr).modRM(ModRM.new().reg(R15).rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                        .nosib()
                        .disp32(-@as(i32, @intCast(dest_offset)), out);
                }
            }

            // apply right
            if (rm <= MaxReg and dest <= MaxReg) {
                var rex = REX.new().op64bit();
                if (rm > 7) {
                    rex = rex.extRMandSIBBase();
                }
                if (dest > 7) {
                    rex = rex.extReg();
                }
                _ = try X64InstructionBuilder(1).new()
                    .rex(rex)
                    .inst(inst_rm)
                    .modRM(ModRM.new().reg(@intCast(dest)).rm(@intCast(rm)).mode(ModRM.Mode.RM))
                    .finish(out);
            } else if (rm <= MaxReg) {
                var rex = REX.new().op64bit();
                if (rm > 7) {
                    rex = rex.extReg();
                }
                var offset = (dest - MaxReg) * 8;
                _ = try X64InstructionBuilder(1)
                    .new()
                    .rex(rex)
                    .inst(inst_mr)
                    .modRM(ModRM.new().reg(@intCast(rm)).rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset)), out);
            } else if (dest <= MaxReg) {
                var rex = REX.new().op64bit();
                if (dest > 7) {
                    rex = rex.extReg();
                }
                var offset = (rm - MaxReg) * 8;
                _ = try X64InstructionBuilder(1)
                    .new()
                    .rex(rex)
                    .inst(inst_rm)
                    .modRM(ModRM.new().reg(@intCast(dest)).rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset)), out);
            } else {
                var dest_offset = (dest - MaxReg) * 8;
                var rm_offset = (rm - MaxReg) * 8;
                // mov r15, reg
                _ = try X64InstructionBuilder(1).new()
                    .rex(REX.new().op64bit().extReg())
                    .inst(.{MOV_RR_RM}).modRM(ModRM.new().reg(R15).rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                    .nosib()
                    .disp32(-@as(i32, @intCast(rm_offset)), out);
                // inst dest, r15
                _ = try X64InstructionBuilder(len).new()
                    .rex(REX.new().op64bit().extReg())
                    .inst(inst_mr).modRM(ModRM.new().reg(R15).rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                    .nosib()
                    .disp32(-@as(i32, @intCast(dest_offset)), out);
            }
        }
    };
}

const Compiler = struct {
    const JmpFix = struct {
        label: u64,
        fixat: u64,

        fn fixjmp(jmpfix: *const Compiler.JmpFix, location: u64, code: *std.ArrayList(u8)) void {
            code.items[jmpfix.fixat + 0] = 0x49; // REX.WB
            code.items[jmpfix.fixat + 1] = 0xbf; // MOVABS r15
            var num = @as(*const [8]u8, @ptrCast(&location)).*;
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

    ir: *std.ArrayList(IR),
    machinecode: std.ArrayList(u8),
    jmpfixes: std.ArrayList(JmpFix),
    jmplabelmap: std.AutoHashMap(u64, u64),

    fn new(allocator: std.mem.Allocator, ir: *std.ArrayList(IR)) Compiler {
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
                    var fix = try comp(ir, &compiler.machinecode);
                    if (fix) |fixat| {
                        try compiler.jmpfixes.append(fixat);
                    }
                },
            }
        }
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

fn comp(ir: IR, out: *std.ArrayList(u8)) !?Compiler.JmpFix {
    const MOV_RR_MR: u8 = 0x89;
    const MOV_RR_RM: u8 = 0x8B;
    const ADD_MR: u8 = 0x01;
    const ADD_RM: u8 = 0x03;
    const SUB_RM: u8 = 0x2b;
    const SUB_MR: u8 = 0x29;

    const RBP: u8 = 0b101;
    const RSP: u8 = 0b100;
    const R15: u8 = 15;
    switch (ir) {
        IR.Copy => |cvalue| {
            var value = cvalue;
            try X64IrToInstructionBuilder(1).left_and_right(
                value.from,
                value.to,
                .Dirty,
                .{MOV_RR_RM},
                .{MOV_RR_MR},
                out,
            );
        },
        IR.Jmp => |cvalue| {
            var value = cvalue;
            var offset = out.items.len;

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
            var value = cvalue;
            if (value.dest == value.left) {
                try X64IrToInstructionBuilder(1).left_and_right(
                    value.left,
                    value.right,
                    .MovLeft,
                    .{ADD_RM},
                    .{ADD_MR},
                    out,
                );
            } else {
                try X64IrToInstructionBuilder(1).dest_left_right(
                    value.dest,
                    value.left,
                    value.right,
                    X64DestLeftRightBehaviour.MovLeftApplyRight,
                    .{ADD_RM},
                    .{ADD_MR},
                    out,
                );
            }
        },
        IR.Sub => |cvalue| {
            var value = cvalue;
            if (value.dest == value.left) {
                try X64IrToInstructionBuilder(1).left_and_right(
                    value.left,
                    value.right,
                    .MovLeft,
                    .{SUB_RM},
                    .{SUB_MR},
                    out,
                );
            } else {
                try X64IrToInstructionBuilder(1).dest_left_right(
                    value.dest,
                    value.left,
                    value.right,
                    X64DestLeftRightBehaviour.MovLeftApplyRight,
                    .{SUB_RM},
                    .{SUB_MR},
                    out,
                );
            }
        },
        IR.Mul => |cvalue| {
            var value = cvalue;
            try X64IrToInstructionBuilder(0).copy_to_r15(value.left, out);
            const builder = X64IrToInstructionBuilder(0);
            if (value.right > 3) {
                value.right += 2;
            }
            if (value.right > builder.MaxReg) {
                var offset = (value.right - X64IrToInstructionBuilder(0).MaxReg) * 8;
                _ = try X64InstructionBuilder(2).new()
                    .rex(REX.new().op64bit().extReg())
                    .inst(.{ 0x0f, 0xaf })
                    .modRM(ModRM.new().reg(R15).rm(RBP).mode(ModRM.Mode.SIB_DISP32))
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset)), out);
            } else {
                var rex = REX.new().op64bit().extReg();
                if (value.right > 7) {
                    rex = rex.extRMandSIBBase();
                }
                _ = try X64InstructionBuilder(2).new()
                    .rex(rex)
                    .inst(.{ 0x0f, 0xaf }).modRM(ModRM.new().reg(R15).rm(@intCast(value.right)).mode(ModRM.Mode.RM))
                    .nosib()
                    .finish(out);
            }
            try builder.copy_from_r15(value.dest, out);
        },
        IR.FnPrologue => |cvalue| {
            var value = cvalue;
            try out.appendSlice(&[_]u8{
                0x55, // push rbp
                0x48, 0x89, 0xe5, // mov rbp, rsp
            });
            _ = try X64InstructionBuilder(1).new()
                .rex(REX.new().op64bit()).inst(.{
                0x81, // sub rsp, smthsmth with modrm.reg = 5
            })
                .modRM(ModRM.new().reg(5).rm(RSP).mode(ModRM.Mode.RM))
                .nosib()
                .disp32(@intCast((value.registers - X64IrToInstructionBuilder(0).MaxReg + 2) * 8), out);
        },
        IR.FnEpilogue => |cvalue| {
            var value = cvalue;
            _ = try X64InstructionBuilder(1).new()
                .rex(REX.new().op64bit()).inst(.{
                0x81, // add rsp, smthsmth with modrm.reg = 0
            })
                .modRM(ModRM.new().reg(0).rm(RSP).mode(ModRM.Mode.RM))
                .nosib()
                .disp32(@intCast((value.registers - X64IrToInstructionBuilder(0).MaxReg + 2) * 8), out);
            try out.appendSlice(&[_]u8{
                0x48, 0x89, 0xec, // mov rsp, rbp
                0x5d, // pop rbp
            });
        },
        IR.Return => {
            try out.append(0xc3); // ret
        },
        else => @panic("Not yet supported"),
    }
    return null;
}

fn compile(allocator: std.mem.Allocator, relative_offset: u64) !std.ArrayList(u8) {
    _ = relative_offset;
    var list = try std.ArrayList(u8).init(allocator);
    _ = list;
}

pub fn main() !void {
    var code = [_]u8{
        0x89, 0xC8, // mov eax,ecx
        0x83, 0xC0, 0x05, // add eax, 5
        0xC3, // ret
    };
    _ = code;
    var buff = try alloc(100);
    // var disp: i32 = 984;
    // std.debug.print("{}\n", .{@as(*const [4]u8, @ptrCast(&disp))[1]});
    // std.mem.copy(u8, buff, &code);
    // var fun: *fn (i32) i32 = @ptrCast(buff);
    // std.debug.print("{}\n", .{fun(10)});
    var ir: std.ArrayList(IR) = std.ArrayList(IR).init(std.heap.page_allocator);
    try ir.append(.{ .Label = .{ .name = 1 } });
    try ir.append(.{ .FnPrologue = .{ .registers = 123 } });
    try ir.append(.{ .FnEpilogue = .{ .registers = 123 } });
    try ir.append(.Return);

    var compiler = Compiler.new(std.heap.page_allocator, &ir);
    try compiler.compile();
    compiler.fixToBaseLocation(1234);

    @memset(buff, 0);
    std.mem.copy(u8, buff, compiler.machinecode.items);
    for (buff) |item| {
        std.debug.print("{x:0>2} ", .{item});
    }
    std.debug.print("\n", .{});
    var f: *fn () i32 = @ptrCast(buff);
    _ = f();
    std.debug.print("Worked\n", .{});
}
