const std = @import("std");

fn alloc(size: usize) ![]u8 {
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
}

const IR = union(enum) {
    Copy: struct {
        from: u64,
        to: u64,
    },
    Add: struct {
        left: u64,
        right: u64,
    },
    Jmp: struct {
        label: u64,
    },
    Label: struct {
        name: u64,
    },
    FnEpilogue,
    FnPrologue: struct {
        registers: u64,
    },
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

const Compiler = struct {
    const JmpFix = struct {
        label: u64,
        fixat: u64,
    };

    ir: *std.ArrayList(IR),
    machinecode: std.ArrayList(u8),
    jmpfixes: std.ArrayList(JmpFix),
    jmplabelmap: std.AutoHashMap(u64, u64),
};

fn comp(ir: IR, out: *std.ArrayList(u8)) !void {
    const MOV_RR_MR: u8 = 0x89;
    const MOV_RR_RM: u8 = 0x8B;

    const PUSH_RAX: u8 = 0x50;
    const POP_RAX: u8 = 0x58;
    const RBP: u8 = 0b101;
    switch (ir) {
        IR.Copy => |cvalue| {
            var value = cvalue;

            // Ignore RSP+RBP
            if (value.from > 3) {
                value.from += 2;
            }
            if (value.to > 3) {
                value.to += 2;
            }

            // pick RegReg, RegMem, MemReg, or MemMem move
            if (value.from < 14 and value.to < 14) {
                var rex = REX.new().op64bit();

                if (value.from > 7) {
                    rex = rex.extReg();
                }
                if (value.to > 7) {
                    rex = rex.extRMandSIBBase();
                }
                if (value.to > 16 or value.from > 16) {
                    @panic("more than 16 regs not yet supported");
                }
                _ = try X64InstructionBuilder(1).new()
                    .rex(rex)
                    .inst(.{MOV_RR_RM})
                    .modRM(ModRM.new().reg(@intCast(value.to)).rm(@intCast(value.from)).mode(ModRM.Mode.RM)) // <reg> <- <reg>
                    .finish(out);
            } else if (value.from >= 14 and value.to >= 14) {
                // ugly mem-mem case
                var offset_from = (value.from - 14) * 8;
                var offset_to = (value.to - 14) * 8;
                try out.append(PUSH_RAX);

                // MOV rax, [rsp - x]
                _ = try X64InstructionBuilder(1).new()
                    .rex(REX.new().op64bit())
                    .inst(.{MOV_RR_RM})
                    .modRM(ModRM.new().reg(0).rm(RBP).mode(ModRM.Mode.SIB_DISP32)) // rax <- rsp-offset
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset_from)), out);

                // MOV [rsp - y], rax
                _ = try X64InstructionBuilder(1).new()
                    .rex(REX.new().op64bit())
                    .inst(.{MOV_RR_MR})
                    .modRM(ModRM.new().reg(0).rm(RBP).mode(ModRM.Mode.SIB_DISP32)) // rsp-offset <- rax
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset_to)), out);

                try out.append(POP_RAX);
            } else if (value.from < 14) {
                var rex = REX.new().op64bit();
                if (value.from > 7) {
                    rex = rex.extReg();
                }
                var offset_to = (value.to - 14) * 8;
                // MOV [rsp - y], rax
                _ = try X64InstructionBuilder(1).new()
                    .rex(rex)
                    .inst(.{MOV_RR_MR})
                    .modRM(ModRM.new().reg(@intCast(value.from)).rm(RBP).mode(ModRM.Mode.SIB_DISP32)) // rbp-offset <- <reg>
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset_to)), out);
            } else {
                var rex = REX.new().op64bit();
                if (value.to > 7) {
                    rex = rex.extReg();
                }
                var offset_from = (value.from - 14) * 8;
                // MOV rax, MOV [rsp - y]
                _ = try X64InstructionBuilder(1).new()
                    .rex(rex)
                    .inst(.{MOV_RR_RM})
                    .modRM(ModRM.new().reg(@intCast(value.to)).rm(RBP).mode(ModRM.Mode.SIB_DISP32)) // <reg> <- rbp-offset
                    .nosib()
                    .disp32(-@as(i32, @intCast(offset_from)), out);
            }
        },
        else => @panic("Not yet supported"),
    }
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
    // var buff = try alloc(100);
    // std.mem.copy(u8, buff, &code);
    // var fun: *fn (i32) i32 = @ptrCast(buff);
    // std.debug.print("{}\n", .{fun(10)});
    var bin: std.ArrayList(u8) = std.ArrayList(u8).init(std.heap.page_allocator);
    try comp(.{ .Copy = .{ .from = 7, .to = 8 } }, &bin);
    var data = bin.items;
    for (data) |item| {
        std.debug.print("{x} ", .{item});
    }
    std.debug.print("\n", .{});
}
