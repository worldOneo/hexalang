const x86_64 = @import("./x86_64_jit.zig");
const std = @import("std");

fn isHex(comptime char: u8) bool {
    return (char >= 'a' and char <= 'f') or (char >= '0' and char <= '9');
}

fn fromHex(comptime char: u8) u64 {
    return @intCast(if (char >= 'a' and char <= 'f') char - 'a' + 10 else char - '0');
}

fn readNum(comptime str: []const u8, comptime offset: *usize) comptime_int {
    var num = 0;
    var idx = offset.*;
    num = @intCast(str[idx] - '0');
    idx += 1;
    while (idx < str.len and str[idx] >= '0' and str[idx] <= '9') {
        num *= 10;
        num += @intCast(str[idx] - '0');
        idx += 1;
    }
    offset.* = idx;
    return num;
}

pub fn instgen(comptime sstr: []const u8) type {
    comptime var count = 0;
    @setEvalBranchQuota(2000000);
    {
        const str = sstr;
        comptime var argidx: usize = 0;
        while (argidx < str.len) {
            if (str[argidx] == '@' or str[argidx] == '$') {
                argidx += 1;
                const arg = readNum(str, &argidx);
                count = @max(arg, count);
            } else {
                argidx += 1;
            }
        }
    }

    const ContinueWrite = struct {
        const Self = @This();

        began: u8,
        written: u64,

        fn pushBit(self: *const Self, bit: u8, out: *std.ArrayList(u8)) !Self {
            if (self.written + 1 == 8) {
                try out.append((self.began << 1) + bit);
                return .{ .began = 0, .written = 0 };
            }
            return .{ .began = (self.began << 1) + bit, .written = self.written + 1 };
        }

        fn pushByte(self: *const Self, byte: u8, out: *std.ArrayList(u8)) !Self {
            var now = self.*;
            for (0..8) |idx| {
                now = try now.pushBit((byte >> @intCast(7 - idx)) & 1, out);
            }
            return now;
        }
    };

    const StartWrite = struct {
        const Self = @This();
        fn write(_: *const Self, c: ContinueWrite, _: *std.ArrayList(u8), _: [count]u64) !ContinueWrite {
            return c;
        }
    };

    const WriteTypes = struct {
        fn FirstCall(comptime Prev: type) type {
            return struct {
                const Self = @This();
                prev: Prev = Prev{},
                pub fn write(self: *const Self, out: *std.ArrayList(u8), args: [count]u64) !ContinueWrite {
                    return self.prev.write(.{ .began = 0, .written = 0 }, out, args);
                }
            };
        }
        fn NoopWriter(comptime Prev: type) type {
            return struct {
                const Self = @This();
                prev: Prev = Prev{},
                fn write(self: *const Self, c: ContinueWrite, out: *std.ArrayList(u8), args: [count]u64) !ContinueWrite {
                    return self.prev.write(c, out, args);
                }
            };
        }
        fn CondionalWriteBits(comptime Start: usize, comptime Stop: usize, comptime ArgNum: usize, comptime Cond: type, comptime Otherwise: type, comptime Prev: type) type {
            return struct {
                const Self = @This();
                prev: Prev = Prev{},
                cond: Cond = Cond{},
                otherwise: Otherwise = Otherwise{},
                fn write(self: *const Self, c: ContinueWrite, out: *std.ArrayList(u8), args: [count]u64) !ContinueWrite {
                    var old: ContinueWrite = try self.prev.write(c, out, args);
                    const mask = ((1 << @intCast(Stop - Start)) - 1) << @intCast(Start);
                    const condwrite = args[ArgNum] & mask != 0;
                    if (condwrite) {
                        old = try self.cond.write(old, out, args);
                    } else {
                        old = try self.otherwise.write(old, out, args);
                    }
                    return old;
                }
            };
        }

        fn AppendByte(comptime Val: u8, comptime Prev: type) type {
            return struct {
                const Self = @This();
                prev: Prev = Prev{},
                fn write(self: *const Self, c: ContinueWrite, out: *std.ArrayList(u8), args: [count]u64) !ContinueWrite {
                    const old: ContinueWrite = try self.prev.write(c, out, args);
                    return try old.pushByte(Val, out);
                }
            };
        }

        fn AppendArgsBits(comptime Start: usize, comptime Stop: usize, comptime ArgNum: usize, comptime Prev: type) type {
            // TODO: Support way to access nbits as little endian
            return struct {
                const Self = @This();
                prev: Prev = Prev{},
                fn write(self: *const Self, c: ContinueWrite, out: *std.ArrayList(u8), args: [count]u64) !ContinueWrite {
                    var old: ContinueWrite = try self.prev.write(c, out, args);
                    for (Start..Stop) |idx| {
                        old = try old.pushBit(@intCast(args[ArgNum] >> @intCast(Stop - 1 - idx + Start) & 1), out);
                    }
                    return old;
                }
            };
        }

        fn AppendLittleEndianBytes(comptime Start: usize, comptime Stop: usize, comptime ArgNum: usize, comptime Prev: type) type {
            return struct {
                const Self = @This();
                prev: Prev = Prev{},
                fn write(self: *const Self, c: ContinueWrite, out: *std.ArrayList(u8), args: [count]u64) !ContinueWrite {
                    var old = try self.prev.write(c, out, args);
                    const arg: *const [8]u8 = @ptrCast(&args[ArgNum]);
                    for (Start..Stop) |idx| {
                        old = try old.pushByte(arg[idx], out);
                    }
                    return old;
                }
            };
        }

        fn AppendBit(comptime Bit: u8, comptime Prev: type) type {
            return struct {
                const Self = @This();
                prev: Prev = Prev{},
                fn write(self: *const Self, c: ContinueWrite, out: *std.ArrayList(u8), args: [count]u64) !ContinueWrite {
                    const old: ContinueWrite = try self.prev.write(c, out, args);
                    return old.pushBit(Bit, out);
                }
            };
        }
    };

    var iidx: usize = 0;
    const Template = struct {
        fn recursive_instgen(comptime str: []const u8, comptime argcount: comptime_int, comptime offset: *usize, comptime expect_closing: bool) type {

            // instgen format:
            //    push rax-rdi: "(01010|@1[0-3])",
            //    push r8-r15:  "x41(01010|@1[0-3])"
            //    push rax-r15: "{1[3-4]=x41}(01010|@1[0-3])"
            //    mov  [rbp+offset], rax-rdi: "x48x89(1|@1[0-3]|0101)$2[0-4]"
            comptime var idx: usize = offset.*;
            var bropen = 0;

            var writer = StartWrite;
            while (idx < str.len) {
                if (expect_closing and (str[idx] == '}' or str[idx] == ':')) {
                    offset.* = idx;
                    return writer;
                }
                if (str[idx] == ')') {
                    if (bropen == 0) {
                        @compileError("invalid instgen expression: '" ++ str ++ "' paren wasn't opened");
                    }
                    bropen = 0;
                    idx += 1;
                    continue;
                }
                if (str[idx] == '(') {
                    if (bropen == 1) {
                        @compileError("invalid instgen expression: '" ++ str ++ "' only one level of parens allowed");
                    }
                    bropen = 1;
                    idx += 1;
                    continue;
                }
                if (str[idx] == 'x') {
                    if (idx + 2 >= str.len or !isHex(str[idx + 1]) or !isHex(str[idx + 2])) {
                        @compileError("invalid instgen expression: '" ++ str ++ "' expected 2 digit hexadecimal after x");
                    }
                    writer = WriteTypes.AppendByte((fromHex(str[idx + 1]) << 4) + fromHex(str[idx + 2]), writer);
                    idx += 3;
                    continue;
                }
                if (str[idx] == '|') {
                    idx += 1;
                    continue;
                }
                if (bropen == 1 and (str[idx] == '0' or str[idx] == '1')) {
                    writer = WriteTypes.AppendBit(if (str[idx] == '1') 1 else 0, writer);
                    idx += 1;
                    continue;
                }
                if (str[idx] != '@' and !(bropen == 0 and str[idx] == '$') and str[idx] != '{') {
                    @compileError("invalid instgen expression: '" ++ str ++ "' invalid char '" ++ .{str[idx]} ++ "' expected @" ++ if (bropen == '1') ",0,1,x, or $" else ",$, or x");
                }
                const is_little_endian = str[idx] == '$';
                const conditional = str[idx] == '{';

                idx += 1;
                if (idx >= str.len or str[idx] < '1' or str[idx] > '9') {
                    @compileError("invalid instgen expression: '" ++ str ++ "' expected none null as parameter after '" ++ .{str[idx - 1]} ++ "'");
                }
                const paramnum = readNum(str, &idx);
                if (idx >= str.len or str[idx] != '[') {
                    @compileError("invalid instgen expression: '" ++ str ++ "' expected '[' after parameter number");
                }
                idx += 1;
                const start = readNum(str, &idx);
                if (idx >= str.len or str[idx] != '-') {
                    @compileError("invalid instgen expression: '" ++ str ++ "' expected '-' after start index");
                }
                idx += 1;
                const end = readNum(str, &idx);
                if (idx >= str.len or str[idx] != ']') {
                    @compileError("invalid instgen expression: '" ++ str ++ "' expected ']' after end index");
                }
                idx += 1;
                if (conditional) {
                    if (idx >= str.len or str[idx] != '=') {
                        @compileError("invalid instgen expression: '" ++ str ++ "' expected '=' condition");
                    }
                    idx += 1;
                    const nested_writer = recursive_instgen(str, argcount, &idx, true);
                    if (idx >= str.len or (str[idx] != ':' and str[idx] != '}')) {
                        @compileError("invalid instgen expression: '" ++ str ++ "' expected ':' or '}' after conditional clause");
                    }
                    const recurse = str[idx] == ':';
                    idx += 1;
                    const otherwise = if (recurse) recursive_instgen(str, argcount, &idx, true) else WriteTypes.NoopWriter(writer);
                    if (recurse and (idx >= str.len or str[idx] != '}')) {
                        @compileError("invalid instgen expression: '" ++ str ++ "' '}' after conditional clause");
                    } else if (recurse) {
                        idx += 1;
                    }
                    writer = WriteTypes.CondionalWriteBits(start, end, paramnum - 1, nested_writer, otherwise, writer);
                } else if (is_little_endian) {
                    writer = WriteTypes.AppendLittleEndianBytes(start, end, paramnum - 1, writer);
                } else {
                    writer = WriteTypes.AppendArgsBits(start * if (bropen == 1) 1 else 8, end * if (bropen == 1) 1 else 8, paramnum - 1, writer);
                }
            }
            if (bropen == 1) {
                @compileError("invalid instgen expression: '" ++ str ++ "' unclosed paren");
            }
            offset.* = idx;
            return writer;
        }
    }.recursive_instgen(sstr, count, &iidx, false);

    return WriteTypes.FirstCall(Template);
}

pub const OpSize = enum(u8) {
    B = 0b00000001,
    W = 0b00000011,
    DW = 0b00001111,
    QW = 0b11111111,
};

pub const RegisterPart = enum(u8) {
    B1 = 0b10000000,
    B2 = 0b01000000,
    B3 = 0b00100000,
    B4 = 0b00010000,
    B5 = 0b00001000,
    B6 = 0b00000100,
    B7 = 0b00000010,
    B8 = 0b00000001,
    W1 = 0b11000000,
    W2 = 0b00110000,
    W3 = 0b00001100,
    W4 = 0b00000011,
    DW1 = 0b11110000,
    DW2 = 0b00001111,
    QW = 0b11111111,

    pub fn popCount(self: RegisterPart) usize {
        return @popCount(@intFromEnum(self));
    }

    pub fn ctz(self: RegisterPart) usize {
        return @ctz(@intFromEnum(self));
    }

    pub fn clz(self: RegisterPart) usize {
        return @clz(@intFromEnum(self));
    }
};

pub const BiOp = enum {
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    SHIFT_L,
    SHIFT_R,
    XOR,
    OR,
    AND,
    EQ,
    NEQ,
    GEQ,
    GT,
};

pub const Label = struct {
    name: u64,
    known_at_offset: ?u64,
};

pub const LabelFix = struct {
    compiler_info: u64,
    name: u64,
    at_location: u64,
};

pub const FnData = struct {
    name: u64,
    /// The number of registers required for the return value
    return_value_registers: u64,
    /// The number of registers required for the arguments of the function
    arg_registers: u64,
    /// The number of registers required to store the local values of this function (including arg registers)
    registers_required: u64,
    /// The maximum number of additional registers required for calling another function
    max_call_arg_registers: u64,
    /// The maximum number of additional registers required for receiving the return value of another function
    max_call_return_registers: u64,
};

pub const Register = struct { number: u64, part: RegisterPart = RegisterPart.QW, function: FnData };

pub const Arch = struct {
    ptr: *anyopaque,
    loadVirtualRegister: *const fn (self: *anyopaque, register: Register, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64,
    storeVirtualRegister: *const fn (self: *anyopaque, register: Register, loaded_at: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void,

    loadSavedVirtualRegister: *const fn (self: *anyopaque, register: Register, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64,
    restoreVirtualRegisters: *const fn (self: *anyopaque, currentFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void,
    saveVirtualRegisters: *const fn (self: *anyopaque, currentFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void,

    allocateReturnArgRegisters: *const fn (self: *anyopaque, currentFn: FnData, callFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void,
    storeToArgRegister: *const fn (self: *anyopaque, currentFn: FnData, callFn: FnData, part: RegisterPart, loaded_at: u64, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void,
    storeToReturnRegister: *const fn (self: *anyopaque, currentFn: FnData, part: RegisterPart, loaded_at: u64, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void,
    loadFromReturnRegister: *const fn (self: *anyopaque, currentFn: FnData, callFn: FnData, part: RegisterPart, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64,

    loadconst: *const fn (self: *anyopaque, val: u64, register: Register, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64,
    biop: *const fn (self: *anyopaque, operation: BiOp, size: OpSize, left: u64, right: u64, dest: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void,
    jmpif: *const fn (self: *anyopaque, label: Label, reg: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?LabelFix,
    jmp: *const fn (self: *anyopaque, label: Label, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?LabelFix,
    call: *const fn (self: *anyopaque, currentFn: FnData, callFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?LabelFix,
    yield: *const fn (self: *anyopaque, currentFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void,

    fnreturn: *const fn (self: *anyopaque, currentFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?LabelFix,
    fnprologue: *const fn (self: *anyopaque, data: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void,
    fnepilogue: *const fn (self: *anyopaque, data: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void,

    /// Clears the idea of a loaded register
    /// this could be because of a scope ending
    /// i.e.:
    /// for
    ///
    /// if condition {
    ///     a = b + c
    /// }
    ///
    /// the to-ir-compiler must emite
    ///
    /// invalidate a
    /// invalidate b
    /// invalidate c
    ///
    /// TODO: Add to-ir-compiler hint to communicate when it is valid to ignore invalidation
    invalidateLoadedReg: *const fn (self: *anyopaque, currentfn: FnData, reg: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void,
};

pub fn createJITFrom(comptime Impl: type, ptr: *Impl) Arch {
    const Caster = struct {
        fn trueSelf(self: *anyopaque) *Impl {
            return @alignCast(@ptrCast(self));
        }

        fn loadVirtualRegister(self: *anyopaque, register: Register, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {
            return trueSelf(self).loadVirtualRegister(register, out);
        }
        fn storeVirtualRegister(self: *anyopaque, register: Register, loaded_at: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
            return trueSelf(self).storeVirtualRegister(register, loaded_at, out);
        }

        fn loadSavedVirtualRegister(self: *anyopaque, register: Register, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {
            return trueSelf(self).loadSavedVirtualRegister(register, out);
        }

        fn restoreVirtualRegisters(self: *anyopaque, currentFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
            return trueSelf(self).restoreVirtualRegisters(currentFn, out);
        }

        fn saveVirtualRegisters(self: *anyopaque, currentFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
            return trueSelf(self).saveVirtualRegisters(currentFn, out);
        }

        fn allocateReturnArgRegisters(self: *anyopaque, currentFn: FnData, callFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
            return trueSelf(self).allocateReturnArgRegisters(currentFn, callFn, out);
        }
        fn storeToArgRegister(self: *anyopaque, currentFn: FnData, callFn: FnData, part: RegisterPart, loaded_at: u64, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
            return trueSelf(self).storeToArgRegister(currentFn, callFn, part, loaded_at, number, out);
        }
        fn storeToReturnRegister(self: *anyopaque, currentFn: FnData, part: RegisterPart, loaded_at: u64, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
            return trueSelf(self).storeToReturnRegister(currentFn, part, loaded_at, number, out);
        }
        fn loadFromReturnRegister(self: *anyopaque, currentFn: FnData, callFn: FnData, part: RegisterPart, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {
            return trueSelf(self).loadFromReturnRegister(currentFn, callFn, part, number, out);
        }

        fn loadconst(self: *anyopaque, val: u64, register: Register, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {
            return trueSelf(self).loadconst(val, register, out);
        }
        fn biop(self: *anyopaque, operation: BiOp, part: OpSize, left: u64, right: u64, dest: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
            return trueSelf(self).biop(operation, part, left, right, dest, out);
        }
        fn jmpif(self: *anyopaque, label: Label, reg: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?LabelFix {
            return trueSelf(self).jmpif(label, reg, out);
        }
        fn jmp(self: *anyopaque, label: Label, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?LabelFix {
            return trueSelf(self).jmp(label, out);
        }
        fn call(self: *anyopaque, currentFn: FnData, callFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?LabelFix {
            return trueSelf(self).call(currentFn, callFn, out);
        }
        fn yield(self: *anyopaque, currentFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
            return trueSelf(self).yield(currentFn, out);
        }

        fn fnreturn(self: *anyopaque, currentFn: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?LabelFix {
            return trueSelf(self).fnreturn(currentFn, out);
        }

        fn fnprologue(self: *anyopaque, data: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
            return trueSelf(self).fnprologue(data, out);
        }
        fn fnepilogue(self: *anyopaque, data: FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
            return trueSelf(self).fnepilogue(data, out);
        }

        fn invalidateLoadedReg(self: *anyopaque, currentfn: FnData, reg: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
            return trueSelf(self).invalidateLoadedReg(currentfn, reg, out);
        }
    };
    return Arch{
        .ptr = @ptrCast(ptr),
        .loadVirtualRegister = Caster.loadVirtualRegister,
        .storeVirtualRegister = Caster.storeVirtualRegister,
        .loadSavedVirtualRegister = Caster.loadSavedVirtualRegister,
        .restoreVirtualRegisters = Caster.restoreVirtualRegisters,
        .saveVirtualRegisters = Caster.saveVirtualRegisters,
        .allocateReturnArgRegisters = Caster.allocateReturnArgRegisters,
        .storeToArgRegister = Caster.storeToArgRegister,
        .storeToReturnRegister = Caster.storeToReturnRegister,
        .loadFromReturnRegister = Caster.loadFromReturnRegister,
        .loadconst = Caster.loadconst,
        .biop = Caster.biop,
        .jmpif = Caster.jmpif,
        .jmp = Caster.jmp,
        .call = Caster.call,
        .yield = Caster.yield,
        .fnreturn = Caster.fnreturn,
        .fnprologue = Caster.fnprologue,
        .fnepilogue = Caster.fnepilogue,
        .invalidateLoadedReg = Caster.invalidateLoadedReg,
    };
}

pub fn LRURegisterAllocator(comptime N: usize, comptime Registers: [N]u64) type {
    return struct {
        const ManagedRegister = struct {
            next: ?*ManagedRegister = null,
            prev: ?*ManagedRegister = null,
            idx: usize = 0,
            part: RegisterPart = RegisterPart.QW,
            vreg: u64 = 0,
        };
        const Self = @This();

        pub const Iter = struct {
            _next: ?*ManagedRegister,

            pub fn next(self: *Iter) ?struct { idx: u64, vreg: u64, part: RegisterPart } {
                if (self._next) |n| {
                    self._next = n.next;
                    return .{ .idx = n.idx, .vreg = n.vreg, .part = n.part };
                }
                return null;
            }
        };

        usedregs: [N]ManagedRegister = [_]ManagedRegister{.{}} ** N,
        head: ?*ManagedRegister = null,
        tail: ?*ManagedRegister = null,
        freeregs: [N]usize = [_]usize{0} ** N,
        freeregidx: usize = 0,

        pub fn init() Self {
            var s: Self = .{};
            for (Registers, 0..) |_, i| {
                s.freeregs[N - i - 1] = i;
            }
            s.freeregidx = N;
            return s;
        }

        pub fn clear(self: *Self) void {
            self.* = self.init();
        }

        const RegisterClaim = struct {
            claimed: u64,
            loaded: bool = false,
            mustStore: ?struct { vreg: u64, part: RegisterPart },
        };

        pub fn claimRegister(self: *Self, part: RegisterPart, vreg: u64) RegisterClaim {
            if (self.getLoadedAt(vreg, part)) |idx| {
                self.touchRegister(idx);
                return .{ .claimed = idx, .loaded = true, .mustStore = null };
            }
            if (self.freeregidx > 0) {
                const idx = self.freeregs[self.freeregidx - 1];
                self.freeregidx -= 1;
                self.prependRegister(&self.usedregs[idx]);
                self.usedregs[idx].part = part;
                self.usedregs[idx].vreg = vreg;
                return .{ .claimed = idx, .mustStore = null };
            }
            if (self.tail) |*tail| {
                self.tail = tail.*.prev;
                const oldreg = tail.*.vreg;
                const oldpart = tail.*.part;
                tail.*.vreg = vreg;
                tail.*.part = part;
                return .{ .claimed = tail.*.idx, .mustStore = .{ .vreg = oldreg, .part = oldpart } };
            }
            unreachable;
        }

        fn unlinkRegister(self: *Self, reg: *ManagedRegister) void {
            if (reg.next) |*next| {
                next.*.prev = reg.prev;
            } else {
                self.tail = reg.prev;
            }
            if (reg.prev) |*prev| {
                prev.*.next = reg.next;
            } else {
                self.head = reg.next;
            }
        }

        fn prependRegister(self: *Self, reg: *ManagedRegister) void {
            if (self.head) |*head| {
                head.*.prev = reg;
                reg.next = head.*;
            } else {
                self.tail = reg;
            }
            self.head = reg;
        }

        pub fn returnRegister(self: *Self, idx: u64) struct { vreg: u64, part: RegisterPart } {
            const reg: *ManagedRegister = &self.usedregs[idx];
            self.unlinkRegister(reg);
            self.freeregs[self.freeregidx - 1] = reg.idx;
            self.freeregidx -= 1;
            return .{
                .vreg = reg.vreg,
                .part = reg.part,
            };
        }

        pub fn touchRegister(self: *Self, idx: u64) void {
            const reg: *ManagedRegister = &self.usedregs[idx];
            self.unlinkRegister(reg);
            self.prependRegister(reg);
        }

        pub fn iter(self: *Self) Iter {
            return Self.Iter{ ._next = self.head };
        }

        pub fn getLoadedAt(self: *Self, vreg: u64, part: RegisterPart) ?u64 {
            var _iter = self.iter();
            while (_iter.next()) |node| {
                if (node.vreg == vreg and node.part == part) {
                    return node.idx;
                }
            }
            return null;
        }
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var out = std.ArrayList(u8).init(alloc);
    var jit = try x86_64.create_x86_64_arch(alloc);
    const mainfn = FnData{
        .arg_registers = 0,
        .max_call_arg_registers = 1,
        .max_call_return_registers = 1,
        .registers_required = 1,
        .return_value_registers = 1,
        .name = 0,
    };
    const trivial = FnData{
        .arg_registers = 1,
        .max_call_arg_registers = 0,
        .max_call_return_registers = 0,
        .registers_required = 1,
        .return_value_registers = 1,
        .name = 1,
    };
    try jit.fnprologue(jit.ptr, mainfn, &out);
    _ = try jit.call(jit.ptr, mainfn, trivial, &out);
    const returned = try jit.loadFromReturnRegister(jit.ptr, mainfn, trivial, RegisterPart.QW, 0, &out);
    try jit.storeToReturnRegister(jit.ptr, mainfn, RegisterPart.QW, returned, 0, &out);
    try jit.fnepilogue(jit.ptr, mainfn, &out);
    const stdout = std.io.getStdOut().writer();
    for (out.items) |item| {
        std.debug.print("{x:0>2} ", .{item});
    }
    std.debug.print("\n", .{});
    try stdout.print("Hello, {}!\n", .{instgen("{1[3-4]=x41}(01010|@1[0-3])")});
}
