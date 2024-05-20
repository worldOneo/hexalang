const std = @import("std");

fn isHex(comptime char: u8) bool {
    return (char >= 'a' and char <= 'f') or (char >= '0' and char <= '9');
}

fn fromHex(comptime char: u8) u64 {
    return @intCast(if (char >= 'a' and char <= 'f') char - 'a' + 9 else char - '0');
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

fn instgen(comptime str: []const u8) type {
    comptime var argcount = 0;
    @setEvalBranchQuota(2000000);

    comptime var argidx: usize = 0;
    while (argidx < str.len) {
        if (str[argidx] == '@' or str[argidx] == '$') {
            argidx += 1;
            var arg = readNum(str, &argidx);
            argcount = @max(arg, argcount);
        } else {
            argidx += 1;
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

    const NoopWrite = struct {
        const Self = @This();
        fn write(_: *const Self, _: *std.ArrayList(u8), _: [argcount]u64) !ContinueWrite {
            return .{ .began = 0, .written = 0 };
        }
    };
    const WriteTypes = struct {
        fn AppendByte(comptime Val: u8, comptime Prev: type) type {
            return struct {
                const Self = @This();
                prev: Prev = Prev{},
                fn write(self: *const Self, out: *std.ArrayList(u8), args: [argcount]u64) !ContinueWrite {
                    const old: ContinueWrite = try self.prev.write(out, args);
                    return try old.pushByte(Val, out);
                }
            };
        }

        fn AppendArgsBits(comptime Start: usize, comptime Stop: usize, comptime ArgNum: usize, comptime Prev: type) type {
            // TODO: Support way to access nbits as little endian
            return struct {
                const Self = @This();
                prev: Prev = Prev{},
                fn write(self: *const Self, out: *std.ArrayList(u8), args: [argcount]u64) !ContinueWrite {
                    var old: ContinueWrite = try self.prev.write(out, args);
                    for (Start..Stop) |idx| {
                        old = try old.pushBit(@intCast(args[ArgNum] >> @intCast(idx) & 1), out);
                    }
                    return old;
                }
            };
        }

        fn AppendLittleEndianBytes(comptime Start: usize, comptime Stop: usize, comptime ArgNum: usize, comptime Prev: type) type {
            return struct {
                const Self = @This();
                prev: Prev = Prev{},
                fn write(self: *const Self, out: *std.ArrayList(u8), args: [argcount]u64) !ContinueWrite {
                    var old = try self.prev.write(out, args);
                    var arg: *const [8]u8 = @ptrCast(&args[ArgNum]);
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
                fn write(self: *const Self, out: *std.ArrayList(u8), args: [argcount]u64) !ContinueWrite {
                    const old: ContinueWrite = try self.prev.write(out, args);
                    return old.pushBit(Bit, out);
                }
            };
        }
    };

    // instgen format:
    //    push rax-rdi: "(01010|@1[0-3])",
    //    push r8-r15:  "x41(01010|@1[0-3])"
    //    mov  [rbp+offset], rax-rdi: "x48x89(1|@1[0-3]|0101)$2[0-4]"
    comptime var idx: usize = 0;
    var bropen = 0;

    var writer = NoopWrite;
    while (idx < str.len) {
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
        if (bropen == 1 and str[idx] == '|') {
            idx += 1;
            continue;
        }
        if (bropen == 1 and (str[idx] == '0' or str[idx] == '1')) {
            writer = WriteTypes.AppendBit(if (str[idx] == '1') 1 else 0, writer);
            idx += 1;
            continue;
        }
        if (str[idx] != '@' and !(bropen == 0 and str[idx] == '$')) {
            @compileError("invalid instgen expression: '" ++ str ++ "' invalid char '" ++ .{str[idx]} ++ "' expected @ " ++ if (bropen == '1') "or $" else "");
        }
        var is_little_endian = str[idx] == '$';

        idx += 1;
        if (idx >= str.len or str[idx] < '1' or str[idx] > '9') {
            @compileError("invalid instgen expression: '" ++ str ++ "' expected none null number after @");
        }
        var paramnum = readNum(str, &idx);
        if (idx >= str.len or str[idx] != '[') {
            @compileError("invalid instgen expression: '" ++ str ++ "' expected '[' after parameter number");
        }
        idx += 1;
        var start = readNum(str, &idx);
        if (idx >= str.len or str[idx] != '-') {
            @compileError("invalid instgen expression: '" ++ str ++ "' expected '-' after start index");
        }
        idx += 1;
        var end = readNum(str, &idx);
        if (idx >= str.len or str[idx] != ']') {
            @compileError("invalid instgen expression: '" ++ str ++ "' expected ']' after end idx");
        }
        idx += 1;
        if (is_little_endian) {
            writer = WriteTypes.AppendLittleEndianBytes(start, end, paramnum - 1, writer);
        } else {
            writer = WriteTypes.AppendArgsBits(start * if (bropen == 1) 1 else 8, end * if (bropen == 1) 1 else 8, paramnum - 1, writer);
        }
    }
    if (bropen == 1) {
        @compileError("invalid instgen expression: '" ++ str ++ "' unclosed paren");
    }

    return writer;
}
const generated = instgen("x48x89(1|@1[0-3]|0101)$2[0-4]"){};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gpa.allocator();
    var out = std.ArrayList(u8).init(alloc);
    _ = try generated.write(&out, .{ 0b0, 0x3039 });
    const stdout = std.io.getStdOut().writer();
    for (out.items) |item| {
        std.debug.print("{x:0>2} ", .{item});
    }
    std.debug.print("\n", .{});
    try stdout.print("Hello, {}!\n", .{instgen("x48x89(1|@1[0-3]|0101)$2[0-4]")});
}
