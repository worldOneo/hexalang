const jit = @import("./generic_jit.zig");
const std = @import("std");

// Use RAX, RCX, RDX, RBX, RSI, RDI, R8, R9, R10 (0,1,2,3,6,7,8,9,10) as temporary registers
// Use RSP and RBP (4,5) as stack registers but stack grows upwards for resizing
// Use R11 as yield payload
// Use R12 for temporary values
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
        return self.loadVirtualRegister(part, currentFn.registers_required + number, out);
    }

    pub fn storeToArgRegister(self: *X86_64jit, currentFn: jit.FnData, callFn: jit.FnData, part: jit.PartialRegister, loaded_at: u64, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        return self.storeVirtualRegister(part, loaded_at, currentFn.registers_required + callFn.return_value_registers + number, out);
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

    fn loadconst(self: *X86_64jit, val: u64, part: jit.PartialRegister, number: u64, out: *std.ArrayList(u8)) std.mem.Allocator!u64 {
        const movabsq = jit.instgen("(01001|@1[3-4]|00)(10111|@[0-3])$2[0-8]"){};
        const claim = self.registers.claimRegister(part, number);
        if (claim.mustStore) |old| {
            try self.storeVirtualRegister(old.part, claim.claimed, old.vreg, out);
        }
        _ = try movabsq.write(out, .{val});
        return claim.claimed;
    }
    fn biop(self: *X86_64jit, operation: jit.BiOp, left: u64, right: u64, dest: u64, out: *std.ArrayList(u8)) std.mem.Allocator.Error!u64 {}
    fn jmpif(self: *X86_64jit, label: jit.Label, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?jit.LabelFix {}
    fn jmp(self: *X86_64jit, label: jit.Label, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?jit.LabelFix {}
    fn call(self: *X86_64jit, func: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!?jit.LabelFix {}
    fn yield(self: *X86_64jit, callback: *const fn (*anyopaque) void, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {}
    fn fnprologue(self: *X86_64jit, data: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {}
    fn fnepilogue(self: *X86_64jit, data: jit.FnData, out: *std.ArrayList(u8)) std.mem.Allocator.Error!void {}
};

fn create_x86_64_arch(allocator: std.mem.Allocator) jit.Arch {
    return jit.createJITFrom(X86_64jit, allocator.create(X86_64jit));
}
