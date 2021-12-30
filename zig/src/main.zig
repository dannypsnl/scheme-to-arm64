const std = @import("std");
const stdout = std.io.getStdOut().writer();
const rep = @import("./representation.zig");
const c = @cImport({
    @cInclude("gc.h");
});

extern "c" fn scheme_entry() i64;

pub fn main() !void {
    const result: i64 = scheme_entry();
    rep.show(result);
    try stdout.print("\n", .{});
}

export fn _scheme_cons(car: i64, cdr: i64) callconv(.C) i64 {
    const p: [*]i64 = @intToPtr([*]i64, @ptrToInt(c.GC_malloc(2 * rep.WORDSIZE)));
    p[0] = car;
    p[1] = cdr;
    return @intCast(i64, @ptrToInt(p)) | rep.PAIR_TAG;
}

export fn _scheme_make_string(length: i64, filled_by: i64) callconv(.C) i64 {
    const len: i64 = length >> rep.FIXNUM_SHIFT;
    const p: [*]i64 = @intToPtr([*]i64, @ptrToInt(c.GC_malloc(1 + @intCast(usize, @divTrunc(len, 8)))));
    p[0] = len;
    var p2: [*]u8 = @ptrCast([*]u8, p + 1);
    var i: usize = 0;
    while (i <= len) {
        p2[i] = @intCast(u8, filled_by >> rep.CHAR_SHIFT);
        i += 1;
    }
    return @intCast(i64, @ptrToInt(p)) | rep.STR_TAG;
}

export fn _scheme_make_vector(length: i64, filled_by: i64) callconv(.C) i64 {
    const len: i64 = length >> rep.FIXNUM_SHIFT;
    const p: [*]i64 = @intToPtr([*]i64, @ptrToInt(c.GC_malloc(1 + @intCast(usize, len))));
    p[0] = len;
    const p2: [*]i64 = p + 1;
    var i: usize = 0;
    while (i <= len) {
        p2[i] = filled_by;
        i += 1;
    }
    return @intCast(i64, @ptrToInt(p)) | rep.VEC_TAG;
}
