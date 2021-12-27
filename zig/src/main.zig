const std = @import("std");
const stdout = std.io.getStdOut().writer();
const c = @cImport({
    @cInclude("gc.h");
});
const rep = @import("./representation.zig");

extern fn scheme_entry() callconv(.C) i64;

export fn _scheme_cons(car: i64, cdr: i64) callconv(.C) i64 {
    // const p: [*c]i64 = c.GC_malloc(2 * rep.WORDSIZE);
    // p.* = car;
    // (p + 1).* = cdr;
    const allocator = std.heap.page_allocator;
    const p: []i64 = allocator.alloc(i64, 2) catch unreachable;
    p[0] = car;
    p[1] = cdr;
    return @intCast(i64, @ptrToInt(&p)) | rep.PAIR_TAG;
}

pub fn main() !void {
    const result: i64 = scheme_entry();
    rep.show(result);
    try stdout.print("\n", .{});
}
