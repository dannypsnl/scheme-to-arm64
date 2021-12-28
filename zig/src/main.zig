const std = @import("std");
const stdout = std.io.getStdOut().writer();
const rep = @import("./representation.zig");

extern "c" fn scheme_entry() i64;

pub fn main() !void {
    const result: i64 = scheme_entry();
    rep.show(result);
    try stdout.print("\n", .{});
}
