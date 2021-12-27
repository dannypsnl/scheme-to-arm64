const std = @import("std");
const rep = @import("./representation.zig");

extern "c" fn scheme_entry() i64;

pub fn main() !void {
    const result: i64 = scheme_entry();
    rep.show(result);
    std.debug.print("\n", .{});
}
