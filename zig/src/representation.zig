const std = @import("std");
const print = @import("std").debug.print;
const c = @cImport({
    @cInclude("representation.h");
});

fn putchar(x: u8) void {
    print("{c}", .{x});
}

pub fn show(x: i64) void {
    if ((x & c.FIXNUM_MASK) == c.FIXNUM_TAG) {
        // integer
        print("{d}", .{x >> c.FIXNUM_SHIFT});
    } else if ((x & c.CHAR_MASK) == c.CHAR_TAG) {
        // character
        const char = @truncate(u8, @intCast(u64, x >> c.CHAR_SHIFT));
        switch (char) {
            0 => print("#\\nul", .{}),
            ' ' => print("#\\space", .{}),
            '\n' => print("#\\newline", .{}),
            else => print("#\\{c}", .{char}),
        }
    } else if ((x & c.BOOL_MASK) == c.BOOL_TAG) {
        // boolean
        if ((x >> c.BOOL_SHIFT) != 0) {
            print("#t", .{});
        } else {
            print("#f", .{});
        }
    } else if ((x & c.PTR_MASK) == c.VOID_TAG) {} else if ((x & c.PTR_MASK) == c.PAIR_TAG) {
        var ptr = @intToPtr(?[*]i64, @intCast(usize, x - c.PAIR_TAG));
        if (ptr == null) {
            print("()", .{});
            return;
        }
        // either a list or a dotted pair
        var car: i64 = ptr.?[0];
        var cdr: i64 = ptr.?[1];
        putchar('(');
        show(car);
        // show additional space-separated elems
        while ((cdr & c.PTR_MASK) == c.PAIR_TAG) {
            ptr = @intToPtr(?[*]i64, @intCast(usize, cdr - c.PAIR_TAG));
            if (ptr == null) break;

            car = ptr.?[0];
            cdr = ptr.?[1];
            putchar(' ');
            show(car);
        }
        // show dotted pair notation if relevant
        if ((cdr & c.PTR_MASK) != c.PAIR_TAG) {
            print(" . ", .{});
            show(cdr);
        }
        putchar(')');
    } else if ((x & c.PTR_MASK) == c.STR_TAG) {
        const ptr = @intToPtr([*]i64, @intCast(usize, x - c.STR_TAG));
        var len: i64 = ptr[0];
        var body: [*]u8 = @ptrCast([*]u8, ptr + 1);
        putchar('"');
        while (len > 0) {
            body += 1;
            putchar(body[0]);
            len -= 1;
        }
        putchar('"');
    } else if ((x & c.PTR_MASK) == c.VEC_TAG) {
        var ptr = @intToPtr([*]i64, @intCast(usize, x - c.VEC_TAG));
        var len: i64 = ptr[0];
        ptr += 1;
        print("#(", .{});
        while (len > 0) {
            show(ptr[0]);
            ptr += 1;
            if (len != 1)
                putchar(' ');
            len -= 1;
        }
        print(")", .{});
    } else {
        print("bad: {d}", .{x});
    }
}
