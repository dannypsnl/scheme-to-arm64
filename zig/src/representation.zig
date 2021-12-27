const std = @import("std");
const print = @import("std").debug.print;
const c = @cImport({
    @cInclude("representation.h");
});
const FIXNUM_MASK = c.FIXNUM_MASK;
const FIXNUM_TAG = c.FIXNUM_TAG;
const FIXNUM_SHIFT = c.FIXNUM_SHIFT;
const CHAR_MASK = c.CHAR_MASK;
const CHAR_TAG = c.CHAR_TAG;
const CHAR_SHIFT = c.CHAR_SHIFT;
const BOOL_MASK = c.BOOL_MASK;
const BOOL_TAG = c.BOOL_TAG;
const BOOL_SHIFT = c.BOOL_SHIFT;
const PTR_MASK = c.PTR_MASK;
const PAIR_TAG = c.PAIR_TAG;
const VOID_TAG = c.VOID_TAG;
const STR_TAG = c.STR_TAG;
const VEC_TAG = c.VEC_TAG;

fn putchar(x: u8) void {
    print("{c}", .{x});
}

pub fn show(x: i64) void {
    if ((x & FIXNUM_MASK) == FIXNUM_TAG) {
        // integer
        print("{d}", .{x >> FIXNUM_SHIFT});
    } else if ((x & CHAR_MASK) == CHAR_TAG) {
        // character
        const char = @truncate(u8, @intCast(u64, x >> CHAR_SHIFT));
        switch (char) {
            0 => print("#\\nul", .{}),
            ' ' => print("#\\space", .{}),
            '\n' => print("#\\newline", .{}),
            else => print("#\\{c}", .{char}),
        }
    } else if ((x & BOOL_MASK) == BOOL_TAG) {
        // boolean
        if ((x >> BOOL_SHIFT) != 0) {
            print("#t", .{});
        } else {
            print("#f", .{});
        }
    } else if ((x & PTR_MASK) == VOID_TAG) {} else if ((x & PTR_MASK) == PAIR_TAG) {
        var ptr = @intToPtr(?[*]i64, @intCast(usize, x - PAIR_TAG));
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
        while ((cdr & PTR_MASK) == PAIR_TAG) {
            ptr = @intToPtr(?[*]i64, @intCast(usize, cdr - PAIR_TAG));
            if (ptr == null) break;

            car = ptr.?[0];
            cdr = ptr.?[1];
            putchar(' ');
            show(car);
        }
        // show dotted pair notation if relevant
        if ((cdr & PTR_MASK) != PAIR_TAG) {
            print(" . ", .{});
            show(cdr);
        }
        putchar(')');
    } else if ((x & PTR_MASK) == STR_TAG) {
        const ptr = @intToPtr([*]i64, @intCast(usize, x - STR_TAG));
        var len: i64 = ptr[0];
        var body: [*]u8 = @ptrCast([*]u8, ptr + 1);
        putchar('"');
        while (len > 0) {
            body += 1;
            putchar(body[0]);
            len -= 1;
        }
        putchar('"');
    } else if ((x & PTR_MASK) == VEC_TAG) {
        var ptr = @intToPtr([*]i64, @intCast(usize, x - VEC_TAG));
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
