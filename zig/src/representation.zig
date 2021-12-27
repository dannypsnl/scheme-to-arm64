const std = @import("std");
const print = @import("std").debug.print;

pub const WORDSIZE = 8;

pub const FIXNUM_MASK = 3;
pub const FIXNUM_TAG = 0;
pub const FIXNUM_SHIFT = 2;

pub const CHAR_MASK = 0xff;
pub const CHAR_SHIFT = 8;
pub const CHAR_TAG = 7;

pub const BOOL_MASK = 0xff;
pub const BOOL_SHIFT = 8;
pub const BOOL_TAG = 15;

pub const PTR_MASK = 7;
pub const PAIR_TAG = 1;
pub const VEC_TAG = 2;
pub const STR_TAG = 3;
pub const VOID_TAG = 5;
pub const CLOSURE_TAG = 6;

fn putchar(x: u8) void {
    print("{c}", .{x});
}

pub fn show(x: i64) void {
    if ((x & FIXNUM_MASK) == FIXNUM_TAG) {
        // integer
        print("{d}", .{x >> FIXNUM_SHIFT});
    } else if ((x & CHAR_MASK) == CHAR_TAG) {
        // character
        const c = @truncate(u8, @intCast(u64, x >> CHAR_SHIFT));
        switch (c) {
            0 => print("#\\nul", .{}),
            ' ' => print("#\\space", .{}),
            '\n' => print("#\\newline", .{}),
            else => print("#\\{c}", .{c}),
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
