const std = @import("std");
pub const expr = @import("expr.zig");

test {
    // run all tests
    std.testing.refAllDecls(expr);
}
