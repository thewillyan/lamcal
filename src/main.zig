const std = @import("std");
pub const expr = @import("expr.zig");
pub const types = @import("types.zig");

test {
    // run all tests
    std.testing.refAllDecls(expr);
    std.testing.refAllDecls(types);
}
