const std = @import("std");
pub const expr = @import("expr.zig");
pub const types = @import("types.zig");
pub const lexer = @import("lexer.zig");

test {
    // run all tests
    std.testing.refAllDecls(expr);
    std.testing.refAllDecls(types);
    std.testing.refAllDecls(lexer);
}
