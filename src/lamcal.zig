const std = @import("std");
pub const expr = @import("expr.zig");
pub const types = @import("types.zig");
pub const lexer = @import("lexer.zig");
pub const ast = @import("ast.zig");
pub const parser = @import("parser.zig");

test {
    // run all tests
    std.testing.refAllDecls(expr);
    std.testing.refAllDecls(types);
    std.testing.refAllDecls(lexer);
    std.testing.refAllDecls(ast);
    std.testing.refAllDecls(parser);
}
