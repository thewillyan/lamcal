//! Provides lambda calculus expressions utilities.

const std = @import("std");
const expect = std.testing.expect;

/// Lambda calculus expression.
pub const Expr = union(enum) {
    ifelse: IfExpr,
    variable: Var,
    apl: AplExpr,
    val: Val,
};

/// If expression.
pub const IfExpr = struct {
    prop: *const Expr,
    t1: *const Expr,
    t2: *const Expr,
};

/// Function aplication expression.
pub const AplExpr = struct {
    f: *const Expr,
    arg: *const Expr,
};

/// Variable
pub const Var = struct { name: []const u8, val: ?*const Val };

/// Lambda calculus values.
pub const Val = union(enum) {
    nat: u32,
    boolean: bool,
    func: Fn,
};

/// Function values
pub const Fn = union(enum) {
    suc,
    pred,
    iszero,
    abs: Abs,
};

/// Abstraction value
pub const Abs = struct {
    v: Var,
    term: *const Expr,
};

test "expr test" {
    // create expression:
    // if(iszero 8) then 42 else 8 endif
    const v1 = Expr{ .val = Val{ .nat = 8 } };
    const v2 = Expr{ .val = Val{ .nat = 42 } };
    const iszero = Expr{ .val = Val{ .func = Fn{ .iszero = {} } } };
    const apl_iszero = Expr{ .apl = AplExpr{ .f = &iszero, .arg = &v1 } };
    const if_expr = Expr{ .ifelse = IfExpr{ .prop = &apl_iszero, .t1 = &v2, .t2 = &v1 } };
    try expect(if_expr == .ifelse);
}
