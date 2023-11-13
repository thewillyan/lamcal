//! Provides lambda calculus expressions utilities.
const std = @import("std");
const alloc = std.testing.allocator;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const expectError = std.testing.expectError;
const Type = @import("types.zig").Type;

/// Lambda calculus expression.
pub const Expr = union(enum) {
    ifelse: IfExpr,
    variable: Var,
    apl: AplExpr,
    val: Val,

    pub fn deinit(self: *Expr, allocator: Allocator) void {
        switch (self.*) {
            .ifelse => |*ifexpr| ifexpr.deinit(allocator),
            .apl => |*aplexpr| aplexpr.deinit(allocator),
            .val => |*v| v.deinit(allocator),
            .variable => {},
        }
    }

    pub fn eql(self: *const Expr, other: *const Expr) bool {
        return switch (self.*) {
            .ifelse => |if1| switch (other.*) {
                .ifelse => |if2| if1.eql(&if2),
                else => false,
            },
            .variable => |x| switch (other.*) {
                .variable => |y| x.eql(&y),
                else => false,
            },
            .apl => |apl1| switch (other.*) {
                .apl => |apl2| apl1.eql(&apl2),
                else => false,
            },
            .val => |v1| switch (other.*) {
                .val => |v2| v1.eql(&v2),
                else => false,
            },
        };
    }
};

/// Variable
pub const Var = struct {
    name: []const u8,

    pub fn init(name: []const u8) Var {
        return Var{ .name = name };
    }

    pub fn intoExpr(self: Var) Expr {
        return Expr{ .variable = self };
    }

    pub fn eql(self: *const Var, other: *const Var) bool {
        return (std.mem.eql(u8, self.name, other.name));
    }
};

/// Lambda calculus values.
pub const Val = union(enum) {
    nat: u32,
    boolean: bool,
    fun: Fn,

    pub fn initNat(n: u32) Val {
        return Val{ .nat = n };
    }

    pub fn initBoolean(b: bool) Val {
        return Val{ .boolean = b };
    }

    pub fn deinit(self: *Val, allocator: Allocator) void {
        switch (self.*) {
            .fun => |*fun| fun.deinit(allocator),
            else => {},
        }
    }

    pub fn intoExpr(self: Val) Expr {
        return Expr{ .val = self };
    }

    pub fn eql(self: *const Val, other: *const Val) bool {
        return switch (self.*) {
            .nat => |n1| switch (other.*) {
                .nat => |n2| n1 == n2,
                else => false,
            },
            .boolean => |b1| switch (other.*) {
                .boolean => |b2| b1 == b2,
                else => false,
            },
            .fun => |f1| switch (other.*) {
                .fun => |f2| f1.eql(&f2),
                else => false,
            },
        };
    }
};

/// Function values
pub const Fn = union(enum) {
    suc,
    pred,
    iszero,
    abs: Abs,

    pub fn intoValue(self: Fn) Val {
        return Val{ .fun = self };
    }

    pub fn intoExpr(self: Fn) Expr {
        return Expr{ .val = self.intoValue() };
    }

    pub fn eql(self: *const Fn, other: *const Fn) bool {
        return switch (self.*) {
            .suc => if (other.* == .suc) true else false,
            .pred => if (other.* == .pred) true else false,
            .iszero => if (other.* == .iszero) true else false,
            .abs => |abs1| switch (other.*) {
                .abs => |abs2| abs1.eql(&abs2),
                else => false,
            },
        };
    }

    pub fn deinit(self: *Fn, allocator: Allocator) void {
        switch (self.*) {
            .abs => |*abs| abs.deinit(allocator),
            else => {},
        }
    }
};

/// Abstraction value
pub const Abs = struct {
    v: Var,
    v_type: *const Type,
    term: *Expr,

    pub fn init(v: Var, v_type: *const Type, term: *Expr) Abs {
        return Abs{ .v = v, .v_type = v_type, .term = term };
    }

    pub fn deinit(self: *Abs, allocator: Allocator) void {
        self.term.deinit(allocator);
        allocator.destroy(self.term);
    }

    pub fn intoValue(self: Abs) Val {
        return Val{
            .fun = Fn{ .abs = self },
        };
    }

    pub fn intoExpr(self: Abs) Expr {
        return Expr{
            .val = Val{
                .fun = Fn{ .abs = self },
            },
        };
    }

    pub fn eql(self: *const Abs, other: *const Abs) bool {
        return (self.*.v.eql(&other.*.v)) and
            (self.*.term.eql(other.*.term));
    }
};

/// If expression.
pub const IfExpr = struct {
    prop: *Expr,
    t: *Expr,
    u: *Expr,

    pub fn init(prop: *Expr, then: *Expr, otherwise: *Expr) IfExpr {
        return IfExpr{
            .prop = prop,
            .t = then,
            .u = otherwise,
        };
    }

    pub fn deinit(self: *IfExpr, allocator: Allocator) void {
        self.prop.deinit(allocator);
        self.t.deinit(allocator);
        self.u.deinit(allocator);

        allocator.destroy(self.prop);
        allocator.destroy(self.t);
        allocator.destroy(self.u);
    }

    pub fn intoExpr(self: IfExpr) Expr {
        return Expr{ .ifelse = self };
    }

    pub fn eql(self: *const IfExpr, other: *const IfExpr) bool {
        return (self.*.prop.*.eql(other.*.prop)) and
            (self.*.t.*.eql(other.*.t)) and
            (self.*.u.*.eql(other.*.u));
    }
};

/// Function aplication expression.
pub const AplExpr = struct {
    f: *Expr,
    arg: *Expr,

    pub fn init(f: *Expr, arg: *Expr) AplExpr {
        return AplExpr{ .f = f, .arg = arg };
    }

    pub fn deinit(self: *AplExpr, allocator: Allocator) void {
        self.f.deinit(allocator);
        self.arg.deinit(allocator);

        allocator.destroy(self.f);
        allocator.destroy(self.arg);
    }

    pub fn intoExpr(self: AplExpr) Expr {
        return Expr{ .apl = self };
    }

    pub fn eql(self: *const AplExpr, other: *const AplExpr) bool {
        return (self.*.f.*.eql(other.*.f)) and
            (self.*.arg.*.eql(other.*.arg));
    }
};

test "nat value test" {
    const expr = Val.initNat(2).intoExpr();
    switch (expr) {
        .val => |n| switch (n) {
            .nat => |v| try expect(v == 2),
            else => return error.InvalidValueType,
        },
        else => return error.InvalidExprType,
    }
}
test "boolean value test" {
    const expr = Val.initBoolean(true).intoExpr();
    switch (expr) {
        .val => |b| switch (b) {
            .boolean => |v| try expect(v),
            else => return error.InvalidValueType,
        },
        else => return error.InvalidExprType,
    }
}
test "variable test" {
    var x = Var.init("x");
    try expect(std.mem.eql(u8, x.name, "x"));
}
test "variable equality test" {
    var x1 = Var.init("x");
    var y = Var.init("y");

    try expect(!x1.eql(&y));
    try expect(!y.eql(&x1));
}

test "abstraction value test" {
    var x = Var.init("x");
    var nat = @as(Type, .nat);

    var x_expr = x.intoExpr();
    const expr = Abs.init(x, &nat, &x_expr).intoExpr();
    switch (expr) {
        .val => |v| switch (v) {
            .fun => |f| switch (f) {
                .abs => |a| {
                    try expect(a.v.eql(&x));
                    switch (a.term.*) {
                        .variable => |y| try expect(x.eql(&y)),
                        else => return error.InvalidTermType,
                    }
                },
                else => return error.InvalidFunctionType,
            },
            else => return error.InvalidValueType,
        },
        else => return error.InvalidExprType,
    }
}
test "ifexpr test" {
    var b = Val.initBoolean(true).intoExpr();
    var two = Val.initNat(2).intoExpr();
    var four = Val.initNat(4).intoExpr();
    var expr = IfExpr.init(&b, &two, &four).intoExpr();
    switch (expr) {
        .ifelse => |*ifexpr| {
            try expect(ifexpr.prop.eql(&b));
            try expect(ifexpr.t.eql(&two));
            try expect(ifexpr.u.eql(&four));
        },
        else => return error.InvalidExprType,
    }
}

test "aplexpr test" {
    var suc = @as(Fn, Fn.suc).intoExpr();
    var one = Val.initNat(1).intoExpr();
    var apl = AplExpr.init(&suc, &one).intoExpr();
    switch (apl) {
        .apl => |a| {
            try expect(a.f.*.eql(&suc));
            try expect(a.arg.*.eql(&one));
        },
        else => return error.InvalidExprType,
    }
}
