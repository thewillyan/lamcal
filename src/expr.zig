//! Provides lambda calculus expressions utilities.

const std = @import("std");
const expect = std.testing.expect;
const types = @import("types.zig");
const Type = types.Type;
const FnType = types.FnType;

/// Lambda calculus expression.
pub const Expr = union(enum) {
    ifelse: IfExpr,
    variable: Var,
    apl: AplExpr,
    val: Val,

    pub fn getType(self: *const Expr) ?Type {
        return switch (self.*) {
            .ifelse => |exp| exp.getType(),
            .variable => |exp| exp.getType(),
            .apl => |exp| exp.getType(),
            .val => |exp| exp.getType(),
        };
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
    ty: Type,

    pub fn init(name: []const u8, ty: Type) Var {
        return Var{ .name = name, .ty = ty };
    }

    pub fn intoExpr(self: Var) Expr {
        return Expr{ .variable = self };
    }

    pub fn getType(self: *const Var) ?Type {
        return self.ty;
    }

    pub fn eql(self: *const Var, other: *const Var) bool {
        return (std.mem.eql(u8, self.name, other.name)) and
            (self.ty.eql(&other.ty));
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

    pub fn intoExpr(self: Val) Expr {
        return Expr{ .val = self };
    }

    pub fn getType(self: *const Val) ?Type {
        return switch (self.*) {
            .nat => Type.nat,
            .boolean => Type.boolean,
            .fun => |f| f.getType(),
        };
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

    pub fn getType(self: *const Fn) ?Type {
        const nat = @as(Type, Type.nat);
        return switch (self.*) {
            .suc => FnType.init(&nat, &nat).intoType(),
            .pred => FnType.init(&nat, &nat).intoType(),
            .iszero => FnType.init(&nat, &@as(Type, Type.boolean)).intoType(),
            .abs => |abs| abs.getType(),
        };
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
};

/// Abstraction value
pub const Abs = struct {
    v: Var,
    term: *const Expr,

    pub fn init(v: Var, term: *const Expr) Abs {
        return Abs{ .v = v, .term = term };
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

    pub fn getType(self: *const Abs) ?Type {
        const t = self.v.getType().?;
        const u = self.term.getType() orelse return null;
        return FnType.init(&t, &u).intoType();
    }

    pub fn eql(self: *const Abs, other: *const Abs) bool {
        return (self.*.v.eql(&other.*.v)) and
            (self.*.term.eql(other.*.term));
    }
};

/// If expression.
pub const IfExpr = struct {
    prop: *const Expr,
    t: *const Expr,
    u: *const Expr,

    pub fn init(prop: *const Expr, then: *const Expr, otherwise: *const Expr) IfExpr {
        return IfExpr{
            .prop = prop,
            .t = then,
            .u = otherwise,
        };
    }

    pub fn intoExpr(self: IfExpr) Expr {
        return Expr{ .ifelse = self };
    }

    pub fn getType(self: *const IfExpr) ?Type {
        const prop_type = self.prop.*.getType() orelse return null;
        const t_type = self.t.*.getType() orelse return null;
        const u_type = self.u.*.getType() orelse return null;
        if (prop_type == .boolean and t_type.eql(&u_type)) {
            return t_type;
        }
        return null;
    }

    pub fn eql(self: *const IfExpr, other: *const IfExpr) bool {
        return (self.*.prop.*.eql(other.*.prop)) and
            (self.*.t.*.eql(other.*.t)) and
            (self.*.u.*.eql(other.*.u));
    }
};

/// Function aplication expression.
pub const AplExpr = struct {
    f: *const Expr,
    arg: *const Expr,

    pub fn init(f: *const Expr, arg: *const Expr) AplExpr {
        return AplExpr{ .f = f, .arg = arg };
    }

    pub fn intoExpr(self: AplExpr) Expr {
        return Expr{ .apl = self };
    }

    pub fn getType(self: *const AplExpr) ?Type {
        const arg_type = self.arg.*.getType() orelse return null;
        const f = self.f.*.getType() orelse return null;
        const f_type = switch (f) {
            .fun => |t| t,
            else => return null,
        };
        if (f_type.in.*.eql(&arg_type)) {
            return f_type.out.*;
        }
        return null;
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
    const x = Var.init("x", @as(Type, Type.nat));
    try expect(std.mem.eql(u8, x.name, "x"));
    try expect(x.ty == Type.nat);
}
test "variable equality test" {
    const x1 = Var.init("x", @as(Type, Type.nat));
    const x2 = Var.init("x", @as(Type, Type.nat));
    const x3 = Var.init("x", @as(Type, Type.boolean));
    const y = Var.init("y", @as(Type, Type.nat));
    try expect(x1.eql(&x2));
    try expect(x2.eql(&x1));

    try expect(!x1.eql(&x3));
    try expect(!x3.eql(&x1));
    try expect(!x1.eql(&y));
    try expect(!y.eql(&x1));
}
test "abstraction value test" {
    const x = Var.init("x", @as(Type, Type.nat));
    const expr = Abs.init(x, &x.intoExpr()).intoExpr();
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
    const b = Val.initBoolean(true).intoExpr();
    const two = Val.initNat(2).intoExpr();
    const four = Val.initNat(4).intoExpr();
    const expr = IfExpr.init(&b, &two, &four).intoExpr();
    switch (expr) {
        .ifelse => |ifexpr| {
            try expect(ifexpr.prop.eql(&b));
            try expect(ifexpr.t.eql(&two));
            try expect(ifexpr.u.eql(&four));
        },
        else => return error.InvalidExprType,
    }
}
test "aplexpr test" {
    const suc = @as(Fn, Fn.suc).intoExpr();
    const one = Val.initNat(1).intoExpr();
    const apl = AplExpr.init(&suc, &one).intoExpr();
    switch (apl) {
        .apl => |a| {
            try expect(a.f.*.eql(&suc));
            try expect(a.arg.*.eql(&one));
        },
        else => return error.InvalidExprType,
    }
}

test "nat value type test" {
    const n = Val.initNat(2).intoExpr();
    if (n.getType()) |t| {
        try expect(t == Type.nat);
    } else {
        return error.UnexpectedNullValue;
    }
}
test "boolean type test" {
    const b = Val.initBoolean(false).intoExpr();
    if (b.getType()) |t| {
        try expect(t == Type.boolean);
    } else {
        return error.UnexpectedNullValue;
    }
}
test "function type test" {
    // create types
    const nat = @as(Type, Type.nat);
    const boolean = @as(Type, Type.boolean);
    const nat_nat = FnType.init(&nat, &nat).intoType();
    const nat_bool = FnType.init(&nat, &boolean).intoType();

    // testing
    const suc = @as(Fn, Fn.suc).intoExpr();
    try expect(suc.getType().?.eql(&nat_nat));
    const pred = @as(Fn, Fn.pred).intoExpr();
    try expect(pred.getType().?.eql(&nat_nat));
    const iszero = @as(Fn, Fn.iszero).intoExpr();
    try expect(iszero.getType().?.eql(&nat_bool));
}

test "abstraction type test" {
    // create types
    const nat = @as(Type, Type.nat);
    const boolean = @as(Type, Type.boolean);
    const nat_nat = FnType.init(&nat, &nat).intoType();
    const nat_bool = FnType.init(&nat, &boolean).intoType();

    // testig
    const x_nat = Var.init("x", @as(Type, Type.nat));
    const abs1 = Abs.init(x_nat, &x_nat.intoExpr()).intoExpr();
    try expect(abs1.getType().?.eql(&nat_nat));
    const abs2 = Abs.init(x_nat, &Val.initBoolean(true).intoExpr()).intoExpr();
    try expect(abs2.getType().?.eql(&nat_bool));
}
test "ifexpr type test" {
    const nat = @as(Type, Type.nat);

    const if1 = IfExpr.init(
        &Val.initBoolean(true).intoExpr(),
        &Val.initNat(2).intoExpr(),
        &Val.initNat(4).intoExpr(),
    );
    try expect(if1.getType().?.eql(&nat));

    const if2 = IfExpr.init(
        &Val.initBoolean(true).intoExpr(),
        &Val.initNat(2).intoExpr(),
        &Val.initBoolean(false).intoExpr(),
    );
    try expect(if2.getType() == null);
}
test "aplexpr type test" {
    const nat = @as(Type, Type.nat);
    const boolean = @as(Type, Type.boolean);
    const iszero = @as(Fn, Fn.iszero);
    const suc = @as(Fn, Fn.suc);

    const apl1 = AplExpr.init(&iszero.intoExpr(), &Val.initNat(4).intoExpr())
        .intoExpr();
    try expect(apl1.getType().?.eql(&boolean));

    const apl2 = AplExpr.init(&iszero.intoExpr(), &Val.initBoolean(false).intoExpr())
        .intoExpr();
    try expect(apl2.getType() == null);

    const apl3 = AplExpr.init(&suc.intoExpr(), &Val.initNat(0).intoExpr())
        .intoExpr();
    try expect(apl3.getType().?.eql(&nat));

    const apl4 = AplExpr.init(&suc.intoExpr(), &Val.initBoolean(true).intoExpr())
        .intoExpr();
    try expect(apl4.getType() == null);
}
test "nested expr test" {
    // construct expression: (lambda x: Nat. iszero x)
    const iszero = @as(Fn, Fn.iszero).intoExpr();
    const x = Var.init("x", @as(Type, Type.nat));
    const apl = AplExpr.init(&iszero, &x.intoExpr()).intoExpr();
    const abs = Abs.init(x, &apl).intoExpr();

    // Nat -> Boolean
    const expected_type = FnType.init(&@as(Type, .nat), &@as(Type, .boolean))
        .intoType();
    try expect(abs.getType().?.eql(&expected_type));
}
