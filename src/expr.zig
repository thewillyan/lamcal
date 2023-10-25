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
        if (prop_type == .boolean and t_type.eq(&u_type)) {
            return t_type;
        }
        return null;
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
        if (f_type.in.*.eq(&arg_type)) {
            return f_type.out.*;
        }
        return null;
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
};

/// Abstraction value
pub const Abs = struct {
    v: Var,
    term: *const Expr,

    pub fn init(v: Var, term: *const Expr) Abs {
        return Abs{ .v = v, .term = term };
    }

    pub fn intoExpr(self: Abs) Expr {
        Expr{ .abs = self };
    }

    pub fn getType(self: *const Abs) ?Type {
        const t = self.v.getType().?;
        const u = self.term.getType() orelse return null;
        return FnType.init(&t, &u).intoType();
    }
};

test "expr test" {
    // create expression:
    // if(iszero 8) then 42 else 8 endif
    const x = Val.initNat(8).intoExpr();
    const y = Val.initNat(42).intoExpr();
    const apl_iszero = AplExpr.init(&@as(Fn, Fn.iszero).intoExpr(), &x).intoExpr();
    const if_expr = IfExpr.init(&apl_iszero, &y, &x).intoExpr();
    try expect(if_expr == .ifelse);
}

test "expr type test" {
    const two = Val.initNat(2).intoExpr();
    const apl = AplExpr.init(&@as(Fn, Fn.suc).intoExpr(), &two);
    try expect(apl.getType() != null);
}
