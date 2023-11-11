//! Provides lambda calculus expressions utilities.
const std = @import("std");
const alloc = std.testing.allocator;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const expectError = std.testing.expectError;
const types = @import("types.zig");
const Type = types.Type;
const FnType = types.FnType;

const TypeError = error{ InvalidType, OutOfMemory };

/// Lambda calculus expression.
pub const Expr = union(enum) {
    ifelse: IfExpr,
    variable: Var,
    apl: AplExpr,
    val: Val,

    pub fn getType(self: *Expr, allocator: Allocator) TypeError!Type {
        return switch (self.*) {
            .ifelse => |exp| exp.getType(allocator),
            .variable => |exp| exp.getType(),
            .apl => |exp| exp.getType(allocator),
            .val => |*exp| exp.getType(allocator),
        };
    }

    pub fn deinit(self: *Expr, allocator: Allocator) void {
        switch (self.*) {
            .ifelse => |*ifexpr| ifexpr.deinit(allocator),
            .variable => |*v| v.deinit(allocator),
            .apl => |*aplexpr| aplexpr.deinit(allocator),
            .val => |*v| v.deinit(allocator),
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
    ty: *Type,

    pub fn init(name: []const u8, ty: *Type) Var {
        return Var{ .name = name, .ty = ty };
    }

    pub fn deinit(self: *Var, allocator: Allocator) void {
        self.ty.deinit(allocator);
        // allocator.destroy(self.ty);
    }

    pub fn intoExpr(self: Var) Expr {
        return Expr{ .variable = self };
    }

    pub fn getType(self: *const Var) Type {
        return self.ty.*;
    }

    pub fn getTypePtr(self: *Var) *Type {
        return self.ty;
    }

    pub fn eql(self: *const Var, other: *const Var) bool {
        return (std.mem.eql(u8, self.name, other.name)) and
            (self.ty.eql(other.ty));
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

    pub fn getType(self: *Val, allocator: Allocator) TypeError!Type {
        return switch (self.*) {
            .nat => Type.nat,
            .boolean => Type.boolean,
            .fun => |*f| f.getType(allocator),
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

    pub fn getType(self: *Fn, allocator: Allocator) TypeError!Type {
        switch (self.*) {
            .suc, .pred => {
                var nat_in = try Type.natPtr(allocator);
                errdefer allocator.destroy(nat_in);
                var nat_out = try Type.natPtr(allocator);
                errdefer allocator.destroy(nat_out);
                return FnType.init(nat_in, nat_out).intoType();
            },
            .iszero => {
                var nat_in = try Type.natPtr(allocator);
                errdefer allocator.destroy(nat_in);
                var boolean_out = try Type.boolPtr(allocator);
                errdefer allocator.destroy(boolean_out);
                return FnType.init(nat_in, boolean_out).intoType();
            },
            .abs => |*abs| return abs.getType(allocator),
        }
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
    term: *Expr,

    pub fn init(v: Var, term: *Expr) Abs {
        return Abs{ .v = v, .term = term };
    }

    pub fn deinit(self: *Abs, allocator: Allocator) void {
        self.v.deinit(alloc);
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

    pub fn getType(self: *Abs, allocator: Allocator) TypeError!Type {
        var t = self.v.getTypePtr();

        var u = try allocator.create(Type);
        errdefer allocator.destroy(u);
        u.* = try self.term.getType(allocator);

        return FnType.init(t, u).intoType();
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

    pub fn getType(self: *const IfExpr, allocator: Allocator) TypeError!Type {
        const prop_type = try self.prop.*.getType(allocator);
        const t_type = try self.t.*.getType(allocator);
        const u_type = try self.u.*.getType(allocator);
        if (prop_type == .boolean and t_type.eql(&u_type)) {
            return t_type;
        }
        return error.InvalidType;
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

    pub fn getType(self: *const AplExpr, allocator: Allocator) TypeError!Type {
        var arg_type = try self.arg.*.getType(allocator);
        defer arg_type.deinit(alloc);

        var f = try self.f.*.getType(allocator);
        defer f.deinit(alloc);

        const f_type = switch (f) {
            .fun => |t| t,
            else => return error.InvalidType,
        };

        if (f_type.in.*.eql(&arg_type)) {
            return f_type.out.*;
        }
        return error.InvalidType;
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
    var nat_ptr = try Type.natPtr(alloc);
    defer alloc.destroy(nat_ptr);
    var x = Var.init("x", nat_ptr);
    defer x.deinit(alloc);
    try expect(std.mem.eql(u8, x.name, "x"));
    try expect(x.ty.* == Type.nat);
}
test "variable equality test" {
    var x1 = Var.init("x", try Type.natPtr(alloc));
    defer alloc.destroy(x1.ty);
    defer x1.deinit(alloc);
    var x2 = Var.init("x", try Type.natPtr(alloc));
    defer alloc.destroy(x2.ty);
    defer x2.deinit(alloc);
    var x3 = Var.init("x", try Type.boolPtr(alloc));
    defer alloc.destroy(x3.ty);
    defer x3.deinit(alloc);
    var y = Var.init("y", try Type.natPtr(alloc));
    defer alloc.destroy(y.ty);
    defer y.deinit(alloc);

    try expect(x1.eql(&x2));
    try expect(x2.eql(&x1));

    try expect(!x1.eql(&x3));
    try expect(!x3.eql(&x1));
    try expect(!x1.eql(&y));
    try expect(!y.eql(&x1));
}
test "abstraction value test" {
    var x = Var.init("x", try Type.natPtr(alloc));
    defer alloc.destroy(x.ty);
    defer x.deinit(alloc);

    var x_expr = x.intoExpr();
    const expr = Abs.init(x, &x_expr).intoExpr();
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

test "nat value type test" {
    var n = Val.initNat(2).intoExpr();
    var ty = try n.getType(alloc);
    defer ty.deinit(alloc);
    try expect(ty == Type.nat);
}

test "boolean type test" {
    var b = Val.initBoolean(false).intoExpr();
    var ty = try b.getType(alloc);
    defer ty.deinit(alloc);
    try expect(ty == Type.boolean);
}

test "function type test" {
    // create types
    var nat_nat = FnType.init(try Type.natPtr(alloc), try Type.natPtr(alloc))
        .intoType();
    defer nat_nat.deinit(alloc);
    var nat_bool = FnType.init(try Type.natPtr(alloc), try Type.boolPtr(alloc))
        .intoType();
    defer nat_bool.deinit(alloc);

    // testing
    var suc = @as(Fn, Fn.suc).intoExpr();
    var suc_ty = try suc.getType(alloc);
    defer suc_ty.deinit(alloc);
    try expect(suc_ty.eql(&nat_nat));

    var pred = @as(Fn, Fn.pred).intoExpr();
    var pred_ty = try pred.getType(alloc);
    defer pred_ty.deinit(alloc);
    try expect(pred_ty.eql(&nat_nat));

    var iszero = @as(Fn, Fn.iszero).intoExpr();
    var iszero_ty = try iszero.getType(alloc);
    defer iszero_ty.deinit(alloc);

    try expect(iszero_ty.eql(&nat_bool));
}

test "abstraction type test" {
    var x_nat = Var.init("x", try Type.natPtr(alloc));

    var x_nat_expr = x_nat.intoExpr();
    // defer x_nat_expr.deinit(alloc);
    var abs1 = Abs.init(x_nat, &x_nat_expr).intoExpr();
    // defer abs1.deinit(alloc);
    var abs1_ty = try abs1.getType(alloc);
    defer abs1_ty.deinit(alloc);

    var expected_abs1_ty =
        FnType.init(try Type.natPtr(alloc), try Type.natPtr(alloc)).intoType();
    defer expected_abs1_ty.deinit(alloc);
    try expect(abs1_ty.eql(&expected_abs1_ty));

    var two = Val.initBoolean(true).intoExpr();
    var abs2 = Abs.init(Var.init("x", try Type.natPtr(alloc)), &two).intoExpr();
    var abs2_ty = try abs2.getType(alloc);
    defer abs2_ty.deinit(alloc);

    var expected_abs2_ty =
        FnType.init(try Type.natPtr(alloc), try Type.boolPtr(alloc)).intoType();
    defer expected_abs2_ty.deinit(alloc);
    try expect(abs2_ty.eql(&expected_abs2_ty));
}

test "ifexpr type test" {
    var nat = @as(Type, Type.nat);
    var prop = Val.initBoolean(true).intoExpr();
    var two = Val.initNat(2).intoExpr();
    var four = Val.initNat(4).intoExpr();
    var falseVal = Val.initBoolean(false).intoExpr();

    var if1 = IfExpr.init(&prop, &two, &four);
    var if1_ty = try if1.getType(alloc);
    defer if1_ty.deinit(alloc);
    try expect(if1_ty.eql(&nat));

    var if2 = IfExpr.init(&prop, &two, &falseVal);
    try expectError(error.InvalidType, if2.getType(alloc));
}

test "aplexpr type test" {
    var nat = @as(Type, Type.nat);
    var boolean = @as(Type, Type.boolean);

    var iszero = @as(Fn, Fn.iszero).intoExpr();
    var suc = @as(Fn, Fn.suc).intoExpr();

    var four = Val.initNat(4).intoExpr();
    var zero = Val.initNat(0).intoExpr();
    var falseVal = Val.initBoolean(false).intoExpr();
    var trueVal = Val.initBoolean(true).intoExpr();

    var apl1 = AplExpr.init(&iszero, &four).intoExpr();
    var apl1_ty = try apl1.getType(alloc);
    defer apl1_ty.deinit(alloc);
    try expect(apl1_ty.eql(&boolean));

    var apl2 = AplExpr.init(&iszero, &falseVal)
        .intoExpr();
    try expectError(error.InvalidType, apl2.getType(alloc));

    var apl3 = AplExpr.init(&suc, &zero)
        .intoExpr();
    var apl3_ty = try apl3.getType(alloc);
    defer apl3_ty.deinit(alloc);
    try expect(apl3_ty.eql(&nat));

    var apl4 = AplExpr.init(&suc, &trueVal)
        .intoExpr();
    try expectError(error.InvalidType, apl4.getType(alloc));
}

test "nested expr test" {
    // construct expression: (lambda x: Nat. iszero x)
    var iszero = @as(Fn, Fn.iszero).intoExpr();
    var x = Var.init("x", try Type.natPtr(alloc));
    var x_expr = x.intoExpr();
    var apl = AplExpr.init(&iszero, &x_expr).intoExpr();
    var abs = Abs.init(x, &apl).intoExpr();

    var abs_ty = try abs.getType(alloc);
    defer abs_ty.deinit(alloc);

    // Nat -> Boolean
    var expected_type = FnType.init(try Type.natPtr(alloc), try Type.boolPtr(alloc))
        .intoType();
    defer expected_type.deinit(alloc);
    try expect(abs_ty.eql(&expected_type));
}
