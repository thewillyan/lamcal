const std = @import("std");
const Allocator = std.mem.Allocator;

const Expr = @import("expr.zig").Expr;
const Val = @import("expr.zig").Val;
const Fn = @import("expr.zig").Fn;
const Var = @import("expr.zig").Var;
const IfExpr = @import("expr.zig").IfExpr;
const Abs = @import("expr.zig").Abs;
const AplExpr = @import("expr.zig").AplExpr;
const Type = @import("types.zig").Type;
const FnType = @import("types.zig").FnType;

pub const AstParseError = error{ UnboundVariable, OutOfMemory };

pub const Ast = struct {
    root: AstNode,
    allocator: Allocator,

    pub fn init(root: AstNode, allocator: Allocator) Ast {
        return Ast{ .root = root, .allocator = allocator };
    }

    pub fn intoExpr(self: *const Ast, context: anytype) AstParseError!Expr {
        return self.root.intoExpr(context, self.allocator);
    }

    pub fn deinit(self: *Ast) void {
        self.root.deinit(self.allocator);
    }
};

pub const AstNode = union(enum) {
    ifNode: IfNode,
    lambda: LambdaNode,
    apl: AplNode,
    literal: LiteralNode,

    pub fn intoExpr(self: AstNode, context: anytype, allocator: Allocator) AstParseError!Expr {
        return switch (self) {
            .ifNode => |ifnode| ifnode.intoExpr(context, allocator),
            .lambda => |lambda| lambda.intoExpr(context, allocator),
            .apl => |apl| apl.intoExpr(context, allocator),
            .literal => |lit| lit.intoExpr(context),
        };
    }

    pub fn deinit(self: *AstNode, allocator: Allocator) void {
        switch (self.*) {
            .ifNode => |*ifnode| ifnode.deinit(allocator),
            .lambda => |*lambda| lambda.deinit(allocator),
            .apl => |*apl| apl.deinit(allocator),
            .literal => {},
        }
    }
};

pub const LiteralNode = union(enum) {
    nat: u32,
    boolean: bool,
    variable: []const u8,
    literal_fn: LiteralFn,

    pub fn initNat(n: u32) LiteralNode {
        return LiteralNode{ .nat = n };
    }

    pub fn initBool(b: bool) LiteralNode {
        return LiteralNode{ .boolean = b };
    }

    pub fn initVar(name: []const u8) LiteralNode {
        return LiteralNode{ .variable = name };
    }

    pub fn initSucLiteral() LiteralNode {
        return LiteralNode{ .literal_fn = LiteralFn.suc };
    }

    pub fn initPredLiteral() LiteralNode {
        return LiteralNode{ .literal_fn = LiteralFn.pred };
    }

    pub fn initIsZeroLiteral() LiteralNode {
        return LiteralNode{ .literal_fn = LiteralFn.is_zero };
    }

    pub fn intoExpr(self: LiteralNode, context: anytype) AstParseError!Expr {
        return switch (self) {
            .nat => |n| Val.initNat(n).intoExpr(),
            .boolean => |b| Val.initBoolean(b).intoExpr(),
            .variable => |name| varblk: {
                const stack_ptr = context.getPtr(name) orelse
                    return error.UnboundVariable;
                const var_ty = stack_ptr.*.getLastOrNull() orelse
                    return error.UnboundVariable;
                break :varblk Var.init(name, var_ty).intoExpr();
            },
            .literal_fn => |lit_fn| switch (lit_fn) {
                .suc => @as(Fn, .suc).intoExpr(),
                .pred => @as(Fn, .pred).intoExpr(),
                .is_zero => @as(Fn, .iszero).intoExpr(),
            },
        };
    }
};

pub const LiteralFn = enum { suc, pred, is_zero };

pub const TypeNode = union(enum) {
    base: BaseType,
    arrow: ArrowType,

    pub fn initBase(tk: BaseType) TypeNode {
        return TypeNode{ .base = tk };
    }

    pub fn initArrow(in: *TypeNode, out: *TypeNode) TypeNode {
        return TypeNode{ .arrow = ArrowType{
            .in = in,
            .out = out,
        } };
    }

    pub fn intoType(self: TypeNode, allocator: Allocator) error{OutOfMemory}!Type {
        switch (self) {
            .base => |ty| return switch (ty) {
                .nat => @as(Type, .nat),
                .boolean => @as(Type, .boolean),
            },
            .arrow => |arrow| return (try arrow.intoFnType(allocator)).intoType(),
        }
    }

    pub fn deinit(self: *TypeNode, allocator: Allocator) void {
        switch (self.*) {
            .base => {},
            .arrow => |*arrow| arrow.deinit(allocator),
        }
    }
};

pub const BaseType = enum { nat, boolean };

pub const ArrowType = struct {
    in: *TypeNode,
    out: *TypeNode,

    pub fn intoFnType(self: ArrowType, allocator: Allocator) error{OutOfMemory}!FnType {
        var in = try allocator.create(Type);
        errdefer allocator.destroy(in);
        in.* = try self.in.intoType(allocator);

        var out = try allocator.create(Type);
        errdefer allocator.destroy(out);
        out.* = try self.out.intoType(allocator);

        return FnType.init(in, out);
    }

    pub fn deinit(self: *ArrowType, allocator: Allocator) void {
        self.in.deinit(allocator);
        self.out.deinit(allocator);
        allocator.destroy(self.in);
        allocator.destroy(self.out);
    }
};

pub const IfNode = struct {
    p: *AstNode,
    then: *AstNode,
    otherwise: *AstNode,

    pub fn intoExpr(self: IfNode, context: anytype, allocator: Allocator) AstParseError!Expr {
        var p = try allocator.create(Expr);
        errdefer allocator.destroy(p);
        p.* = try self.p.intoExpr(context, allocator);
        errdefer p.deinit(allocator);

        var then = try allocator.create(Expr);
        errdefer allocator.destroy(then);
        then.* = try self.then.intoExpr(context, allocator);
        errdefer then.deinit(allocator);

        var otherwise = try allocator.create(Expr);
        errdefer allocator.destroy(otherwise);
        otherwise.* = try self.otherwise.intoExpr(context, allocator);
        errdefer then.deinit(allocator);

        return IfExpr.init(p, then, otherwise).intoExpr();
    }

    pub fn deinit(self: *IfNode, allocator: Allocator) void {
        self.p.deinit(allocator);
        self.then.deinit(allocator);
        self.otherwise.deinit(allocator);

        allocator.destroy(self.p);
        allocator.destroy(self.then);
        allocator.destroy(self.otherwise);
    }
};

pub const LambdaNode = struct {
    var_name: []const u8,
    ty: TypeNode,
    body: *AstNode,

    pub fn intoExpr(self: LambdaNode, context: anytype, allocator: Allocator) AstParseError!Expr {
        // add variable to the context.
        var ty = try allocator.create(Type);
        errdefer allocator.destroy(ty);
        ty.* = try self.ty.intoType(allocator);

        var value = try context.getOrPut(self.var_name);
        if (!value.found_existing) {
            value.value_ptr.* = std.ArrayList(*Type).init(allocator);
        }
        try value.value_ptr.*.append(ty);
        defer _ = value.value_ptr.*.pop();
        const ty_ptr = value.value_ptr.*.getLast();
        defer allocator.destroy(ty_ptr);
        defer ty_ptr.*.deinit(allocator);

        var body = try allocator.create(Expr);
        errdefer allocator.destroy(body);
        body.* = try self.body.intoExpr(context, allocator);
        errdefer body.deinit(allocator);

        return Abs.init(
            Var.init(self.var_name, ty_ptr),
            body,
        ).intoExpr();
    }

    pub fn deinit(self: *LambdaNode, allocator: Allocator) void {
        self.ty.deinit(allocator);
        self.body.deinit(allocator);

        // allocator.free(self.var_namae);
        allocator.destroy(self.body);
    }
};

pub const AplNode = struct {
    f: *AstNode,
    arg: *AstNode,

    pub fn intoExpr(self: AplNode, context: anytype, allocator: Allocator) AstParseError!Expr {
        var f = try allocator.create(Expr);
        errdefer allocator.destroy(f);
        f.* =
            try self.f.intoExpr(context, allocator);
        errdefer self.f.*.deinit(allocator);

        var arg = try allocator.create(Expr);
        errdefer allocator.destroy(arg);
        arg.* = try self.arg.intoExpr(context, allocator);
        errdefer arg.*.deinit(allocator);

        return AplExpr.init(f, arg).intoExpr();
    }

    pub fn deinit(self: *AplNode, allocator: Allocator) void {
        self.f.*.deinit(allocator);
        self.arg.*.deinit(allocator);

        allocator.destroy(self.f);
        allocator.destroy(self.arg);
    }
};

test "AST into expr test" {
    const alloc = std.testing.allocator;
    const strMap = std.StringHashMap;
    const Lexer = @import("lexer.zig").Lexer;
    const parser = @import("parser.zig");

    const slice =
        "( ( lambda f : ( Nat -> Bool ) . lambda n : Nat . ( f ( pred n ) ) end end iszero ) 42 )";
    var lexer = try Lexer.tokenize(slice, alloc);
    defer lexer.deinit();

    var ast = try parser.parse(&lexer, alloc);
    defer ast.deinit();

    var map = strMap(std.ArrayList(*Type)).init(alloc);
    defer map.deinit();
    defer Type.deinitContext(map, alloc);

    var expr = try ast.intoExpr(&map);
    defer expr.deinit(alloc);
}
