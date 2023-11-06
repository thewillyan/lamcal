const Expr = @import("expr.zig").Expr;

pub const AstNode = union(enum) {
    ifNode: IfNode,
    lambda: LambdaNode,
    apl: AplNode,
    literal: LiteralNode,
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
};

pub const LiteralFn = enum { suc, pred, is_zero };

pub const TypeNode = union(enum) {
    base: BaseType,
    arrow: ArrowType,

    pub fn initBase(tk: BaseType) TypeNode {
        return TypeNode{ .base = tk };
    }

    pub fn initArrow(in: *const TypeNode, out: *const TypeNode) TypeNode {
        return TypeNode{ .arrow = ArrowType{
            .in = in,
            .out = out,
        } };
    }
};

pub const BaseType = enum { nat, boolean };

pub const ArrowType = struct {
    in: *const TypeNode,
    out: *const TypeNode,
};

pub const IfNode = struct {
    p: *const AstNode,
    then: *const AstNode,
    otherwise: *const AstNode,
};

pub const LambdaNode = struct {
    var_name: []const u8,
    ty: TypeNode,
    body: *const AstNode,
};

pub const AplNode = struct {
    f: *const AstNode,
    arg: *const AstNode,
};
