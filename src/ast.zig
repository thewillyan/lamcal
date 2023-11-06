const Token = @import("lexer.zig").Token;

pub const AstNode = union(enum) {
    ifNode: IfNode,
    lambda: LambdaNode,
    apl: AplNode,
    literal: Token,
};

pub const TypeNode = union(enum) {
    base: Token,
    arrow: ArrowType,

    pub fn initBase(tk: Token) TypeNode {
        return TypeNode{ .base = tk };
    }

    pub fn initArrow(in: *const TypeNode, out: *const TypeNode) TypeNode {
        return TypeNode{ .arrow = ArrowType{
            .in = in,
            .out = out,
        } };
    }
};

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
