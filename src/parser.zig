const Token = @import("./lexer.zig").Token;
const Lexer = @import("./lexer.zig").Lexer;
const std = @import("std");
const gpa = std.heap.GeneralPurposeAllocator;

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

pub fn parse(lexer: *Lexer) error{InvalidSyntax}!?AstNode {
    const curr_token = lexer.*.next() orelse return null;
    const root = try switch (curr_token) {
        .ifStart => AstNode{ .ifNode = try parseIfNode(lexer) },
        .lambda => AstNode{ .lambda = try parseLambdaNode(lexer) },
        .blockStart => AstNode{ .apl = try parseAplNode(lexer) },
        .trueVal,
        .falseVal,
        .nat,
        .variable,
        .pred,
        .suc,
        .iszero,
        => AstNode{ .literal = curr_token },
        else => error.InvalidSyntax,
    };
    return root;
}

fn parseIfNode(lexer: *Lexer) error{InvalidSyntax}!IfNode {
    const p = try parse(lexer) orelse return error.InvalidSyntax;

    var next = lexer.next() orelse return error.InvalidSyntax;
    if (next != .ifThen) return error.InvalidSyntax;
    const t = (try parse(lexer)) orelse return error.InvalidSyntax;

    next = lexer.next() orelse return error.InvalidSyntax;
    if (next != .ifElse) return error.InvalidSyntax;
    const o = (try parse(lexer)) orelse return error.InvalidSyntax;

    next = lexer.next() orelse return error.InvalidSyntax;
    return if (next == .ifEnd)
        error.InvalidSyntax
    else
        IfNode{ .p = &p, .then = &t, .otherwise = &o };
}

fn parseTypeNode(lexer: *Lexer) error{InvalidSyntax}!TypeNode {
    if (lexer.next()) |tk| {
        switch (tk) {
            .natType, .boolType => return TypeNode.initBase(tk),
            .blockStart => {
                const in = try parseTypeNode(lexer);
                var next = lexer.next() orelse return error.InvalidSyntax;

                if (next != .arrow) return error.InvalidSyntax;
                const out = try parseTypeNode(lexer);
                next = lexer.next() orelse return error.InvalidSyntax;
                if (next != .blockEnd) return error.InvalidSyntax;

                return TypeNode.initArrow(&in, &out);
            },
            else => return error.InvalidSyntax,
        }
    } else return error.InvalidSyntax;
}

fn parseLambdaNode(lexer: *Lexer) error{InvalidSyntax}!LambdaNode {
    const var_name = if (lexer.next()) |tk| name: {
        switch (tk) {
            .variable => |name| break :name name,
            else => return error.InvalidSyntax,
        }
    } else return error.InvalidSyntax;

    var next = lexer.next() orelse return error.InvalidSyntax;
    if (next != .typeAssignment) return error.InvalidSyntax;
    const ty = try parseTypeNode(lexer);

    next = lexer.next() orelse return error.InvalidSyntax;
    if (next != .dot) return error.InvalidSyntax;
    const body = try parse(lexer) orelse return error.InvalidSyntax;

    next = lexer.next() orelse return error.InvalidSyntax;
    if (next != .absEnd) return error.InvalidSyntax;

    return LambdaNode{
        .var_name = var_name,
        .ty = ty,
        .body = &body,
    };
}

fn parseAplNode(lexer: *Lexer) error{InvalidSyntax}!AplNode {
    const f = try parse(lexer) orelse return error.InvalidSyntax;
    const arg = try parse(lexer) orelse return error.InvalidSyntax;

    const end = lexer.next() orelse return error.InvalidSyntax;
    if (end != .blockEnd) return error.InvalidSyntax;

    return AplNode{
        .f = &f,
        .arg = &arg,
    };
}

test {
    const slice =
        "( ( lambda f : ( Nat -> Bool ) . lambda n : Nat . ( f ( pred n ) ) end end iszero ) 42 )";
    var alloc = gpa(.{}){};
    var lexer = try Lexer.tokenize(slice, alloc.allocator());
    const ast = try parse(&lexer);
    // TODO: Check equality
    _ = ast;
}
