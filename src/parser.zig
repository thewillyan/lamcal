const Token = @import("lexer.zig").Token;
const Lexer = @import("lexer.zig").Lexer;

const AstNode = @import("ast.zig").AstNode;
const LiteralNode = @import("ast.zig").LiteralNode;
const TypeNode = @import("ast.zig").TypeNode;
const BaseType = @import("ast.zig").BaseType;
const ArrowType = @import("ast.zig").ArrowType;
const IfNode = @import("ast.zig").IfNode;
const LambdaNode = @import("ast.zig").LambdaNode;
const AplNode = @import("ast.zig").AplNode;

pub fn parse(lexer: *Lexer) error{InvalidSyntax}!?AstNode {
    const curr_token = lexer.*.next() orelse return null;
    const root = try switch (curr_token) {
        .ifStart => AstNode{ .ifNode = try parseIfNode(lexer) },
        .lambda => AstNode{ .lambda = try parseLambdaNode(lexer) },
        .blockStart => AstNode{ .apl = try parseAplNode(lexer) },
        .trueVal => AstNode{ .literal = LiteralNode.initBool(true) },
        .falseVal => AstNode{ .literal = LiteralNode.initBool(false) },
        .nat => |n| AstNode{ .literal = LiteralNode.initNat(n) },
        .variable => |name| AstNode{ .literal = LiteralNode.initVar(name) },
        .pred => AstNode{ .literal = LiteralNode.initPredLiteral() },
        .suc => AstNode{ .literal = LiteralNode.initSucLiteral() },
        .iszero => AstNode{ .literal = LiteralNode.initIsZeroLiteral() },
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
            .natType => return TypeNode.initBase(BaseType.nat),
            .boolType => return TypeNode.initBase(BaseType.boolean),
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
    const gpa = @import("std").heap.GeneralPurposeAllocator;
    const slice =
        "( ( lambda f : ( Nat -> Bool ) . lambda n : Nat . ( f ( pred n ) ) end end iszero ) 42 )";
    var alloc = gpa(.{}){};
    var lexer = try Lexer.tokenize(slice, alloc.allocator());
    const ast = try parse(&lexer);
    // TODO: Check equality
    _ = ast;
}
