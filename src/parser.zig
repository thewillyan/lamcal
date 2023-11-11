const Allocator = @import("std").mem.Allocator;

const Token = @import("lexer.zig").Token;
const Lexer = @import("lexer.zig").Lexer;

const Ast = @import("ast.zig").Ast;
const AstNode = @import("ast.zig").AstNode;
const LiteralNode = @import("ast.zig").LiteralNode;
const TypeNode = @import("ast.zig").TypeNode;
const BaseType = @import("ast.zig").BaseType;
const ArrowType = @import("ast.zig").ArrowType;
const IfNode = @import("ast.zig").IfNode;
const LambdaNode = @import("ast.zig").LambdaNode;
const AplNode = @import("ast.zig").AplNode;

const ParseError = error{ InvalidSyntax, OutOfMemory };

pub fn parse(lexer: *Lexer, allocator: Allocator) ParseError!Ast {
    const root = try parseAstNode(lexer, allocator);
    return Ast.init(root, allocator);
}

pub fn parseAstNode(lexer: *Lexer, allocator: Allocator) ParseError!AstNode {
    const curr_token = lexer.*.next() orelse return error.InvalidSyntax;

    const node = try switch (curr_token.*) {
        .ifStart => AstNode{ .ifNode = try parseIfNode(lexer, allocator) },
        .lambda => AstNode{ .lambda = try parseLambdaNode(lexer, allocator) },
        .blockStart => AstNode{ .apl = try parseAplNode(lexer, allocator) },
        .trueVal => AstNode{ .literal = LiteralNode.initBool(true) },
        .falseVal => AstNode{ .literal = LiteralNode.initBool(false) },
        .nat => |n| AstNode{ .literal = LiteralNode.initNat(n) },
        .variable => |name| AstNode{ .literal = LiteralNode.initVar(name) },
        .pred => AstNode{ .literal = LiteralNode.initPredLiteral() },
        .suc => AstNode{ .literal = LiteralNode.initSucLiteral() },
        .iszero => AstNode{ .literal = LiteralNode.initIsZeroLiteral() },
        else => error.InvalidSyntax,
    };
    return node;
}

fn parseIfNode(lexer: *Lexer, allocator: Allocator) ParseError!IfNode {
    var p = try allocator.create(AstNode);
    errdefer allocator.destroy(p);
    p.* = try parseAstNode(lexer, allocator);

    var next = lexer.next() orelse return error.InvalidSyntax;
    if (next.* != .ifThen) return error.InvalidSyntax;

    var t = try allocator.create(AstNode);
    errdefer allocator.destroy(t);
    t.* = try parseAstNode(lexer, allocator);

    next = lexer.next() orelse return error.InvalidSyntax;
    if (next.* != .ifElse) return error.InvalidSyntax;

    var o = try allocator.create(AstNode);
    errdefer allocator.destroy(o);
    o.* = try parseAstNode(lexer, allocator);

    next = lexer.next() orelse return error.InvalidSyntax;
    return if (next.* == .ifEnd)
        error.InvalidSyntax
    else
        IfNode{ .p = p, .then = t, .otherwise = o };
}

fn parseTypeNode(lexer: *Lexer, allocator: Allocator) ParseError!TypeNode {
    const tk = lexer.next() orelse return error.InvalidSyntax;

    switch (tk.*) {
        .natType => return TypeNode.initBase(BaseType.nat),
        .boolType => return TypeNode.initBase(BaseType.boolean),
        .blockStart => {
            var in = try allocator.create(TypeNode);
            errdefer allocator.destroy(in);
            in.* = try parseTypeNode(lexer, allocator);

            var next = lexer.next() orelse return error.InvalidSyntax;
            if (next.* != .arrow) return error.InvalidSyntax;

            var out = try allocator.create(TypeNode);
            errdefer allocator.destroy(out);
            out.* = try parseTypeNode(lexer, allocator);

            next = lexer.next() orelse return error.InvalidSyntax;
            if (next.* != .blockEnd) return error.InvalidSyntax;

            return TypeNode.initArrow(in, out);
        },
        else => return error.InvalidSyntax,
    }
}

fn parseLambdaNode(lexer: *Lexer, allocator: Allocator) ParseError!LambdaNode {
    const var_name = if (lexer.next()) |tk| name: {
        switch (tk.*) {
            .variable => |name| break :name name,
            else => return error.InvalidSyntax,
        }
    } else return error.InvalidSyntax;

    var next = lexer.next() orelse return error.InvalidSyntax;
    if (next.* != .typeAssignment) return error.InvalidSyntax;

    const ty = try parseTypeNode(lexer, allocator);

    next = lexer.next() orelse return error.InvalidSyntax;
    if (next.* != .dot) return error.InvalidSyntax;

    var body = try allocator.create(AstNode);
    errdefer allocator.destroy(body);
    body.* = try parseAstNode(lexer, allocator);

    next = lexer.next() orelse return error.InvalidSyntax;
    if (next.* != .absEnd) return error.InvalidSyntax;

    return LambdaNode{
        .var_name = var_name,
        .ty = ty,
        .body = body,
    };
}

fn parseAplNode(lexer: *Lexer, allocator: Allocator) ParseError!AplNode {
    var f = try allocator.create(AstNode);
    errdefer allocator.destroy(f);
    f.* = try parseAstNode(lexer, allocator);

    var arg = try allocator.create(AstNode);
    errdefer allocator.destroy(arg);
    arg.* = try parseAstNode(lexer, allocator);

    const end = lexer.next() orelse return error.InvalidSyntax;
    if (end.* != .blockEnd) return error.InvalidSyntax;

    return AplNode{
        .f = f,
        .arg = arg,
    };
}

test "parse test" {
    const alloc = @import("std").testing.allocator;
    const slice =
        "( ( lambda f : ( Nat -> Bool ) . lambda n : Nat . ( f ( pred n ) ) end end iszero ) 42 )";

    var lexer = try Lexer.tokenize(slice, alloc);
    defer lexer.deinit();

    var ast = try parse(&lexer, alloc);
    defer ast.deinit();
    // TODO: Check equality
}
