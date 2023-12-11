const Allocator = @import("std").mem.Allocator;

const expr = @import("expr.zig");
const Token = @import("lexer.zig").Token;
const Lexer = @import("lexer.zig").Lexer;
const Context = @import("context.zig").Context;
const Type = @import("types.zig").Type;
const FnType = @import("types.zig").FnType;

const ParseError = error{ InvalidSyntax, OutOfMemory };

pub const Parser = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) Parser {
        return Parser{ .allocator = allocator };
    }

    /// Parse a lambda calculus expression from an [Lexer](lexer.zig)
    pub fn parse(self: *Parser, lexer: *Lexer, context: *Context) ParseError!expr.Expr {
        const final_expr = try self.parseTokens(lexer, context);
        return if (lexer.next() == null) final_expr else error.InvalidSyntax;
    }

    pub fn parseTokens(self: *Parser, lexer: *Lexer, context: *Context) ParseError!expr.Expr {
        const curr_token = lexer.*.next() orelse return error.InvalidSyntax;

        const exp = try switch (curr_token.*) {
            .ifStart => (try self.parseIfTokens(lexer, context)).intoExpr(),
            .lambda => (try self.parseLambdaTokens(lexer, context)).intoExpr(),
            .blockStart => (try self.parseAplTokens(lexer, context)).intoExpr(),
            .trueVal => expr.Val.initBoolean(true).intoExpr(),
            .falseVal => expr.Val.initBoolean(false).intoExpr(),
            .nat => |n| expr.Val.initNat(n).intoExpr(),
            .variable => |name| expr.Var.init(name).intoExpr(),
            .pred => @as(expr.Fn, .pred).intoExpr(),
            .suc => @as(expr.Fn, .suc).intoExpr(),
            .iszero => @as(expr.Fn, .iszero).intoExpr(),
            else => error.InvalidSyntax,
        };
        // errdefer exp.deinit(self.allocator);
        return exp;
    }

    fn parseIfTokens(self: *Parser, lexer: *Lexer, context: *Context) ParseError!expr.IfExpr {
        var prop = try self.allocator.create(expr.Expr);
        errdefer self.allocator.destroy(prop);
        prop.* = try self.parseTokens(lexer, context);
        errdefer prop.deinit(self.allocator);

        var next = lexer.next() orelse return error.InvalidSyntax;
        if (next.* != .ifThen) return error.InvalidSyntax;

        var t = try self.allocator.create(expr.Expr);
        errdefer self.allocator.destroy(t);
        t.* = try self.parseTokens(lexer, context);
        errdefer t.deinit(self.allocator);

        next = lexer.next() orelse return error.InvalidSyntax;
        if (next.* != .ifElse) return error.InvalidSyntax;

        var u = try self.allocator.create(expr.Expr);
        errdefer self.allocator.destroy(u);
        u.* = try self.parseTokens(lexer, context);
        errdefer u.deinit(self.allocator);

        next = lexer.next() orelse return error.InvalidSyntax;
        return if (next.* != .ifEnd)
            error.InvalidSyntax
        else
            expr.IfExpr.init(prop, t, u);
    }

    fn parseTypeTokens(self: *Parser, lexer: *Lexer, context: *Context) ParseError!*const Type {
        const tk = lexer.next() orelse return error.InvalidSyntax;

        return switch (tk.*) {
            .natType => context.natPtr(),
            .boolType => context.booleanPtr(),
            .blockStart => blk: {
                const in = try self.parseTypeTokens(lexer, context);
                errdefer self.allocator.destroy(in);

                var next = lexer.next() orelse return error.InvalidSyntax;
                if (next.* != .arrow) return error.InvalidSyntax;

                const out = try self.parseTypeTokens(lexer, context);
                errdefer self.allocator.destroy(out);

                next = lexer.next() orelse return error.InvalidSyntax;
                if (next.* != .blockEnd) return error.InvalidSyntax;

                var ty_ptr = try context.createTypePtr(FnType.init(in, out).intoType());
                errdefer self.allocator.destroy(ty_ptr);
                break :blk ty_ptr;
            },
            else => return error.InvalidSyntax,
        };
    }

    fn parseLambdaTokens(self: *Parser, lexer: *Lexer, context: *Context) ParseError!expr.Abs {
        const v = if (lexer.next()) |tk| name: {
            switch (tk.*) {
                .variable => |name| break :name expr.Var.init(name),
                else => return error.InvalidSyntax,
            }
        } else return error.InvalidSyntax;

        var next = lexer.next() orelse return error.InvalidSyntax;
        if (next.* != .typeAssignment) return error.InvalidSyntax;

        const ty = try self.parseTypeTokens(lexer, context);

        next = lexer.next() orelse return error.InvalidSyntax;
        if (next.* != .dot) return error.InvalidSyntax;

        var term = try self.allocator.create(expr.Expr);
        errdefer self.allocator.destroy(term);
        term.* = try self.parseTokens(lexer, context);
        errdefer term.deinit(self.allocator);

        next = lexer.next() orelse return error.InvalidSyntax;
        if (next.* != .absEnd) return error.InvalidSyntax;

        return expr.Abs.init(v, ty, term);
    }

    fn parseAplTokens(self: *Parser, lexer: *Lexer, context: *Context) ParseError!expr.AplExpr {
        var f = try self.allocator.create(expr.Expr);
        errdefer self.allocator.destroy(f);
        f.* = try self.parseTokens(lexer, context);
        errdefer f.deinit(self.allocator);

        var arg = try self.allocator.create(expr.Expr);
        errdefer self.allocator.destroy(arg);
        arg.* = try self.parseTokens(lexer, context);
        errdefer arg.deinit(self.allocator);

        const end = lexer.next() orelse return error.InvalidSyntax;
        if (end.* != .blockEnd) return error.InvalidSyntax;

        return expr.AplExpr.init(f, arg);
    }
};

test "parse test" {
    const alloc = @import("std").testing.allocator;
    const slice =
        "( ( lambda f : ( Nat -> Bool ) . lambda n : Nat . if ( iszero n ) then ( f n ) else ( f ( pred n ) ) endif end end iszero ) 42 )";

    var lexer = try Lexer.tokenize(slice, alloc);
    defer lexer.deinit();

    var context = try Context.init(alloc);
    defer context.deinit();

    var parser = Parser.init(alloc);

    var exp = try parser.parseTokens(&lexer, &context);
    defer exp.deinit(alloc);
}
