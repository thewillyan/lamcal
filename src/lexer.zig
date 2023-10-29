const std = @import("std");
const gpa = std.heap.GeneralPurposeAllocator;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

fn isAlphanumeric(slice: []const u8) bool {
    return for (slice) |c| {
        if (!std.ascii.isAlphanumeric(c)) break false;
    } else true;
}

pub const Token = union(enum) {
    trueVal,
    falseVal,
    nat: u32,
    boolType,
    natType,
    arrow,
    dot,
    ifStart,
    ifThen,
    ifElse,
    ifEnd,
    suc,
    pred,
    iszero,
    lambda,
    // a block start with '(' and ends with ')'
    blockStart,
    blockEnd,
    typeAssignment,
    variable: []const u8,

    pub fn tryFromSlice(str: []const u8) !Token {
        const slice = std.mem.trim(u8, str, " ");

        const parse_res: ?u32 = std.fmt.parseInt(u32, slice, 10) catch null;
        if (parse_res) |n| return Token{ .nat = n };

        const token: anyerror!Token = if (std.mem.eql(u8, slice, "true"))
            .trueVal
        else if (std.mem.eql(u8, slice, "false"))
            .falseVal
        else if (std.mem.eql(u8, slice, "Bool"))
            .boolType
        else if (std.mem.eql(u8, slice, "Nat"))
            .natType
        else if (std.mem.eql(u8, slice, "->"))
            .arrow
        else if (std.mem.eql(u8, slice, "."))
            .dot
        else if (std.mem.eql(u8, slice, "if"))
            .ifStart
        else if (std.mem.eql(u8, slice, "then"))
            .ifThen
        else if (std.mem.eql(u8, slice, "else"))
            .ifElse
        else if (std.mem.eql(u8, slice, "endif"))
            .ifEnd
        else if (std.mem.eql(u8, slice, "suc"))
            .suc
        else if (std.mem.eql(u8, slice, "pred"))
            .pred
        else if (std.mem.eql(u8, slice, "iszero"))
            .iszero
        else if (std.mem.eql(u8, slice, "lambda"))
            .lambda
        else if (std.mem.eql(u8, slice, "("))
            .blockStart
        else if (std.mem.eql(u8, slice, ")"))
            .blockEnd
        else if (std.mem.eql(u8, slice, ":"))
            .typeAssignment
        else if (isAlphanumeric(slice))
            Token{ .variable = slice }
        else
            error.InvalidSlice;
        return token;
    }

    pub fn eql(self: *const Token, other: *const Token) bool {
        return switch (self.*) {
            .trueVal => (other.* == .trueVal),
            .falseVal => (other.* == .falseVal),
            .boolType => (other.* == .boolType),
            .natType => (other.* == .natType),
            .arrow => (other.* == .arrow),
            .dot => (other.* == .dot),
            .ifStart => (other.* == .ifStart),
            .ifThen => (other.* == .ifThen),
            .ifElse => (other.* == .ifElse),
            .ifEnd => (other.* == .ifEnd),
            .suc => (other.* == .suc),
            .pred => (other.* == .pred),
            .iszero => (other.* == .iszero),
            .lambda => (other.* == .lambda),
            .blockStart => (other.* == .blockStart),
            .blockEnd => (other.* == .blockEnd),
            .typeAssignment => (other.* == .typeAssignment),
            .nat => |n| switch (other.*) {
                .nat => |m| (n == m),
                else => false,
            },
            .variable => |x| switch (other.*) {
                .variable => |y| std.mem.eql(u8, x, y),
                else => false,
            },
        };
    }
};

pub const Lexer = struct {
    tokens: ArrayList(Token),
    index: usize,

    pub fn tokenize(str: []const u8, allocator: Allocator) !Lexer {
        var slices = std.mem.split(u8, std.mem.trim(u8, str, " "), " ");
        var token_list = try ArrayList(Token)
            .initCapacity(allocator, slices.buffer.len);
        errdefer token_list.deinit();

        while (slices.next()) |slice| {
            const token = try Token.tryFromSlice(slice);
            try token_list.append(token);
        }
        return Lexer{ .tokens = token_list, .index = 0 };
    }

    pub fn next(self: *Lexer) ?Token {
        if (self.*.index < self.*.tokens.items.len) {
            defer self.*.index += 1;
            return self.*.tokens.items[self.*.index];
        } else {
            return null;
        }
    }

    pub fn deinit(self: Lexer) void {
        self.tokens.deinit();
    }
};

test "lexer test" {
    const slice =
        "( ( lambda f : Nat -> Bool . ( lambda n : Nat . f ( pred n ) ) ) iszero ) 42";
    const expected_tokens = [_]Token{
        .blockStart,              .blockStart,     .lambda,
        Token{ .variable = "f" }, .typeAssignment, .natType,
        .arrow,                   .boolType,       .dot,
        .blockStart,              .lambda,         Token{ .variable = "n" },
        .typeAssignment,          .natType,        .dot,
        Token{ .variable = "f" }, .blockStart,     .pred,
        Token{ .variable = "n" }, .blockEnd,       .blockEnd,
        .blockEnd,                .iszero,         .blockEnd,
        Token{ .nat = 42 },
    };

    var alloc = gpa(.{}){};
    var lexer = try Lexer.tokenize(slice, alloc.allocator());
    defer lexer.deinit();
    var i: usize = 0;
    while (lexer.next()) |*token| : (i += 1) {
        try std.testing.expect(expected_tokens[i].eql(token));
    }
}
