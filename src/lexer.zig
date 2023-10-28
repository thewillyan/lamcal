const std = @import("std");
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
    ifEnd,
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
        else if (std.mem.eql(u8, slice, "endif"))
            .ifEnd
        else if (std.mem.eql(u8, slice, "lambda"))
            .lambda
        else if (std.mem.eql(u8, slice, "("))
            .blockStart
        else if (std.mem.eql(u8, slice, ")"))
            .blockEnd
        else if (std.mem.eql(u8, slice, ":"))
            .typeAssignment
        else if (isAlphanumeric(slice))
            .variable
        else
            error.InvalidTokenSlice;
        return token;
    }
};

pub const Lexer = struct {
    tokens: ArrayList(Token),
    index: usize,

    pub fn tokenize(str: []const u8, allocator: Allocator) Lexer {
        const slices = std.mem.split(u8, std.mem.trim(u8, str, " "), " ");
        var token_list = try ArrayList(Token)
            .initCapacity(allocator, slices.buffer.len);
        errdefer token_list.deinit();

        for (slices) |slice| {
            token_list.append(Token.tryFromSlice(slice));
        }
        return Lexer{ .tokens = token_list };
    }

    pub fn next(self: *Lexer) ?Token {
        defer self.*.index += 1;
        if (self.*.index < self.*.tokens.items.len) {
            const i = self.*.index;
            self.*.index += 1;
            return &self.*.tokens.items[i];
        } else {
            return null;
        }
    }
};
