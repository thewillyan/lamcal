const std = @import("std");
const lamcal = @import("lamcal");
const Lexer = lamcal.lexer.Lexer;
const Parser = lamcal.parser.Parser;
const Context = lamcal.context.Context;

fn getInputLine(reader: anytype, allocator: std.mem.Allocator) !?[]const u8 {
    const line = (try reader.readUntilDelimiterOrEofAlloc(
        allocator,
        '\n',
        std.math.maxInt(usize),
    )) orelse return null;

    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    } else {
        return line;
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    const stdin = std.io.getStdIn();
    defer stdin.close();
    const stdout = std.io.getStdOut();
    defer stdout.close();

    const input = (try getInputLine(stdin.reader(), gpa.allocator())) orelse {
        try stdout.writer().print("!", .{});
        std.os.exit(1);
    };

    var lexer = Lexer.tokenize(input, gpa.allocator()) catch |err| {
        switch (err) {
            error.InvalidSlice => {
                try stdout.writer().print("!", .{});
                std.os.exit(1);
            },
            else => return err,
        }
    };
    defer lexer.deinit();

    var context = try Context.init(gpa.allocator());
    defer context.deinit();

    var parser = Parser.init(gpa.allocator());

    var exp = parser.parseTokens(&lexer, &context) catch |err| {
        switch (err) {
            error.InvalidSyntax => {
                try stdout.writer().print("!", .{});
                std.os.exit(1);
            },
            else => return err,
        }
    };
    defer exp.deinit(gpa.allocator());

    const ty = context.getExprType(&exp) catch |err| {
        switch (err) {
            error.InvalidType, error.UnboundVariable => {
                try stdout.writer().print("-", .{});
                std.os.exit(1);
            },
            else => return err,
        }
    };

    try stdout.writer().print("{s}", .{ty});
}
