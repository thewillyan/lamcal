const std = @import("std");
const alloc = std.testing.allocator;
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;

const expr = @import("expr.zig");
const Fn = expr.Fn;
const Expr = expr.Expr;
const Val = expr.Val;

pub const Type = union(enum) {
    boolean,
    nat,
    fun: FnType,

    pub fn natPtr(allocator: Allocator) error{OutOfMemory}!*Type {
        var nat = try allocator.create(Type);
        errdefer allocator.destroy(nat);
        nat.* = @as(Type, .nat);
        return nat;
    }

    pub fn boolPtr(allocator: Allocator) error{OutOfMemory}!*Type {
        var boolean = try allocator.create(Type);
        errdefer allocator.destroy(boolean);
        boolean.* = @as(Type, .boolean);
        return boolean;
    }

    pub fn eql(self: *const Type, other: *const Type) bool {
        return switch (self.*) {
            .boolean => if (other.* == .boolean) true else false,
            .nat => if (other.* == .nat) true else false,
            .fun => |f1_type| switch (other.*) {
                .fun => |*f2_type| f1_type.eql(f2_type),
                else => false,
            },
        };
    }

    pub fn format(
        self: Type,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try switch (self) {
            .nat => writer.print("Nat", .{}),
            .boolean => writer.print("Bool", .{}),
            .fun => |fnty| fnty.format(fmt, options, writer),
        };
    }
};

pub const FnType = struct {
    in: *const Type,
    out: *const Type,

    pub fn init(in: *const Type, out: *const Type) FnType {
        return FnType{
            .in = in,
            .out = out,
        };
    }

    pub fn intoType(self: FnType) Type {
        return Type{ .fun = self };
    }

    pub fn eql(self: *const FnType, other: *const FnType) bool {
        return (self.in.*.eql(other.in) and self.out.*.eql(other.out));
    }

    pub fn format(
        self: FnType,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("( {s} -> {s} )", .{ self.in.*, self.out.* });
    }
};

test "type equality test" {
    var n: Type = Type.nat;
    var b: Type = Type.boolean;
    var f1 = FnType.init(&n, &b).intoType();
    var f2 = FnType.init(&n, &n).intoType();
    var f3 = FnType.init(&b, &b).intoType();
    // equal
    try expect(b.eql(&b));
    try expect(n.eql(&n));
    try expect(f1.eql(&f1));
    try expect(f2.eql(&f2));
    try expect(f3.eql(&f3));
    // not equal
    try expect(!b.eql(&n));
    try expect(!b.eql(&f1));
    try expect(!b.eql(&f2));
    try expect(!b.eql(&f3));

    try expect(!n.eql(&b));
    try expect(!n.eql(&f1));
    try expect(!n.eql(&f2));
    try expect(!n.eql(&f3));

    try expect(!f1.eql(&b));
    try expect(!f1.eql(&n));
    try expect(!f1.eql(&f2));
    try expect(!f1.eql(&f3));

    try expect(!f2.eql(&b));
    try expect(!f2.eql(&n));
    try expect(!f2.eql(&f1));
    try expect(!f2.eql(&f3));

    try expect(!f3.eql(&b));
    try expect(!f3.eql(&n));
    try expect(!f3.eql(&f1));
    try expect(!f3.eql(&f2));
}
