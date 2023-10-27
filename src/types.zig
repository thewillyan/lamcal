const std = @import("std");
const expect = std.testing.expect;
const expr = @import("expr.zig");
const Fn = expr.Fn;
const Expr = expr.Expr;
const Val = expr.Val;

pub const Type = union(enum) {
    boolean,
    nat,
    fun: FnType,

    pub fn eq(self: *const Type, other: *const Type) bool {
        return switch (self.*) {
            .boolean => if (other.* == .boolean) true else false,
            .nat => if (other.* == .nat) true else false,
            .fun => |f1_type| switch (other.*) {
                .fun => |*f2_type| f1_type.eq(f2_type),
                else => false,
            },
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

    pub fn eq(self: *const FnType, other: *const FnType) bool {
        return (self.in.*.eq(other.in) and self.out.*.eq(other.out));
    }
};

test "types test" {
    const n: Type = Type.nat;
    const b: Type = Type.boolean;
    const f1 = FnType.init(&n, &b).intoType();
    const f2 = FnType.init(&n, &n).intoType();
    const f3 = FnType.init(&b, &b).intoType();
    const f4 = FnType.init(&f2, &f2).intoType();
    // verify types
    try std.testing.expect(n == .nat);
    try std.testing.expect(b == .boolean);
    try std.testing.expect(f1 == .fun);
    try std.testing.expect(f2 == .fun);
    try std.testing.expect(f3 == .fun);
    try std.testing.expect(f4 == .fun);
}

test "type equality test" {
    const n: Type = Type.nat;
    const b: Type = Type.boolean;
    const f1 = FnType.init(&n, &b).intoType();
    const f2 = FnType.init(&n, &n).intoType();
    const f3 = FnType.init(&b, &b).intoType();
    // equal
    try expect(b.eq(&b));
    try expect(n.eq(&n));
    try expect(f1.eq(&f1));
    try expect(f2.eq(&f2));
    try expect(f3.eq(&f3));
    // not equal
    try expect(!b.eq(&n));
    try expect(!b.eq(&f1));
    try expect(!b.eq(&f2));
    try expect(!b.eq(&f3));

    try expect(!n.eq(&b));
    try expect(!n.eq(&f1));
    try expect(!n.eq(&f2));
    try expect(!n.eq(&f3));

    try expect(!f1.eq(&b));
    try expect(!f1.eq(&n));
    try expect(!f1.eq(&f2));
    try expect(!f1.eq(&f3));

    try expect(!f2.eq(&b));
    try expect(!f2.eq(&n));
    try expect(!f2.eq(&f1));
    try expect(!f2.eq(&f3));

    try expect(!f3.eq(&b));
    try expect(!f3.eq(&n));
    try expect(!f3.eq(&f1));
    try expect(!f3.eq(&f2));
}
