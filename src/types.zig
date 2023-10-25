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
    const in: Type = Type.nat;
    const out: Type = Type.boolean;
    const t = FnType.init(&in, &out).intoType();
    // verify types
    try std.testing.expect(in == .nat);
    try std.testing.expect(out == .boolean);
    try std.testing.expect(t == .fun);
}

test "type equality test" {
    const n: Type = Type.nat;
    const b: Type = Type.boolean;
    const f1 = FnType.init(&n, &b);
    const f2 = FnType.init(&n, &n);
    // equal
    try expect(b.eq(&b));
    try expect(n.eq(&n));
    try expect(f1.eq(&f1));
    try expect(f2.eq(&f2));
    // not equal
    try expect(!b.eq(&n));
    try expect(!n.eq(&b));
    try expect(!f1.eq(&f2));
    try expect(!f2.eq(&f1));
}
