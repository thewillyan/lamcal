const std = @import("std");
const Fn = @import("expr.zig").Fn;

const Type = union(enum) { boolean, nat, fun: FnType };

const FnType = struct {
    in: *const Type,
    out: *const Type,
};

test "types test" {
    const in = Type{ .nat = {} };
    const out = Type{ .boolean = {} };
    const t = Type{ .fun = FnType{ .in = &in, .out = &out } };
    try std.testing.expect(t == .fun);
}
