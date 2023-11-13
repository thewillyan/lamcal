const std = @import("std");
const Allocator = std.mem.Allocator;
const Type = @import("types.zig").Type;
const ArrowType = @import("types.zig").FnType;
const expr = @import("expr.zig");

pub const GetTypeError = error{
    InvalidType,
    UnboundVariable,
    OutOfMemory,
};

pub const Context = struct {
    map: std.StringHashMap(std.ArrayList(*const Type)),
    allocator: Allocator,
    types: std.ArrayList(*Type),

    pub fn init(allocator: Allocator) error{OutOfMemory}!Context {
        var types = try std.ArrayList(*Type).initCapacity(allocator, 2);
        errdefer types.deinit();

        var nat_ptr = try allocator.create(Type);
        errdefer allocator.destroy(nat_ptr);
        nat_ptr.* = @as(Type, .nat);
        try types.append(nat_ptr);

        var bool_ptr = try allocator.create(Type);
        errdefer allocator.destroy(bool_ptr);
        bool_ptr.* = @as(Type, .boolean);
        try types.append(bool_ptr);

        const map = std.StringHashMap(std.ArrayList(*const Type)).init(allocator);
        return Context{
            .map = map,
            .allocator = allocator,
            .types = types,
        };
    }

    pub fn deinit(self: *Context) void {
        var iter = self.map.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        self.map.deinit();

        for (self.types.items) |ptr| {
            self.allocator.destroy(ptr);
        }
        self.types.deinit();
    }

    pub fn addVar(self: *Context, name: []const u8, ty: *const Type) error{OutOfMemory}!void {
        var value = try self.map.getOrPut(name);
        if (!value.found_existing) {
            value.value_ptr.* = std.ArrayList(*const Type).init(self.allocator);
        }
        try value.value_ptr.*.append(ty);
    }

    pub fn popVar(self: *Context, name: []const u8) ?*const Type {
        if (self.map.getPtr(name)) |var_stack| {
            return var_stack.*.popOrNull();
        }
        return null;
    }

    pub fn getExprType(self: *Context, expression: *const expr.Expr) GetTypeError!*const Type {
        const ty = switch (expression.*) {
            .ifelse => |*ifexpr| try self.getIfType(ifexpr),
            .variable => |*v| try self.getVarType(v.name),
            .apl => |*aplexpr| try self.getAplType(aplexpr),
            .val => |*v| try self.getValType(v),
        };
        errdefer self.allocator.destroy(ty);
        return ty;
    }

    pub fn getIfType(self: *Context, ifexpr: *const expr.IfExpr) GetTypeError!*const Type {
        const prop_ty = try self.getExprType(ifexpr.prop);
        if (prop_ty.* != .boolean) return error.InvalidType;

        const t_ty = try self.getExprType(ifexpr.t);
        const u_ty = try self.getExprType(ifexpr.u);
        if (!t_ty.eql(u_ty)) return error.InvalidType;

        return t_ty;
    }

    pub fn getVarType(self: *Context, name: []const u8) error{UnboundVariable}!*const Type {
        const stack_ptr = self.map.getPtr(name) orelse
            return error.UnboundVariable;
        return stack_ptr.*.getLastOrNull() orelse return error.UnboundVariable;
    }

    pub fn getAplType(self: *Context, aplexpr: *const expr.AplExpr) GetTypeError!*const Type {
        const f_ty = try self.getExprType(aplexpr.f);

        const fntype = switch (f_ty.*) {
            .fun => |f| f,
            else => return error.InvalidType,
        };

        const in_ty = fntype.in;
        const arg_ty = try self.getExprType(aplexpr.arg);
        if (!arg_ty.eql(in_ty)) return error.InvalidType;

        return fntype.out;
    }

    pub fn getValType(self: *Context, valexpr: *const expr.Val) GetTypeError!*const Type {
        return switch (valexpr.*) {
            .nat => self.natPtr(),
            .boolean => self.booleanPtr(),
            .fun => |*fun| self.getFnType(fun),
        };
    }

    pub fn getFnType(self: *Context, fun: *const expr.Fn) GetTypeError!*const Type {
        const fnty = switch (fun.*) {
            .suc, .pred => ArrowType.init(self.natPtr(), self.natPtr()),
            .iszero => ArrowType.init(self.natPtr(), self.booleanPtr()),
            .abs => |*abs| blk: {
                try self.addVar(abs.v.name, abs.v_type);
                const body_ty = try self.getExprType(abs.term);
                _ = self.popVar(abs.v.name);
                break :blk ArrowType.init(abs.v_type, body_ty);
            },
        };
        const ty = try self.createTypePtr(fnty.intoType());
        errdefer self.allocator.destroy(ty);
        return ty;
    }

    pub fn natPtr(self: *Context) *const Type {
        return self.types.items[0];
    }

    pub fn booleanPtr(self: *Context) *const Type {
        return self.types.items[1];
    }

    pub fn createTypePtr(self: *Context, ty: Type) error{OutOfMemory}!*const Type {
        return switch (ty) {
            .nat => self.natPtr(),
            .boolean => self.booleanPtr(),
            .fun => blk: {
                var ty_ptr = try self.allocator.create(Type);
                errdefer self.allocator.destroy(ty_ptr);
                ty_ptr.* = ty;
                try self.types.append(ty_ptr);
                break :blk ty_ptr;
            },
        };
    }
};

test "get expr type test" {
    const Lexer = @import("lexer.zig").Lexer;
    const Parser = @import("parser.zig").Parser;

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

    const ty = try context.getExprType(&exp);
    try std.testing.expect(ty.* == .boolean);
}
