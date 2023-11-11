const std = @import("std");
const Allocator = std.mem.Allocator;
const Type = @import("types.zig").Type;

pub const Context = struct {
    map: std.StringHashMap(std.ArrayList(*Type)),
    allocator: Allocator,

    pub fn init(allocator: Allocator) Context {
        const map = std.StringHashMap(std.ArrayList(*Type)).init(allocator);
        return Context{ .map = map, .allocator = allocator };
    }

    pub fn deinit(self: *Context) void {
        var iter = self.map.iterator();
        while (iter.next()) |entry| {
            for (entry.value_ptr.*.items) |ty_ptr| {
                ty_ptr.*.deinit(self.allocator);
                self.allocator.destroy(ty_ptr);
            }
            entry.value_ptr.*.deinit();
        }
        self.map.deinit();
    }

    pub fn addVar(self: *Context, name: []const u8, ty: Type) error{OutOfMemory}!*Type {
        var ty_ptr = try self.allocator.create(Type);
        errdefer self.allocator.destroy(ty_ptr);
        ty_ptr.* = ty;
        errdefer ty_ptr.deinit(self.allocator);

        var value = try self.map.getOrPut(name);
        if (!value.found_existing) {
            value.value_ptr.* = std.ArrayList(*Type).init(self.allocator);
        }
        try value.value_ptr.*.append(ty_ptr);
        return value.value_ptr.*.getLast();
    }

    pub fn popVar(self: *Context, name: []const u8) void {
        if (self.map.getPtr(name)) |var_stack| {
            if (var_stack.*.popOrNull()) |ty_ptr| {
                ty_ptr.*.deinit(self.allocator);
                self.allocator.destroy(ty_ptr);
            }
        }
    }

    pub fn getVarType(self: *Context, name: []const u8) error{UnboundVariable}!*Type {
        const stack_ptr = self.map.getPtr(name) orelse
            return error.UnboundVariable;
        return stack_ptr.*.getLastOrNull() orelse return error.UnboundVariable;
    }
};

test "context test" {}
