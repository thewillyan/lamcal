const std = @import("std");

// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lamcal = b.createModule(
        .{ .source_file = .{ .path = "src/modules/lamcal.zig" } },
    );

    const main_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/modules/lamcal.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_main_tests = b.addRunArtifact(main_tests);
    const test_step = b.step("test", "Run library tests");

    test_step.dependOn(&run_main_tests.step);

    // add executable
    const get_type = b.addExecutable(.{
        .name = "get-type",
        .root_source_file = .{ .path = "src/exe/get_type.zig" },
        .target = target,
        .optimize = optimize,
    });
    get_type.addModule("lamcal", lamcal);
    b.installArtifact(get_type);

    const run_get_type_cmd = b.addRunArtifact(get_type);
    run_get_type_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_get_type_cmd.addArgs(args);
    }

    const run_get_type_step = b.step("get-type", "Run \"get type\" tool");
    run_get_type_step.dependOn(&run_get_type_cmd.step);
}
