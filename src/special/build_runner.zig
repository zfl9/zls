const root = @import("@build@");
const std = @import("std");
const log = std.log;
const process = std.process;
const Builder = std.build.Builder;
const InstallArtifactStep = std.build.InstallArtifactStep;
const LibExeObjStep = std.build.LibExeObjStep;
const OptionsStep = std.build.OptionsStep;

const StringVoidMap = std.StringArrayHashMapUnmanaged(void);
const StringStringMap = std.StringArrayHashMapUnmanaged([]const u8);

pub const BuildConfig = struct {
    packages: []Pkg,
    include_dirs: []const []const u8,
    c_macros: []const []const u8,

    pub const Pkg = struct {
        name: []const u8,
        path: []const u8,
    };
};

///! This is a modified build runner to extract information out of build.zig
///! Modified version of lib/build_runner.zig
pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    // skip my own exe name
    var arg_idx: usize = 1;

    const zig_exe = nextArg(args, &arg_idx) orelse {
        log.warn("Expected first argument to be path to zig compiler\n", .{});
        return error.InvalidArgs;
    };
    const build_root = nextArg(args, &arg_idx) orelse {
        log.warn("Expected second argument to be build root directory path\n", .{});
        return error.InvalidArgs;
    };
    const cache_root = nextArg(args, &arg_idx) orelse {
        log.warn("Expected third argument to be cache root directory path\n", .{});
        return error.InvalidArgs;
    };
    const global_cache_root = nextArg(args, &arg_idx) orelse {
        log.warn("Expected third argument to be global cache root directory path\n", .{});
        return error.InvalidArgs;
    };

    const builder = try Builder.create(
        allocator,
        zig_exe,
        build_root,
        cache_root,
        global_cache_root,
    );

    defer builder.destroy();

    while (nextArg(args, &arg_idx)) |arg| {
        if (std.mem.startsWith(u8, arg, "-D")) {
            const option_contents = arg[2..];
            if (option_contents.len == 0) {
                log.err("Expected option name after '-D'\n\n", .{});
                return error.InvalidArgs;
            }
            if (std.mem.indexOfScalar(u8, option_contents, '=')) |name_end| {
                const option_name = option_contents[0..name_end];
                const option_value = option_contents[name_end + 1 ..];
                if (try builder.addUserInputOption(option_name, option_value)) {
                    log.err("Option conflict '-D{s}'\n\n", .{option_name});
                    return error.InvalidArgs;
                }
            } else {
                const option_name = option_contents;
                if (try builder.addUserInputFlag(option_name)) {
                    log.err("Option conflict '-D{s}'\n\n", .{option_name});
                    return error.InvalidArgs;
                }
            }
        }
    }

    builder.resolveInstallPrefix(null, Builder.DirList{});
    try runBuild(builder);

    // pkg_name => pkg_path
    var packages: StringStringMap = .{};
    defer packages.deinit(allocator);

    var include_dirs: StringVoidMap = .{};
    defer include_dirs.deinit(allocator);

    // macro_name => macro_value("" means no value)
    var c_macros: StringStringMap = .{};
    defer c_macros.deinit(allocator);

    // This scans the graph of Steps to find all `OptionsStep`s then reifies them
    // Doing this before the loop to find packages ensures their `GeneratedFile`s have been given paths
    for (builder.top_level_steps.items) |tls| {
        for (tls.step.dependencies.items) |step| {
            try reifyOptions(step);
        }
    }

    // TODO: We currently add packages from every LibExeObj step that the install step depends on.
    //       Should we error out or keep one step or something similar?
    // We also flatten them, we should probably keep the nested structure.
    for (builder.top_level_steps.items) |tls| {
        try processStep(allocator, &packages, &include_dirs, &c_macros, &tls.step);
    }

    try std.json.stringify(
        BuildConfig{
            .packages = try getPackageSlice(allocator, packages),
            .include_dirs = include_dirs.keys(),
            .c_macros = try getMacroSlice(allocator, c_macros),
        },
        .{ .whitespace = .{} },
        std.io.getStdOut().writer(),
    );
}

fn getPackageSlice(allocator: std.mem.Allocator, packages: StringStringMap) ![]BuildConfig.Pkg {
    var array = try allocator.alloc(BuildConfig.Pkg, packages.count());

    var i: u32 = 0;
    var it = packages.iterator();
    while (it.next()) |v| {
        array[i].name = v.key_ptr.*;
        array[i].path = v.value_ptr.*;
        i += 1;
    }

    return array;
}

fn getMacroSlice(allocator: std.mem.Allocator, c_macros: StringStringMap) ![]const []const u8 {
    var array = try allocator.alloc([]const u8, c_macros.count());

    var i: u32 = 0;
    var it = c_macros.iterator();
    while (it.next()) |v| {
        const name = v.key_ptr.*;
        const value = v.value_ptr.*;
        if (value.len > 0)
            array[i] = try std.fmt.allocPrint(allocator, "{s}={s}", .{ name, value })
        else
            array[i] = name;
        i += 1;
    }

    return array;
}

fn addPackage(allocator: std.mem.Allocator, packages: *StringStringMap, pkg_name: []const u8, pkg_path: []const u8) !void {
    const v = try packages.getOrPut(allocator, pkg_name);
    if (!v.found_existing)
        v.value_ptr.* = pkg_path;
}

fn addMacro(allocator: std.mem.Allocator, c_macros: *StringStringMap, name_and_value: []const u8) !void {
    const sep = std.mem.indexOfScalar(u8, name_and_value, '=');
    const name = if (sep) |p| name_and_value[0..p] else name_and_value;
    const value = if (sep) |p| name_and_value[p + 1 ..] else "";
    try c_macros.put(allocator, name, value);
}

fn reifyOptions(step: *std.build.Step) anyerror!void {
    // Support Zig 0.9.1
    if (!@hasDecl(OptionsStep, "base_id")) return;

    if (step.cast(OptionsStep)) |option| {
        // We don't know how costly the dependency tree might be, so err on the side of caution
        if (step.dependencies.items.len == 0) {
            try option.step.make();
        }
    }

    for (step.dependencies.items) |unknown_step| {
        try reifyOptions(unknown_step);
    }
}

fn processStep(
    allocator: std.mem.Allocator,
    packages: *StringStringMap,
    include_dirs: *StringVoidMap,
    c_macros: *StringStringMap,
    step: *std.build.Step,
) anyerror!void {
    if (step.cast(InstallArtifactStep)) |install_exe| {
        try processArtifact(allocator, packages, include_dirs, c_macros, install_exe.artifact);
    } else if (step.cast(LibExeObjStep)) |exe| {
        try processArtifact(allocator, packages, include_dirs, c_macros, exe);
    }

    for (step.dependencies.items) |unknown_step| {
        try processStep(allocator, packages, include_dirs, c_macros, unknown_step);
    }
}

fn processArtifact(
    allocator: std.mem.Allocator,
    packages: *StringStringMap,
    include_dirs: *StringVoidMap,
    c_macros: *StringStringMap,
    artifact: *std.build.LibExeObjStep,
) anyerror!void {
    if (artifact.root_src) |src| {
        const maybe_path = switch (src) {
            .path => |path| path,
            .generated => |generated| generated.path,
        };
        if (maybe_path) |path| {
            try addPackage(allocator, packages, "root", path);
        }
    }
    try processMacro(allocator, c_macros, artifact.c_macros.items);
    try processIncludeDirs(allocator, include_dirs, artifact.include_dirs.items);
    try processPkgConfig(allocator, include_dirs, artifact);
    for (artifact.packages.items) |pkg| {
        try processPackage(allocator, packages, pkg);
    }
}

fn processPackage(
    allocator: std.mem.Allocator,
    packages: *StringStringMap,
    pkg: std.build.Pkg,
) anyerror!void {
    if (packages.contains(pkg.name)) return;

    // Support Zig 0.9.1
    const source = if (@hasField(std.build.Pkg, "source")) pkg.source else pkg.path;

    const maybe_path = switch (source) {
        .path => |path| path,
        .generated => |generated| generated.path,
    };

    if (maybe_path) |path| {
        try addPackage(allocator, packages, pkg.name, path);
    }

    if (pkg.dependencies) |dependencies| {
        for (dependencies) |dep| {
            try processPackage(allocator, packages, dep);
        }
    }
}

fn processIncludeDirs(
    allocator: std.mem.Allocator,
    include_dirs: *StringVoidMap,
    dirs: []std.build.LibExeObjStep.IncludeDir,
) !void {
    try include_dirs.ensureUnusedCapacity(allocator, dirs.len);

    for (dirs) |dir| {
        const candidate: []const u8 = switch (dir) {
            .raw_path => |path| path,
            .raw_path_system => |path| path,
            else => continue,
        };

        include_dirs.putAssumeCapacity(candidate, {});
    }
}

fn processMacro(
    allocator: std.mem.Allocator,
    c_macros: *StringStringMap,
    list: []const []const u8,
) !void {
    try c_macros.ensureUnusedCapacity(allocator, list.len);

    for (list) |v| {
        try addMacro(allocator, c_macros, v);
    }
}

fn processPkgConfig(
    allocator: std.mem.Allocator,
    include_dirs: *StringVoidMap,
    exe: *std.build.LibExeObjStep,
) !void {
    for (exe.link_objects.items) |link_object| {
        if (link_object != .system_lib) continue;
        const system_lib = link_object.system_lib;

        // Support Zig 0.9.1
        if (@TypeOf(system_lib) == []const u8) return;

        if (system_lib.use_pkg_config == .no) continue;

        getPkgConfigIncludes(allocator, include_dirs, exe, system_lib.name) catch |err| switch (err) {
            error.PkgConfigInvalidOutput,
            error.PkgConfigCrashed,
            error.PkgConfigFailed,
            error.PkgConfigNotInstalled,
            error.PackageNotFound,
            => switch (system_lib.use_pkg_config) {
                .yes => {
                    // pkg-config failed, so zig will not add any include paths
                },
                .force => {
                    log.warn("pkg-config failed for library {s}", .{system_lib.name});
                },
                .no => unreachable,
            },
            else => |e| return e,
        };
    }
}

fn getPkgConfigIncludes(
    allocator: std.mem.Allocator,
    include_dirs: *StringVoidMap,
    exe: *std.build.LibExeObjStep,
    name: []const u8,
) !void {
    if (exe.runPkgConfig(name)) |args| {
        for (args) |arg| {
            if (std.mem.startsWith(u8, arg, "-I")) {
                const candidate = arg[2..];
                try include_dirs.put(allocator, candidate, {});
            }
        }
    } else |err| return err;
}

fn runBuild(builder: *Builder) anyerror!void {
    switch (@typeInfo(@typeInfo(@TypeOf(root.build)).Fn.return_type.?)) {
        .Void => root.build(builder),
        .ErrorUnion => try root.build(builder),
        else => @compileError("expected return type of build to be 'void' or '!void'"),
    }
}

fn nextArg(args: [][]const u8, idx: *usize) ?[]const u8 {
    if (idx.* >= args.len) return null;
    defer idx.* += 1;
    return args[idx.*];
}
