const std = @import("std");
const zig_builtin = @import("builtin");
const builtin = @import("builtin");
const Config = @import("DocumentStore.zig").Config;
const ast = @import("ast.zig");
const tracy = @import("tracy");
const Ast = std.zig.Ast;
const URI = @import("uri.zig");
const ZCS = @import("ZigCompileServer.zig");
const log = std.log.scoped(.zls_translate_c);

/// converts a `@cInclude` node into an equivalent c header file
/// which can then be handed over to `zig translate-c`
/// Caller owns returned memory.
///
/// **Example**
/// ```zig
/// const glfw = @cImport({
///     @cDefine("GLFW_INCLUDE_VULKAN", {});
///     @cInclude("GLFW/glfw3.h");
/// });
/// ```
/// gets converted into:
/// ```c
/// #define GLFW_INCLUDE_VULKAN
/// #include "GLFW/glfw3.h"
/// ```
pub fn convertCInclude(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index) error{ OutOfMemory, Unsupported }![]const u8 {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const main_tokens = tree.nodes.items(.main_token);

    std.debug.assert(ast.isBuiltinCall(tree, node));
    std.debug.assert(std.mem.eql(u8, Ast.tokenSlice(tree, main_tokens[node]), "@cImport"));

    var output = std.ArrayListUnmanaged(u8){};
    errdefer output.deinit(allocator);

    var buffer: [2]Ast.Node.Index = undefined;
    for (ast.builtinCallParams(tree, node, &buffer).?) |child| {
        var stack_allocator = std.heap.stackFallback(512, allocator);
        try convertCIncludeInternal(allocator, stack_allocator.get(), tree, child, &output);
    }

    return output.toOwnedSlice(allocator);
}

/// HACK self-hosted has not implemented async yet
fn callConvertCIncludeInternal(allocator: std.mem.Allocator, args: anytype) error{ OutOfMemory, Unsupported }!void {
    if (zig_builtin.zig_backend == .other or zig_builtin.zig_backend == .stage1) {
        const FrameSize = @sizeOf(@Frame(convertCIncludeInternal));
        const child_frame = try allocator.alignedAlloc(u8, std.Target.stack_align, FrameSize);
        defer allocator.free(child_frame);

        return await @asyncCall(child_frame, {}, convertCIncludeInternal, args);
    } else {
        // TODO find a non recursive solution
        return @call(.auto, convertCIncludeInternal, args);
    }
}

fn convertCIncludeInternal(
    allocator: std.mem.Allocator,
    stack_allocator: std.mem.Allocator,
    tree: Ast,
    node: Ast.Node.Index,
    output: *std.ArrayListUnmanaged(u8),
) error{ OutOfMemory, Unsupported }!void {
    const node_tags = tree.nodes.items(.tag);
    const main_tokens = tree.nodes.items(.main_token);

    var writer = output.writer(allocator);

    var buffer: [2]Ast.Node.Index = undefined;
    if (ast.blockStatements(tree, node, &buffer)) |statements| {
        for (statements) |statement| {
            try callConvertCIncludeInternal(stack_allocator, .{ allocator, stack_allocator, tree, statement, output });
        }
    } else if (ast.builtinCallParams(tree, node, &buffer)) |params| {
        if (params.len < 1) return;

        const call_name = Ast.tokenSlice(tree, main_tokens[node]);

        if (node_tags[params[0]] != .string_literal) return error.Unsupported;
        const first = extractString(Ast.tokenSlice(tree, main_tokens[params[0]]));

        if (std.mem.eql(u8, call_name, "@cInclude")) {
            try writer.print("#include <{s}>\n", .{first});
        } else if (std.mem.eql(u8, call_name, "@cDefine")) {
            if (params.len < 2) return;

            var buffer2: [2]Ast.Node.Index = undefined;
            const is_void = if (ast.blockStatements(tree, params[1], &buffer2)) |block| block.len == 0 else false;

            if (is_void) {
                try writer.print("#define {s}\n", .{first});
            } else {
                if (node_tags[params[1]] != .string_literal) return error.Unsupported;
                const second = extractString(Ast.tokenSlice(tree, main_tokens[params[1]]));
                try writer.print("#define {s} {s}\n", .{ first, second });
            }
        } else if (std.mem.eql(u8, call_name, "@cUndef")) {
            try writer.print("#undef {s}\n", .{first});
        } else {
            return error.Unsupported;
        }
    }
}

pub const Result = union(enum) {
    // uri to the generated zig file
    success: []const u8,
    // zig translate-c failed with the given error messages
    failure: std.zig.ErrorBundle,

    pub fn deinit(self: *Result, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .success => |path| allocator.free(path),
            .failure => |*bundle| bundle.deinit(allocator),
        }
    }
};

/// takes a c header file and returns the result from calling `zig translate-c`
/// returns a URI to the generated zig file on success or the content of stderr on failure
/// null indicates a failure which is automatically logged
/// Caller owns returned memory.
pub fn translate(
    allocator: std.mem.Allocator,
    config: Config,
    include_dirs: []const []const u8,
    c_macros: []const []const u8,
    source: []const u8,
) !?Result {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const file_path = try std.fs.path.join(allocator, &[_][]const u8{ config.global_cache_path.?, "cimport.h" });
    defer allocator.free(file_path);

    var file = std.fs.createFileAbsolute(file_path, .{}) catch |err| {
        log.warn("failed to create file '{s}': {}", .{ file_path, err });
        return null;
    };
    defer file.close();
    defer std.fs.deleteFileAbsolute(file_path) catch |err| {
        log.warn("failed to delete file '{s}': {}", .{ file_path, err });
    };

    file.writeAll(source) catch |err| {
        log.warn("failed to write to '{s}': {}", .{ file_path, err });
        return null;
    };

    const base_args = &[_][]const u8{
        config.zig_exe_path.?,
        "translate-c",
        "--zig-lib-dir",
        config.zig_lib_path.?,
        "--global-cache-dir",
        config.global_cache_path.?,
        "-lc",
    };

    const argc = base_args.len + (2 * include_dirs.len) + (2 * c_macros.len) + 1;
    var argv = try std.ArrayListUnmanaged([]const u8).initCapacity(allocator, argc);
    defer argv.deinit(allocator);

    argv.appendSliceAssumeCapacity(base_args);

    for (include_dirs) |include_dir| {
        argv.appendAssumeCapacity("-I");
        argv.appendAssumeCapacity(include_dir);
    }

    for (c_macros) |macro| {
        argv.appendAssumeCapacity("-D");
        argv.appendAssumeCapacity(macro);
    }

    argv.appendAssumeCapacity(file_path);

    const argv_str = try std.mem.join(allocator, " ", argv.items);
    defer allocator.free(argv_str);
    log.info("{s}", .{argv_str});

    const run_result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv.items,
        .max_output_bytes = 100 * 1024 * 1024, // 100 MB
    });
    switch (run_result.term) {
        .Exited => |code| {
            if (code != 0) {
                log.err("translate-c failed: ({d}) {s}", .{ code, run_result.stderr });
                return null;
            }

            const cwd_path = try std.fs.cwd().realpathAlloc(allocator, ".");
            defer allocator.free(cwd_path);

            const abs_path = try std.fs.path.join(allocator, &.{ cwd_path, "zig-cache", "cimport.zig" });
            defer allocator.free(abs_path);

            // create tmp file
            var cimport_zig_file = std.fs.createFileAbsolute(abs_path, .{}) catch |err| {
                log.err("create 'cimport.zig' failed: {}", .{err});
                return null;
            };
            defer cimport_zig_file.close();

            cimport_zig_file.writeAll(run_result.stdout) catch |err| {
                log.err("write 'cimport.zig' failed: {}", .{err});
                return null;
            };

            return Result{ .success = try URI.fromPath(allocator, abs_path) };
        },
        else => {
            log.err("translate-c failed: unknown error", .{});
            return null;
        },
    }
}

fn extractString(str: []const u8) []const u8 {
    if (std.mem.startsWith(u8, str, "\"") and std.mem.endsWith(u8, str, "\"")) {
        return str[1 .. str.len - 1];
    } else {
        return str;
    }
}
