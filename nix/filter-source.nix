{ stdenv }:

let
  ignoredDirs = [
    # Don't trigger a rebuild for `git`-related changes
    ".git"

    # Don't trigger a rebuild when switching between Nix and other build tools.
    # Also, these build tools leave behind large work directories, which you
    # don't want to copy to the `/nix/store`.  If you don't filter out these
    # large directories you may get the following warning:
    #
    #     warning: dumping very large path (> 256 MiB); this may run out of memory
    ".cabal-sandbox"
    "dist"
    ".stack-work"
    "nix"
    "motif"
    "pdb"
  ];

  ignoredSuffix = path:
        # These are files left behind when building things using `ghc` directly.
        # There is no point triggering a rebuild if the user is compiling quick
        # and dirty tests with `ghc`.  We also don't expect these extensions to be
        # present in a clean repository that we just fetched.
        stdenv.lib.hasSuffix ".hi"     path
    ||  stdenv.lib.hasSuffix ".o"      path
    ||  stdenv.lib.hasSuffix ".o-boot" path
    ||  stdenv.lib.hasSuffix ".dyn_o"  path
    ||  stdenv.lib.hasSuffix ".p_o"    path;

  predicate = path: type:
    !(  (type == "directory" && builtins.elem (baseNameOf path) ignoredDirs)

        # The the Nix manual states that file types of "unknown" cannot be copied
        # to the `/nix/store` and will cause the build to fail:
        #
        # > ... the type of the file, which is either "regular", "directory",
        # > "symlink" or "unknown" (for other kinds of files such as device nodes
        # > or fifos — but note that those cannot be copied to the Nix store, so
        # > if the predicate returns true for them, the copy will fail)
    ||  (type == "unknown")

    ||  (type == "file" && ignoredSuffix path)

        # `nix-build` by default deposits a `result` symlink which should not
        # trigger a rebuild.  We don't expect any Haskell project files to be
        # named `result`
    ||  (type == "file" && baseNameOf path == "result")
    );

in
  builtins.filterSource predicate
