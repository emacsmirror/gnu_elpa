{
  description = "Described keymaps with popup help";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  # Exact Package-Requires minimum from the immutable NixOS 23.11 release.
  inputs.nixpkgs-emacs291.url = "github:NixOS/nixpkgs/057f9aecfb71c4437d2b27d3323df7f93c010b7e";

  outputs = { nixpkgs, nixpkgs-emacs291, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      mkProject = system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          emacs = pkgs.emacs;
          minimum = (import nixpkgs-emacs291 { inherit system; }).emacs29-nox;
          emacsPackages = pkgs.emacsPackagesFor emacs;

          versionLine = lib.findFirst
            (line: lib.hasPrefix ";; Version: " line)
            (throw "keymap-popup.el has no Version header")
            (lib.splitString "\n" (builtins.readFile ./keymap-popup.el));
          version = lib.removePrefix ";; Version: " versionLine;

          # Only package, test, and manual inputs belong in build sources.
          # The Make frontend separately protects the initial Git flake copy.
          source = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions (map (name: ./. + "/${name}")
              (builtins.fromJSON (builtins.readFile ./admin/source-manifest.json)));
          };

          emacsWithPackages = emacsPackages.emacsWithPackages (epkgs: [
            epkgs.package-lint
          ]);

          # Copy only Lisp sources, including package-lint dependencies.  No
          # bytecode compiled by the default Emacs enters another runtime lane.
          testDependencies = pkgs.runCommand "keymap-popup-test-dependencies" { } ''
            mkdir -p $out
            find -L ${emacsPackages.package-lint} ${lib.concatStringsSep " " emacsPackages.package-lint.packageRequires} -name '*.el' -type f \
              -exec cp '{}' $out/ \;
            cp -r ${emacsPackages.package-lint}/share/emacs/site-lisp/elpa/package-lint-*/data $out/
          '';
          matrixTools = [ pkgs.python3 pkgs.gnumake ];
          matrixShell = runtime: pkgs.mkShellNoCC {
            packages = matrixTools ++ [ runtime ];
            KEYMAP_POPUP_MATRIX_DEPS = testDependencies;
            KEYMAP_POPUP_MATRIX_VERSION = runtime.version;
          };
          matrixCheck = name: runtime: pkgs.stdenvNoCC.mkDerivation {
            pname = "keymap-popup-matrix-${name}";
            inherit version;
            src = source;
            nativeBuildInputs = matrixTools ++ [ runtime ];
            KEYMAP_POPUP_MATRIX_DEPS = testDependencies;
            dontConfigure = true;
            buildPhase = ''
              python3 admin/test-matrix --lane ${name} --expected ${runtime.version} \
                --root "$TMPDIR/lane"
            '';
            installPhase = ''
              mkdir -p $out
              cp "$TMPDIR/lane/ert.json" "$TMPDIR/lane/runtime.json" "$TMPDIR/lane/passed" $out/
            '';
          };

          package = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            inherit version;
            src = source;
            packageRequires = [ ];
          };

          check = pkgs.stdenvNoCC.mkDerivation {
            pname = "keymap-popup-checks";
            inherit version;
            src = source;
            nativeBuildInputs = [
              emacsWithPackages
              pkgs.gnumake
              pkgs.texinfo
            ];
            dontConfigure = true;

            buildPhase = ''
              runHook preBuild
              export HOME="$TMPDIR/home"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              export XDG_CONFIG_HOME="$TMPDIR/config"
              export XDG_DATA_HOME="$TMPDIR/share"
              export XDG_STATE_HOME="$TMPDIR/state"
              mkdir -p "$HOME" "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" \
                "$XDG_DATA_HOME" "$XDG_STATE_HOME"
              make USE_NIX=0 EMACS_CMD=emacs do-dev do-doc
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out
              touch $out/tests-passed
              runHook postInstall
            '';
          };
        in
        {
          inherit check emacsWithPackages package pkgs matrixShell matrixCheck emacs minimum source;
        };
    in
    {
      packages = forAllSystems (system:
        let
          project = mkProject system;
        in
        {
          default = project.package;
        });

      checks = forAllSystems (system:
        let
          project = mkProject system;
        in
        {
          default = project.check;
          matrix-runner = project.pkgs.runCommand "keymap-popup-matrix-runner" {
            nativeBuildInputs = [ project.pkgs.python3 ];
          } ''
            python3 ${project.source}/admin/test-matrix-runner.py
            touch $out
          '';
          matrix-minimum = project.matrixCheck "minimum" project.minimum;
          matrix-default = project.matrixCheck "default" project.emacs;
          package = project.package;
        });

      devShells = forAllSystems (system:
        let
          project = mkProject system;
        in
        {
          matrix-minimum = project.matrixShell project.minimum;
          matrix-default = project.matrixShell project.emacs;
          default = project.pkgs.mkShellNoCC {
            packages = [
              project.emacsWithPackages
              project.pkgs.gnumake
              project.pkgs.texinfo
            ];
          };
        });
    };
}
