{
  description = "Described keymaps with popup help";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
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
          emacs = pkgs.emacs30 or pkgs.emacs29 or pkgs.emacs;
          emacsPackages = pkgs.emacsPackagesFor emacs;

          versionLine = lib.findFirst
            (line: lib.hasPrefix ";; Version: " line)
            (throw "keymap-popup.el has no Version header")
            (lib.splitString "\n" (builtins.readFile ./keymap-popup.el));
          version = lib.removePrefix ";; Version: " versionLine;

          source = lib.cleanSourceWith {
            src = ./.;
            filter = path: type:
              let
                name = baseNameOf path;
              in
              lib.cleanSourceFilter path type
              && !(builtins.elem name [ ".direnv" ".test-results" "refs" "result" ]
                || lib.hasSuffix ".elc" name
                || lib.hasSuffix ".info" name
                || lib.hasSuffix ".texi" name
                || lib.hasSuffix "~" name);
          };

          emacsWithPackages = emacsPackages.emacsWithPackages (epkgs: [
            epkgs.package-lint
          ]);

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
          inherit check emacsWithPackages package pkgs;
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
          package = project.package;
        });

      devShells = forAllSystems (system:
        let
          project = mkProject system;
        in
        {
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
