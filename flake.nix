{
  description = "Knowledge management and spaced repetition for Emacs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.keymap-popup = {
    url = "git+https://git.thanosapollo.org/emacs-keymap-popup.git";
    flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      keymap-popup,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      version = "0.10.6";
      packageFiles = [
        ./lisp/gnosis-logical-day.el
        ./lisp/gnosis-fsrs.el
        ./lisp/gnosis-anki.el
        ./lisp/gnosis-cloze.el
        ./lisp/gnosis-dashboard.el
        ./lisp/gnosis-db.el
        ./lisp/gnosis-export-import.el
        ./lisp/gnosis-journal.el
        ./lisp/gnosis-links.el
        ./lisp/gnosis-monkeytype.el
        ./lisp/gnosis-nodes.el
        ./lisp/gnosis-org.el
        ./lisp/gnosis-review.el
        ./lisp/gnosis-scheduler.el
        ./lisp/gnosis-sqlite.el
        ./lisp/gnosis-tags.el
        ./lisp/gnosis-tl.el
        ./lisp/gnosis-utils.el
        ./lisp/gnosis-vc.el
        ./lisp/gnosis.el
      ];
      testFiles = [
        ./docs/gnosis.org
        ./tests/gnosis-fsrs-v2.json
        ./tests/gnosis-test-anki.el
        ./tests/gnosis-test-autoload-boundary.el
        ./tests/gnosis-test-bulk-link.el
        ./tests/gnosis-test-cloze.el
        ./tests/gnosis-test-dashboard.el
        ./tests/gnosis-test-db.el
        ./tests/gnosis-test-export-import.el
        ./tests/gnosis-test-fsrs.el
        ./tests/gnosis-test-helpers.el
        ./tests/gnosis-test-insert-template.el
        ./tests/gnosis-test-isolation.el
        ./tests/gnosis-test-journal-boundary.el
        ./tests/gnosis-test-journal.el
        ./tests/gnosis-test-links.el
        ./tests/gnosis-test-logical-day.el
        ./tests/gnosis-test-logical-day-cutover.el
        ./tests/gnosis-test-migration.el
        ./tests/gnosis-test-nodes-boundary.el
        ./tests/gnosis-test-nodes.el
        ./tests/gnosis-test-org.el
        ./tests/gnosis-test-review.el
        ./tests/gnosis-test-scheduler-storage.el
        ./tests/gnosis-test-scheduler.el
        ./tests/gnosis-test-script-detection.el
        ./tests/gnosis-test-sqlite.el
      ];
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          emacsPackages = pkgs.emacsPackagesFor pkgs.emacs-nox;
          packageSource = lib.fileset.toSource {
            root = ./lisp;
            fileset = lib.fileset.unions packageFiles;
          };
          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = "0.4.3";
            src = keymap-popup;
            packageRequires = [ ];
          };
          gnosis = emacsPackages.trivialBuild {
            pname = "gnosis";
            inherit version;
            src = packageSource;
            packageRequires = [
              emacsPackages.compat
              keymapPopup
            ];
          };
          emacsWithGnosis = emacsPackages.emacsWithPackages (_: [
            emacsPackages.compat
            keymapPopup
            gnosis
            emacsPackages.package-lint
          ]);
        in
        {
          default = gnosis;
          inherit gnosis;
          keymap-popup = keymapPopup;
          emacs-with-gnosis = emacsWithGnosis;
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          emacsPackages = pkgs.emacsPackagesFor pkgs.emacs-nox;
          source = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions ([ ./Makefile ] ++ packageFiles ++ testFiles);
          };
          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = "0.4.3";
            src = keymap-popup;
            packageRequires = [ ];
          };
          emacsWithDependencies = emacsPackages.emacsWithPackages (_: [
            emacsPackages.compat
            keymapPopup
            self.packages.${system}.gnosis
            emacsPackages.package-lint
          ]);
          check = pkgs.stdenvNoCC.mkDerivation {
            pname = "gnosis-check";
            inherit version;
            src = source;
            nativeBuildInputs = [
              emacsWithDependencies
              pkgs.gnumake
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
              make ENV= EMACS=emacs dev
              runHook postBuild
            '';
            installPhase = ''
              mkdir -p "$out"
              touch "$out/passed"
            '';
          };
        in
        {
          default = check;
          package = self.packages.${system}.gnosis;
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          emacs = self.packages.${system}.emacs-with-gnosis;
          source = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions ([ ./Makefile ] ++ packageFiles ++ testFiles);
          };
          mkApp =
            name: target:
            let
              script = pkgs.writeShellApplication {
                name = "gnosis-${name}";
                runtimeInputs = [
                  emacs
                  pkgs.coreutils
                  pkgs.gnumake
                ];
                text = ''
                  work=$(mktemp -d)
                  trap 'chmod -R u+w "$work"; rm -rf "$work"' EXIT
                  mkdir -p "$work/project" "$work/home" "$work/cache" \
                    "$work/config" "$work/data" "$work/state"
                  cp -R ${source}/. "$work/project/"
                  chmod -R u+w "$work/project"
                  export HOME="$work/home"
                  export XDG_CACHE_HOME="$work/cache"
                  export XDG_CONFIG_HOME="$work/config"
                  export XDG_DATA_HOME="$work/data"
                  export XDG_STATE_HOME="$work/state"
                  cd "$work/project"
                  make ENV= EMACS=emacs ${target} "$@"
                '';
              };
            in
            {
              type = "app";
              program = "${script}/bin/gnosis-${name}";
              meta.description = "Run the Gnosis ${name} target";
            };
        in
        {
          check = mkApp "check" "dev";
          test = mkApp "test" "test";
          lint = mkApp "lint" "lint";
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShellNoCC {
            packages = [
              pkgs.git
              pkgs.gnumake
              self.packages.${system}.emacs-with-gnosis
            ];
          };
        }
      );

      formatter = forAllSystems (system: (import nixpkgs { inherit system; }).nixfmt);
    };
}
