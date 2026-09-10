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
        ./lisp/gnosis-answer.el
        ./lisp/gnosis-cloze.el
        ./lisp/gnosis-dashboard.el
        ./lisp/gnosis-db.el
        ./lisp/gnosis-export-import.el
        ./lisp/gnosis-journal.el
        ./lisp/gnosis-links.el
        ./lisp/gnosis-monkeytype.el
        ./lisp/gnosis-assets.el
        ./lisp/gnosis-backup.el
        ./lisp/gnosis-image.el
        ./lisp/gnosis-model.el
        ./lisp/gnosis-nodes.el
        ./lisp/gnosis-org.el
        ./lisp/gnosis-review.el
        ./lisp/gnosis-study.el
        ./lisp/gnosis-agent.el
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
        ./tests
        # Discovery tests need the optional backend, not a package dependency.
        ./optional/canvas-3d
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
            root = ./.;
            fileset = lib.fileset.unions (packageFiles ++ [ ./docs/gnosis.org ]);
          };
          keymapPopup = emacsPackages.melpaBuild {
            pname = "keymap-popup";
            version = "0.4.3";
            src = keymap-popup;
            packageRequires = [ ];
          };
          gnosis = emacsPackages.melpaBuild {
            pname = "gnosis";
            inherit version;
            src = packageSource;
            files = ''("lisp/*.el" "docs/gnosis.texi")'';
            preBuild = ''
              emacs --quick --batch --load ox-texinfo docs/gnosis.org \
                --funcall org-texinfo-export-to-texinfo
            '';
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
          keymapPopup = emacsPackages.melpaBuild {
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
              make GNOSIS_ENV_WRAPPED=1 ENV= EMACS=emacs JOBS="$NIX_BUILD_CORES" dev
              emacs --quick --batch \
                --eval="(require 'package)" \
                --eval="(package-initialize)" \
                --eval="(unless (and (autoloadp (symbol-function 'gnosis)) \
                                     (commandp 'gnosis)) \
                          (error \"Installed gnosis command is not autoloaded\"))"
              (cd "$TMPDIR"
                emacs --quick --batch \
                  --eval="(require 'package)" \
                  --eval="(package-initialize)" \
                  --eval="(require 'info)" \
                  --eval='(Info-find-node "gnosis" "Top")' \
                  --eval='(unless (and (equal Info-current-node "Top")
                                      (file-equal-p (concat Info-current-file ".info")
                                                    "${self.packages.${system}.gnosis}/share/emacs/site-lisp/elpa/gnosis-${version}/gnosis.info"))
                            (error "Info manual did not resolve from installed package"))'
              )
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
                  pkgs.bash
                  pkgs.coreutils
                  pkgs.gnumake
                  pkgs.gnugrep
                  pkgs.texinfo
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
                  make GNOSIS_ENV_WRAPPED=1 ENV= EMACS=emacs ${target} "$@"
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
              pkgs.texinfo
              self.packages.${system}.emacs-with-gnosis
            ];
          };
        }
      );

      formatter = forAllSystems (system: (import nixpkgs { inherit system; }).nixfmt);
    };
}
