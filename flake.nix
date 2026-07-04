{
  description = "Personal knowledge system for GNU Emacs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      mkGnosis =
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          version =
            let
              rev = self.shortRev or null;
            in
            if rev == null then "0.10.6-dirty" else "0.10.6-${rev}";

          emacs = pkgs.emacs30-nox or pkgs.emacs-nox or pkgs.emacs;
          emacsPackages = pkgs.emacsPackagesFor emacs;
          elispFiles = [
            "lisp/gnosis-sqlite.el"
            "lisp/gnosis-tl.el"
            "lisp/gnosis-utils.el"
            "lisp/gnosis-org.el"
            "lisp/gnosis-algorithm.el"
            "lisp/gnosis-cloze.el"
            "lisp/gnosis-db.el"
            "lisp/gnosis-vc.el"
            "lisp/gnosis-tags.el"
            "lisp/gnosis-custom-values.el"
            "lisp/gnosis-links.el"
            "lisp/gnosis.el"
            "lisp/gnosis-nodes.el"
            "lisp/gnosis-journal.el"
            "lisp/gnosis-review.el"
            "lisp/gnosis-dashboard.el"
            "lisp/gnosis-export-import.el"
            "lisp/gnosis-anki.el"
            "lisp/gnosis-monkeytype.el"
          ];
          elispFileArgs = lib.concatStringsSep " " elispFiles;
          ignoredSourceNames = [
            ".direnv"
            ".emacs-test-cache"
            ".hermes"
            ".test-results"
            ":memory:"
            "gnosis.db"
          ];

          source = lib.cleanSourceWith {
            src = ./.;
            filter =
              path: type:
              let
                name = baseNameOf path;
              in
              (lib.cleanSourceFilter path type)
              && !(
                lib.elem name ignoredSourceNames
                || lib.hasSuffix ".elc" name
                || lib.hasSuffix "~" name
                || lib.hasSuffix ".tar" name
                || lib.hasSuffix ".tar.gz" name
                || lib.hasSuffix ".apkg" name
                || lib.hasInfix "/.worktrees/" path
                || lib.hasInfix "/org-fc/" path
                || lib.hasInfix "/org-roam/" path
                || lib.hasInfix "/temp/" path
              );
          };

          keymapPopupVersion = "0.3.1";
          keymapPopupSrc = pkgs.fetchzip {
            url = "https://elpa.gnu.org/packages/keymap-popup-${keymapPopupVersion}.tar";
            hash = "sha256-hoH9SJ8LQS/uWNmwvauBJwMnnr4+DwhJpUFuHOihldM=";
          };

          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = keymapPopupSrc;
            packageRequires = [ ];
          };

          compat = emacsPackages.compat;

          gnosisEl = emacsPackages.trivialBuild {
            pname = "gnosis";
            inherit version;
            src = source;
            packageRequires = [
              compat
              keymapPopup
            ];
            nativeBuildInputs = [
              pkgs.gnumake
              pkgs.texinfo
            ];

            buildPhase = ''
              runHook preBuild
              emacs -l package -f package-initialize -L lisp --batch \
                -f batch-byte-compile ${elispFileArgs}
              (cd lisp && emacs --batch \
                --eval "(loaddefs-generate \".\" \"gnosis-autoloads.el\")")
              env GNOSIS_ENV_WRAPPED=1 make doc
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              lispdir="$out/share/emacs/site-lisp"
              infodir="$out/share/info"
              mkdir -p "$lispdir" "$infodir"
              install -m444 lisp/*.el lisp/*.elc "$lispdir/"
              install -m444 docs/gnosis.info "$infodir/"
              runHook postInstall
            '';

            meta = with lib; {
              description = "Personal knowledge system for GNU Emacs";
              homepage = "https://thanosapollo.org/projects/gnosis/";
              license = licenses.gpl3Plus;
              platforms = emacs.meta.platforms;
            };
          };

          devEmacs = emacsPackages.emacsWithPackages (_: [
            compat
            keymapPopup
          ]);
          emacsWithGnosis = emacsPackages.emacsWithPackages (_: [ gnosisEl ]);

          mkCheck =
            name: target:
            pkgs.stdenvNoCC.mkDerivation {
              pname = "gnosis-${name}";
              inherit version;
              src = source;
              nativeBuildInputs = [
                devEmacs
                pkgs.gnumake
                pkgs.texinfo
              ];
              dontConfigure = true;

              buildPhase = ''
                runHook preBuild
                unset EMACSDATA EMACSDOC EMACSLOADPATH EMACSPATH GREP_OPTIONS
                export HOME="$TMPDIR/home"
                export XDG_CACHE_HOME="$TMPDIR/cache"
                export XDG_CONFIG_HOME="$TMPDIR/config"
                export XDG_DATA_HOME="$TMPDIR/share"
                export XDG_STATE_HOME="$TMPDIR/state"
                mkdir -p "$HOME" "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" \
                  "$XDG_DATA_HOME" "$XDG_STATE_HOME"
                env GNOSIS_ENV_WRAPPED=1 make EMACS=emacs ${target}
                runHook postBuild
              '';

              installPhase = ''
                runHook preInstall
                mkdir -p "$out"
                touch "$out/${name}-passed"
                runHook postInstall
              '';
            };

          mkApp = name: target: {
            type = "app";
            program = "${
              pkgs.writeShellApplication {
                name = "gnosis-${name}";
                runtimeInputs = [
                  devEmacs
                  pkgs.gnumake
                  pkgs.texinfo
                ];
                text = ''
                  exec env GNOSIS_ENV_WRAPPED=1 make EMACS=emacs ${target} "$@"
                '';
              }
            }/bin/gnosis-${name}";
            meta.description = "Run make ${target} for Gnosis";
          };

          testCheck = mkCheck "test" "test";
        in
        {
          inherit
            devEmacs
            emacsWithGnosis
            gnosisEl
            keymapPopup
            keymapPopupSrc
            pkgs
            testCheck
            ;

          apps = {
            default = mkApp "test" "test";
            doc = mkApp "doc" "doc";
            test = mkApp "test" "test";
          };
        };
    in
    {
      apps = forAllSystems (system: (mkGnosis system).apps);

      checks = forAllSystems (system: {
        default = (mkGnosis system).testCheck;
        package = (mkGnosis system).gnosisEl;
      });

      devShells = forAllSystems (system: {
        default =
          let
            gnosis = mkGnosis system;
          in
          gnosis.pkgs.mkShell {
            packages = with gnosis.pkgs; [
              gnosis.devEmacs
              git
              gnumake
              sqlite
              texinfo
            ];

            EMACS = "emacs";

            shellHook = ''
              echo "gnosis dev shell"
              echo "  make test       # run ERT suites"
              echo "  make doc        # build Texinfo docs"
              echo "  nix flake check # package and test"
            '';
          };
      });

      formatter = forAllSystems (system: (mkGnosis system).pkgs.nixfmt);

      overlays.default = final: prev: {
        gnosis = self.packages.${prev.system}.gnosis;
        emacs-with-gnosis = self.packages.${prev.system}.emacs-with-gnosis;
      };

      packages = forAllSystems (system: {
        default = (mkGnosis system).gnosisEl;
        emacs-with-gnosis = (mkGnosis system).emacsWithGnosis;
        gnosis = (mkGnosis system).gnosisEl;
        keymap-popup = (mkGnosis system).keymapPopup;
      });
    };
}
