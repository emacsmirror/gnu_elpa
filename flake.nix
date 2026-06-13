{
  description = "Forgejo client for Emacs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [
        # Note: Most of the testing I've done is x86_64-linux and
        # aarch64-linux.  If you find any issues with darwin feel free
        # to report/submit a PR.
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      mkForgejo = system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          emacs = pkgs.emacs;
          emacsPackages = pkgs.emacsPackagesFor emacs;

          source = lib.cleanSourceWith {
            src = ./.;
            filter = path: type:
              let name = baseNameOf path;
              in !(name == ".test-results"
                   || lib.hasSuffix ".elc" name
                   || lib.hasSuffix "~" name);
          };

          keymapPopupVersion = "0.3.1";

          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = pkgs.fetchurl {
              url = "https://elpa.gnu.org/packages/keymap-popup-${keymapPopupVersion}.tar";
              hash = "sha256-C+ECWpChsO6MUG+oAPJDhZruWphkxy7VLe9YFAzShFQ=";
            };
            packageRequires = [ ];
          };

          emacsWithPackages = emacsPackages.emacsWithPackages (epkgs: [
            keymapPopup
            epkgs.markdown-mode
          ]);

          tests = pkgs.stdenv.mkDerivation {
            pname = "emacs-forgejo-tests";
            version = "git";
            src = source;
            nativeBuildInputs = [
              emacsWithPackages
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
              EMACS_CMD=emacs \
                FORGEJO_ENV_WRAPPED=1 \
                make test
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out
              touch $out/tests-passed
              runHook postInstall
            '';
          };
        in {
          inherit emacs emacsWithPackages keymapPopup pkgs tests;
        };
    in {
      checks = forAllSystems (system:
        let forgejo = mkForgejo system;
        in {
          test = forgejo.tests;
        });

      devShells = forAllSystems (system:
        let forgejo = mkForgejo system;
        in {
          default = forgejo.pkgs.mkShell {
            packages = with forgejo.pkgs; [
              cacert
              forgejo.emacsWithPackages
              git
              gnumake
              sqlite
            ];

            shellHook = ''
              export EMACS_CMD=emacs
            '';
          };
        });
    };
}
