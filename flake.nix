{
  description = "test-flow dev shell + apps (tests, coverage)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs, ... }:
  let
    forAllSystems =
      f: nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" ] (system:
        f (import nixpkgs { inherit system; }));
  in {
    # Разделяем определение emacs с нужными пакетами, чтобы использовать его и в devShell, и в apps.
    devShells = forAllSystems (pkgs:
      let
        emacsWithPkgs =
          (pkgs.emacsPackagesFor pkgs.emacs30-nox).emacsWithPackages
            (epkgs: [
              epkgs.melpaPackages.undercover
              epkgs.melpaPackages.dash
              epkgs.melpaPackages.f
              epkgs.melpaPackages.s
              epkgs.melpaPackages.pcache
              epkgs.melpaPackages.request
            ]);
      in {
        default = pkgs.mkShell {
          packages = [
            emacsWithPkgs
            pkgs.cask
            pkgs.git
            pkgs.cacert
            pkgs.curl
          ];
          shellHook = ''
            export CASK_EMACS=$(command -v emacs)
            export EMACS=$(command -v emacs)
          '';
        };
      });

    # Утилиты запуска:
    # - nix run .#tests    → запускает batch-ERT через tests/run-tests.el или t/run-tests.el
    # - nix run .#coverage → запускает scripts/coverage.el или s/coverage.el (undercover → LCOV)
    # - nix run .          → alias на .#tests (apps.default)
    apps = forAllSystems (pkgs:
      let
        emacsWithPkgs =
          (pkgs.emacsPackagesFor pkgs.emacs30-nox).emacsWithPackages
            (epkgs: [
              epkgs.melpaPackages.undercover
              epkgs.melpaPackages.dash
              epkgs.melpaPackages.f
              epkgs.melpaPackages.s
              epkgs.melpaPackages.pcache
              epkgs.melpaPackages.request
            ]);

        makeApp = name: text:
          let drv = pkgs.writeShellApplication {
            inherit name;
            runtimeInputs = [ emacsWithPkgs ];
            text = text;
          };
          in { type = "app"; program = "${drv}/bin/${name}"; };
      in rec {
        tests = makeApp "tests" ''
          set -euo pipefail
          if [ -f tests/run-tests.el ]; then
            TARGET=tests/run-tests.el
          elif [ -f t/run-tests.el ]; then
            TARGET=t/run-tests.el
          else
            echo "No run-tests.el found under tests/ or t/" >&2
            exit 1
          fi
          exec ${emacsWithPkgs}/bin/emacs -Q --batch -l "$TARGET"
        '';

        coverage = makeApp "coverage" ''
          set -euo pipefail
          if [ -f scripts/coverage.el ]; then
            TARGET=scripts/coverage.el
          elif [ -f s/coverage.el ]; then
            TARGET=s/coverage.el
          else
            echo "No coverage.el found under scripts/ or s/" >&2
            exit 1
          fi
          exec ${emacsWithPkgs}/bin/emacs -Q --batch -l "$TARGET"
        '';

        # alias по умолчанию, чтобы nix run . работал без #tests
        default = tests;
      });

    # checks: nix flake check будет прогонять тесты
    checks = forAllSystems (pkgs:
      let
        emacsWithPkgs =
          (pkgs.emacsPackagesFor pkgs.emacs30-nox).emacsWithPackages
            (epkgs: [
              epkgs.melpaPackages.undercover
              epkgs.melpaPackages.dash
              epkgs.melpaPackages.f
              epkgs.melpaPackages.s
              epkgs.melpaPackages.pcache
              epkgs.melpaPackages.request
            ]);
      in {
        tests = pkgs.runCommand "test-flow-tests" { buildInputs = [ emacsWithPkgs ]; } ''
          set -euo pipefail
          if [ -f tests/run-tests.el ]; then
            TARGET=tests/run-tests.el
          elif [ -f t/run-tests.el ]; then
            TARGET=t/run-tests.el
          else
            echo "No run-tests.el found under tests/ or t/" >&2
            exit 1
          fi
          ${emacsWithPkgs}/bin/emacs -Q --batch -l "$TARGET"
          mkdir -p "$out"
          echo "ok" > "$out/result"
        '';
      });

    # Дополнительно: default package — исполняемый скрипт запуска тестов (nix build .)
    packages = forAllSystems (pkgs:
      let
        emacsWithPkgs =
          (pkgs.emacsPackagesFor pkgs.emacs30-nox).emacsWithPackages
            (epkgs: [
              epkgs.melpaPackages.undercover
              epkgs.melpaPackages.dash
              epkgs.melpaPackages.f
              epkgs.melpaPackages.s
              epkgs.melpaPackages.pcache
              epkgs.melpaPackages.request
            ]);
      in {
        default = pkgs.writeShellApplication {
          name = "test-flow-tests";
          runtimeInputs = [ emacsWithPkgs ];
          text = ''
            set -euo pipefail
            if [ -f tests/run-tests.el ]; then
              TARGET=tests/run-tests.el
            elif [ -f t/run-tests.el ]; then
              TARGET=t/run-tests.el
            else
              echo "No run-tests.el found under tests/ or t/" >&2
              exit 1
            fi
            exec ${emacsWithPkgs}/bin/emacs -Q --batch -l "$TARGET"
          '';
        };
      });
  };
}
