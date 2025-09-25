{
  description = "test-flow dev shell + apps (tests, coverage)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs, ... }:
  let
    forAllSystems =
      f: nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" ] (system:
        f (import nixpkgs { inherit system; }));
  in {
    # Лёгкий devShell: без Emacs, только инструменты. Emacs подхватывается в apps по запросу.
    devShells = forAllSystems (pkgs:
      {
        default = pkgs.mkShell {
          packages = [
            pkgs.cask
            pkgs.git
            pkgs.cacert
            pkgs.curl
          ];
          shellHook = ''
            # Сертификаты для HTTPS (emacs/cask/	curl)
            export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
            export NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
            # Если у вас есть системный emacs, cask сможет его использовать:
            # export CASK_EMACS=$(command -v emacs || true)
          '';
        };
      });

    # Утилиты запуска:
    # - nix run .#tests        → batch-ERT через tests/run-tests.el или t/run-tests.el (минимальный Emacs)
    # - nix run .#coverage     → scripts/coverage.el (Emacs с undercover)
    # - nix run .#cask-install → cask install с Emacs, в котором уже есть package-build
    # - nix run .              → alias на .#tests (apps.default)
    apps = forAllSystems (pkgs:
      let
        # Базовый минимальный Emacs для тестов (без лишних elpa)
        emacsBase = pkgs.emacs30-nox;

        # Emacs c undercover + package-build для покрытия и cask bootstrap
        emacsCoverage =
          (pkgs.emacsPackagesFor pkgs.emacs30-nox).emacsWithPackages
            (epkgs: [
              epkgs.melpaPackages.undercover
              epkgs.melpaPackages."package-build"
            ]);

        makeApp = name: text:
          let drv = pkgs.writeShellApplication {
            inherit name;
            runtimeInputs = [ ];
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
          exec ${emacsBase}/bin/emacs -Q --batch -l "$TARGET"
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
          exec ${emacsCoverage}/bin/emacs -Q --batch -l "$TARGET"
        '';

        cask-install = makeApp "cask-install" ''
          set -euo pipefail
          export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          export NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
          export CASK_EMACS=${emacsCoverage}/bin/emacs
          exec ${pkgs.cask}/bin/cask install
        '';

        # alias по умолчанию
        default = tests;
      });

    # checks: nix flake check будет прогонять тесты с минимальным Emacs
    checks = forAllSystems (pkgs:
      let
        emacsBase = pkgs.emacs30-nox;
      in {
        tests = pkgs.runCommand "test-flow-tests" { buildInputs = [ emacsBase ]; } ''
          set -euo pipefail
          if [ -f tests/run-tests.el ]; then
            TARGET=tests/run-tests.el
          elif [ -f t/run-tests.el ]; then
            TARGET=t/run-tests.el
          else
            echo "No run-tests.el found under tests/ or t/" >&2
            exit 1
          fi
          ${emacsBase}/bin/emacs -Q --batch -l "$TARGET"
          mkdir -p "$out"
          echo "ok" > "$out/result"
        '';
      });

    # Дополнительно: default package — исполняемый скрипт запуска тестов (nix build .)
    packages = forAllSystems (pkgs:
      let
        emacsBase = pkgs.emacs30-nox;
      in {
        default = pkgs.writeShellApplication {
          name = "test-flow-tests";
          runtimeInputs = [ emacsBase ];
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
            exec ${emacsBase}/bin/emacs -Q --batch -l "$TARGET"
          '';
        };
      });
  };
}
