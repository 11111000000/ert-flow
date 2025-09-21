{
  description = "ert-flow: devShell, tests, and Elisp coverage generator via undercover → lcov";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      eachSystem = f: nixpkgs.lib.genAttrs systems (system:
        let
          pkgs = import nixpkgs { inherit system; config = { allowUnfree = true; }; };

          # Emacs with needed MELPA packages
          emacsPkg = pkgs.emacsWithPackages (epkgs: with epkgs; [
            undercover
            elisp-coverage
            all-the-icons
          ]);

          testsBin = pkgs.writeShellScriptBin "ert-flow-tests" ''
            set -euo pipefail
            # Run project tests (ERT); CWD is the project root
            exec ${emacsPkg}/bin/emacs -Q --batch -L lisp -l tests/run-tests.el
          '';

          coverageBin = pkgs.writeShellScriptBin "ert-flow-coverage" ''
            set -euo pipefail
            mkdir -p coverage
            # Run coverage harness (undercover → coveralls.json → lcov.info)
            exec ${emacsPkg}/bin/emacs -Q --batch -L lisp -l scripts/coverage.el
          '';
        in rec {
          devShells.default = pkgs.mkShell {
            packages = [
              emacsPkg
              pkgs.git
              pkgs.ripgrep
              pkgs.fd
              pkgs.coreutils
              pkgs.gnused
            ];
          };

          apps.tests = {
            type = "app";
            program = "${testsBin}/bin/ert-flow-tests";
          };

          apps.coverage = {
            type = "app";
            program = "${coverageBin}/bin/ert-flow-coverage";
          };

          # Optional: run tests in CI as a flake check
          checks.tests = pkgs.runCommand "ert-flow-tests" { buildInputs = [ emacsPkg ]; } ''
            set -euo pipefail
            cp -r ${self} src
            cd src
            ${emacsPkg}/bin/emacs -Q --batch -L lisp -l tests/run-tests.el
            touch $out
          '';
        });
    in {
      devShells = eachSystem (s: s.devShells);
      apps      = eachSystem (s: s.apps);
      checks    = eachSystem (s: s.checks);
    };
}
