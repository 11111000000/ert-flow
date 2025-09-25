# Simple runner for tests and coverage (works with or without Cask)
EMACS ?= emacs
# If the caller exported CASK_EMACS (e.g. from Nix profile), prefer it as the Emacs binary.
ifeq ($(origin CASK_EMACS), environment)
EMACS := $(CASK_EMACS)
endif
CASK  := $(shell command -v cask 2>/dev/null)
EMACS_CMD = $(if $(CASK),$(CASK) exec $(EMACS),$(EMACS))

.PHONY: test coverage clean-coverage

test:
	@echo "==> Running tests with: $(if $(CASK),cask exec ,)$(EMACS)"
	$(EMACS_CMD) -Q --batch -l tests/run-tests.el

coverage:
	@echo "==> Generating coverage with: $(if $(CASK),cask exec ,)$(EMACS)"
	@mkdir -p coverage
ifeq ($(CASK),)
	@echo "==> No cask found — loading undercover from ~/.emacs.d/elpa"
	UNDERCOVER_FORCE=1 UNDERCOVER_CONFIG="(\"lisp/*.el\" (:exclude \"tests/*.el\") (:report-format 'lcov) (:report-file \"coverage/lcov.info\") (:send-report nil) (:merge-report nil))" $(EMACS) -Q --batch -L $(HOME)/.emacs.d/elpa -l undercover -l scripts/coverage.el
else
	UNDERCOVER_FORCE=1 UNDERCOVER_CONFIG="(\"lisp/*.el\" (:exclude \"tests/*.el\") (:report-format 'lcov) (:report-file \"coverage/lcov.info\") (:send-report nil) (:merge-report nil))" $(EMACS_CMD) -Q --batch -l scripts/coverage.el
endif
	@echo "==> Verifying coverage/lcov.info"
	@test -f coverage/lcov.info && echo "coverage: coverage/lcov.info ready" || (echo "coverage: lcov.info missing" && exit 1)

clean-coverage:
	rm -rf coverage
	@echo "coverage: cleaned"
