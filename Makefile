# Simple runner for tests and coverage (works with or without Cask)
EMACS ?= emacs
CASK  := $(shell command -v cask 2>/dev/null)
EMACS_CMD = $(if $(CASK),$(CASK) exec $(EMACS),$(EMACS))

.PHONY: test coverage clean-coverage

test:
	@echo "==> Running tests with: $(if $(CASK),cask exec ,)$(EMACS)"
	$(EMACS_CMD) -Q --batch -l tests/run-tests.el

coverage:
	@echo "==> Generating coverage with: $(if $(CASK),cask exec ,)$(EMACS)"
	$(EMACS_CMD) -Q --batch -l scripts/coverage.el
	@echo "==> Verifying coverage/lcov.info"
	@test -f coverage/lcov.info && echo "coverage: coverage/lcov.info ready" || (echo "coverage: lcov.info missing" && exit 1)

clean-coverage:
	rm -rf coverage
	@echo "coverage: cleaned"
