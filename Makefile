EMACS ?= emacs

.PHONY: test
test:
	$(EMACS) -Q --batch -L . -l test/gdisco-test.el -f ert-run-tests-batch-and-exit

.PHONY: test-live
test-live:
	GDISCO_LIVE_TESTS=1 $(EMACS) -Q --batch -L . -l test/gdisco-test.el -f ert-run-tests-batch-and-exit

.PHONY: compile
compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile gdisco.el
