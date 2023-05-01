EMACS ?= emacs

test:
	EMACS_TEST_VERBOSE=t $(EMACS) \
		-batch \
		--load lsp-snippet.el \
		--load test-parse.el \
		-f ert-run-tests-batch-and-exit
