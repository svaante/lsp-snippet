EMACS ?= emacs

byte-compile:
	$(EMACS) \
		--batch \
		--load=lsp-snippet.el \
		--eval="(package-initialize)" \
		--eval="(package-refresh-contents)" \
		--eval="(package-install 'tempel)" \
		--eval="(setq byte-compile-error-on-warn t)" \
		--eval="(batch-byte-compile)" \
		./lsp-snippet*.el

autoloads:
	$(EMACS) \
		--batch \
		--eval='(setq make-backup-files nil)' \
		--eval='(make-directory-autoloads default-directory "lsp-snippet-autoloads.el")'

test: byte-compile
	EMACS_TEST_VERBOSE=t $(EMACS) \
		-batch \
		--load lsp-snippet.elc \
		--load test-parse.el \
		-f ert-run-tests-batch-and-exit
