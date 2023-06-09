EMACS ?= emacs

byte-compile:
	$(EMACS) \
	        --no-init-file \
		--batch \
		--load=lsp-snippet.el \
		--eval="(package-initialize)" \
		--eval="(package-refresh-contents)" \
		--eval="(package-install 'tempel)" \
		--eval="(package-install 'yasnippet)" \
		--eval="(setq byte-compile-error-on-warn t)" \
		--eval="(batch-byte-compile)" \
		./lsp-snippet*.el

autoloads: byte-compile
	$(EMACS) \
	        --no-init-file \
		--batch \
		--eval='(setq make-backup-files nil)' \
		--eval='(make-directory-autoloads default-directory "lsp-snippet-autoloads.el")'

test-%: autoloads
	EMACS_TEST_VERBOSE=t $(EMACS) \
		-batch \
		--no-init-file \
		--eval="(add-to-list 'load-path \"$(PWD)\")" \
		--load lsp-snippet-autoloads.el \
		--load test/test-$*.el \
		-f ert-run-tests-batch-and-exit

test: test-parse test-tempel test-yasnippet
