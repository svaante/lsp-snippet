;;(require 'lsp-snippet-tempel)

(ert-deftest lsp-mode-snippet-tempel-parsing-test ()
  (lsp-snippet-tempel-lsp-mode-init)
  (with-temp-buffer
    (insert "printf($1)$0")
    (lsp-snippet-tempel--expand-snippet nil (point-min) (point-max) nil)
    (should (equal (substring-no-properties (buffer-string))
                   "printf()"))
    ))
