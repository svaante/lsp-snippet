;;(require 'lsp-snippet-tempel)

(ert-deftest lsp-mode-snippet-tempel-parsing-test ()
  (lsp-snippet-tempel-lsp-mode-init)
  (with-temp-buffer
    (insert "printf(${1:some_thing})$0")
    (lsp-snippet-tempel--lsp-mode-expand-snippet nil (point-min) (point-max) nil)
    (should (equal (substring-no-properties (buffer-string))
                   "printf(some_thing)"))))

(ert-deftest lsp-mode-snippet-tempel-parsing-format-str-test ()
  (lsp-snippet-tempel-lsp-mode-init)
  (should
   (equal (lsp-snippet-parse "switch (${1:expression}) {
$0
}")
          '("switch (" (p "expression") ") {" n> "" p q "" n> "}"))))
