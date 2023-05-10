(ert-deftest tempel-parsing-test ()
  (lsp-snippet-tempel-lsp-mode-init)
  (should
   (should
   (equal (lsp-snippet-parse "printf(${1:some_thing})$0")
          '("printf(" > (p "some_thing") ")" > p q)))))

(ert-deftest tempel-parsing-format-str-test ()
  (lsp-snippet-tempel-lsp-mode-init)
  (should
   (equal (lsp-snippet-parse "switch (${1:expression}) {
$0
}")
          '("switch (" > (p "expression") ") {" > n "" > p q "" > n "}" >))))

(ert-deftest lsp-mode-tempel-parsing-test ()
  (lsp-snippet-tempel-lsp-mode-init)
  (with-temp-buffer
    (insert "printf(${1:some_thing})$0")
    (lsp-snippet-tempel--lsp-mode-expand-snippet nil (point-min) (point-max) nil)
    (should (equal (substring-no-properties (buffer-string))
                   "printf(some_thing)"))))
