(package-initialize)

(ert-deftest tempel-parsing-test ()
  (lsp-snippet-tempel-lsp-mode-init)
  (should
   (should
   (equal (lsp-snippet-parse "printf(${1:some_thing})$0")
          '("printf(" > (p "some_thing" tabstop-1) ")" > q)))))

(ert-deftest tempel-parsing-format-str-test ()
  (lsp-snippet-tempel-lsp-mode-init)
  (should
   (equal (lsp-snippet-parse "switch (${1:expression}) {
$0
}")
          '("switch (" > (p "expression" tabstop-1) ") {" > n > q > n > "}" >)))
  (should
   (equal (lsp-snippet-parse "if let $1 = $2 {
    $0
}")
          '("if let " > (p "" tabstop-1) " = " > (p "" tabstop-2) " {" > n > q > n > "}" >))))

(ert-deftest lsp-mode-tempel-parsing-test ()
  (lsp-snippet-tempel-lsp-mode-init)
  (with-temp-buffer
    (insert "printf(${1:some_thing})$0")
    (lsp-snippet-tempel--lsp-mode-expand-snippet nil (point-min) (point-max) nil)
    (should (equal (substring-no-properties (buffer-string))
                   "printf(some_thing)"))))
