(package-initialize)

(ert-deftest yasnippet-parsing-test ()
  (lsp-snippet-yasnippet-lsp-mode-init)
  (should
   (equal (lsp-snippet-parse "printf(${1:some_thing})$0")
           "printf(${1:some_thing})${0}")))

(ert-deftest yasnippet-escape-test ()
  (lsp-snippet-yasnippet-lsp-mode-init)
  (should
   (equal (lsp-snippet-parse "`printf(${1:\\$some_thing})$0")
           "\\`printf(${1:\\$some_thing})${0}")))

(ert-deftest yasnippet-var ()
  (lsp-snippet-yasnippet-lsp-mode-init)
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (should
     (equal (lsp-snippet-parse "${TM_FILENAME} $test")
            "test.el test"))))

(ert-deftest yasnippet-choice ()
  (lsp-snippet-yasnippet-lsp-mode-init)
  (with-temp-buffer
    (setq buffer-file-name "test.el")
    (should
     (equal (lsp-snippet-parse "${2|right,center,left|}")
            "${2:$$(yas-choose-value '(\"right\" \"center\" \"left\"))}"))))

(lsp-snippet-yasnippet-eglot-init)
