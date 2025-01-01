(require 'lsp-snippet)

(setq ert-batch-print-level 10)
(setq ert-batch-print-length 15)

(ert-deftest scanner-one-char-test ()
  (let ((scanner (lsp-snippet-scanner--scan "$:,{}\\/|+-?")))
    (should
     (equal (iter-next scanner)
            `(dollar 0 1)))
    (should
     (equal (iter-next scanner)
            `(colon 1 2)))
    (should
     (equal (iter-next scanner)
            `(comma 2 3)))
    (should
     (equal (iter-next scanner)
            `(curly-open 3 4)))
    (should
     (equal (iter-next scanner)
            `(curly-close 4 5)))
    (should
     (equal (iter-next scanner)
            `(backslash 5 6)))
    (should
     (equal (iter-next scanner)
            `(forwardslash 6 7)))
    (should
     (equal (iter-next scanner)
            `(pipe 7 8)))
    (should
     (equal (iter-next scanner)
            `(plus 8 9)))
    (should
     (equal (iter-next scanner)
            `(minus 9 10)))
    (should
     (equal (iter-next scanner)
            `(question-mark 10 11)))
    (should
     (equal (iter-next scanner)
            `(eof 11 11)))))

(ert-deftest scanner-number ()
  (let ((scanner (lsp-snippet-scanner--scan " 123")))
    (should
     (equal (iter-next scanner)
            `(other 0 1)))
    (should
     (equal (iter-next scanner)
            `(number 1 4)))
    (should
     (equal (iter-next scanner)
            `(eof 4 4)))))

(ert-deftest scanner-var ()
  (let ((scanner (lsp-snippet-scanner--scan "var _var VAR VAR1 1var")))
    (should
     (equal (iter-next scanner)
            `(var 0 3)))
    (should
     (equal (iter-next scanner)
            `(other 3 4)))
    (should
     (equal (iter-next scanner)
            `(var 4 8)))
    (should
     (equal (iter-next scanner)
            `(other 8 9)))
    (should
     (equal (iter-next scanner)
            `(var 9 12)))
    (should
     (equal (iter-next scanner)
            `(other 12 13)))
    (should
     (equal (iter-next scanner)
            `(var 13 17)))
    (should
     (equal (iter-next scanner)
            `(other 17 18)))
    (should
     (equal (iter-next scanner)
            `(number 18 19)))
    (should
     (equal (iter-next scanner)
            `(var 19 22)))
    (should
     (equal (iter-next scanner)
            `(eof 22 22)))))

(ert-deftest parser-text-test ()
  (should
   (equal (lsp-snippet-parse " test")
          '((text " test"))))
  (should
   (equal (lsp-snippet-parse " test\\$ test")
          '((text " test\\$ test")))))

(ert-deftest parser-tabstop-test ()
  (should
   (equal (lsp-snippet-parse "$0")
          '((tabstop 0))))
  (should
   (equal (lsp-snippet-parse "${0}")
          '((tabstop 0))))
  (should
   (equal (lsp-snippet-parse "$1test")
          '((tabstop 1) (text "test"))))
  (should
   (equal (lsp-snippet-parse "${1}test")
          '((tabstop 1) (text "test")))))

(ert-deftest parser-placeholder-test ()
  (should
   (equal (lsp-snippet-parse "${1:test\\}}")
          '((placeholder 1 (text "test\\}")))))
  (should
   (equal (lsp-snippet-parse "${0:${1:${1:test}}}")
          '((placeholder 0 (placeholder 1 (placeholder 1 (text "test")))))))
  (should
   (equal (lsp-snippet-parse "${1:}")
          '((placeholder 1 "")))))

(ert-deftest parser-choice-test ()
  (should
   (equal (lsp-snippet-parse "${1||}")
          '((choice 1 ()))))
  (should-error
   (equal (lsp-snippet-parse "${1|}")
          '((choice 1 ()))))
  (should-error
   (equal (lsp-snippet-parse "${1||")
          '((choice 1 ()))))
  (should
   (equal (lsp-snippet-parse "${1|test|}")
          '((choice 1 ("test")))))
  (should
   (equal (lsp-snippet-parse "${1|test1,test2|}")
          '((choice 1 ("test1" "test2")))))
  (should
   (equal (lsp-snippet-parse "${1|test1,test2,test3|}")
          '((choice 1 ("test1" "test2" "test3")))))
  (should
   (equal (lsp-snippet-parse "${1|test1,\\|test2,\\,test3|}")
          '((choice 1 ("test1" "\\|test2" "\\,test3"))))))

(ert-deftest parser-variable-simple-test ()
  (should
   (equal (lsp-snippet-parse "$test")
          '((variable nil "test"))))
  (should
   (equal (lsp-snippet-parse "${test}")
          '((variable nil "test"))))
  (should
   (equal (lsp-snippet-parse "${test:test2}")
          '((variable nil (text "test2")))))
  (should
   (equal (lsp-snippet-parse "${test:${test:${test}}}")
          '((variable nil (variable nil (variable nil "test")))))))

(ert-deftest parser-variable-resolver-test ()
  (seq-do
   (lambda (var-name-and-format)
     (should (equal
              (lsp-snippet--resolve-variable (car var-name-and-format))
              (format-time-string (cdr var-name-and-format)))))
   '(("CURRENT_YEAR" . "%Y")
     ("CURRENT_YEAR_SHORT" . "%y")
     ("CURRENT_MONTH" . "%m")
     ("CURRENT_DATE" . "%d")
     ("CURRENT_HOUR" . "%h")
     ("CURRENT_MINUTE" . "%m")
     ("CURRENT_SECOND" . "%s")
     ("CURRENT_DAY_NAME" . "%A")
     ("CURRENT_DAY_NAME_SHORT" . "%a")
     ("CURRENT_MONTH_NAME" . "%B")
     ("CURRENT_MONTH_NAME_SHORT" . "%b")
     ("CURRENT_SECONDS_UNIX" . "%s")))
  (with-temp-buffer
    (insert "first line\nsecond line")
    (set-mark 2)
    (should
     (equal (lsp-snippet--resolve-variable "SELECTION")
            "irst line\nsecond line"))
    (should
     (equal (lsp-snippet--resolve-variable "TM_SELECTED_TEXT")
            "irst line\nsecond line")))
  (with-temp-buffer
    (insert "\nkilled line")
    (kill-whole-line)
    (should
     (equal (lsp-snippet--resolve-variable "CLIPBOARD")
            "killed line")))
  (with-temp-buffer
    (insert "current line")
    (should
     (equal (lsp-snippet--resolve-variable "TM_CURRENT_LINE")
            "current line"))
    (insert " currentword")
    (should
     (equal (lsp-snippet--resolve-variable "TM_CURRENT_WORD")
            "currentword")))
  (with-temp-buffer
    (insert "first line\nsecond line")
    (should
     (equal (lsp-snippet--resolve-variable "TM_LINE_INDEX")
            "1"))
    (should
     (equal (lsp-snippet--resolve-variable "TM_LINE_NUMBER")
            "2")))
  (with-temp-buffer
    (setq-local buffer-file-name "/folder/file.ext")
    (should
     (equal (lsp-snippet--resolve-variable "TM_FILENAME")
            "file.ext"))
    (should
     (equal (lsp-snippet--resolve-variable "TM_FILENAME_BASE")
            "file"))

    (should
     (equal (lsp-snippet--resolve-variable "TM_DIRECTORY")
            "/folder/"))
    (should
     (equal (lsp-snippet--resolve-variable "TM_FILEPATH")
            "/folder/file.ext")))
  (should
   (string-match-p "^[[:digit:]]\\{6\\}$"
                   (lsp-snippet--resolve-variable "RANDOM")))
  (should
   (string-match-p "^\\([[:digit:]]\\|[[:lower:]]\\)\\{6\\}$"
                   (lsp-snippet--resolve-variable "RANDOM_HEX"))))

(ert-deftest parser-variable-regexp-test ()
  (with-temp-buffer
    (setq-local buffer-file-name "/test/file.el")
    (should
     (equal (lsp-snippet-parse "${TM_FILENAME/(.*)\..+$/$1/}")
            '((variable nil "file.el"))))))

(ert-deftest parser-recursive-test ()
  (should
   (equal (lsp-snippet-parse "${1:${2:${3}}}")
          '((variable "1" (variable "2" (variable "3")))))))
