;;; lsp-snippet.el --- parse lsp-snippets -*- lexical-binding: t -*-
;;; Commentary:

;;; Parsing of lsp-snippets
;;; See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#snippet_syntax
;;; Notably transformations is not supported
;;; This is partly due to laziness and nothing meaningful can be derived
;;; from the parsed results without ~node~.

;;; Code:
(require 'generator)

(defconst lsp-snippet-scanner--char-tokens
  '((?$ . dollar)
    (?: . colon)
    (?, . comma)
    (?{ . curly-open)
    (?} . curly-close)
    (?\\ . backslash)
    (?/ . forwardslash)
    (?| . pipe)
    (?+ . plus)
    (?- . minus)
    (?? . question-mark)))

(defun lsp-snippet-scanner--number-p (char)
  (and (<= ?0 char)
       (<= char ?9)))

(defun lsp-snippet-scanner--var-start-p (char)
  (or (eq ?_ char)
      (and (<= ?A char)
           (<= char ?Z))
      (and (<= ?a char)
           (<= char ?z))))

(iter-defun lsp-snippet-scanner--scan (str)
  (let ((idx 0)
        (char) (tokens))
    (while (< idx (length str))
      (setq char (aref str idx))
      (cond
       ;; One char tokens
       ((alist-get char lsp-snippet-scanner--char-tokens)
        (iter-yield `(,(alist-get char lsp-snippet-scanner--char-tokens)
                      ,idx
                      ,(setq idx (1+ idx)))))
       ;; Number
       ((lsp-snippet-scanner--number-p char)
        (let ((start-idx idx))
          (while (and (setq idx (1+ idx))
                      (< idx (length str))
                      (lsp-snippet-scanner--number-p (aref str idx))))
          (iter-yield `(number ,start-idx ,idx))))
       ;; Variable
       ((lsp-snippet-scanner--var-start-p char)
        (let ((start-idx idx))
          (while (and (setq idx (1+ idx))
                      (< idx (length str))
                      (or (lsp-snippet-scanner--number-p (aref str idx))
                          (lsp-snippet-scanner--var-start-p (aref str idx)))))
          (iter-yield `(var ,start-idx ,idx))))
       ;; Format and any, scanning skipped as format not handled
       (t
        (let ((start-idx idx))
          (while (and (setq idx (1+ idx))
                      (< idx (length str))
                      (setq char (aref str idx))
                      (and (not (alist-get char lsp-snippet-scanner--char-tokens))
                           (not (lsp-snippet-scanner--number-p char))
                           (not (lsp-snippet-scanner--var-start-p char)))))
          (iter-yield `(other ,start-idx ,idx))))))
    (iter-yield `(eof ,idx ,idx))))

(defvar lsp-snippet--str nil)
(defvar lsp-snippet--curr nil)

(defvar lsp-snippet--concat-fn
      (lambda (elements)
        (mapcan 'identity elements)))
(defvar lsp-snippet--text-fn
      (lambda (text)
        `((text ,text))))
(defvar lsp-snippet--tabstop-fn
      (lambda (number)
        `((tabstop ,number))))
(defvar lsp-snippet--placeholder-fn
      (lambda (number placeholder)
        `((placeholder ,number ,placeholder))))
(defvar lsp-snippet--choice-fn
      (lambda (number choice)
        `((choice ,number ,choice))))
(defvar lsp-snippet--variable-fn
      (lambda (resolved fallback)
        `((variable ,resolved ,fallback))))

(iter-defun lsp-snippet--parse-generator (str)
  (let* ((scanner (lsp-snippet-scanner--scan str))
         (tokens (list))
         (lsp-snippet--str str)
         lsp-snippet--curr)
    ;; Roll out tokens
    (iter-do (token scanner)
      (setq tokens (nconc tokens (list token))))
    (setq lsp-snippet--curr tokens)
    (while (not (eq (lsp-snippet--curr-type) 'eof))
      (let ((res (lsp-snippet--any '(dollar))))
        (unless res
          (error "Possible malformed snippet %S"
                 lsp-snippet--str))
        (iter-yield res)))))

;;;###autoload
(defun lsp-snippet--parse (str)
  ;; TODO Docs
  ;; Bind transform functions
  (let ((parser (lsp-snippet--parse-generator str))
        (elements (list)))
    ;; Roll out snippet
    (iter-do (element parser)
      (setq elements (nconc elements (list element))))
    (funcall lsp-snippet--concat-fn
             elements)))

(defun lsp-snippet--take (types)
  (if (null (lsp-snippet--curr-type))
      (error "Did not expect `EOF'; possible malformed snippet %S"
             lsp-snippet--str)
    (when (member (lsp-snippet--curr-type) types)
      (apply (apply-partially 'substring
                              lsp-snippet--str)
             (cdr (pop lsp-snippet--curr))))))

(defun lsp-snippet--ignore (types)
  (if (null (lsp-snippet--curr-type))
      (error "Did not expect `EOF'; possible malformed snippet %S"
             lsp-snippet--str)
    (unless (member (lsp-snippet--curr-type) types)
      (apply (apply-partially 'substring
                              lsp-snippet--str)
             (cdr (pop lsp-snippet--curr))))))

(defun lsp-snippet--curr-type ()
  (caar lsp-snippet--curr))

(defun lsp-snippet--index ()
  (car (cdr (car lsp-snippet--curr))))

(defun lsp-snippet--revert (revert)
  (setq lsp-snippet--curr revert)
  nil)

(defun lsp-snippet--escaped-string-to (stop-at)
  (let ((start (lsp-snippet--index))
        (continue t))
    (while continue
      ;; Skip next token if `backslash'
      (when (lsp-snippet--take '(backslash))
        ;; Skip next token
        (lsp-snippet--ignore '(eof)))
      (setq continue (lsp-snippet--ignore
                      `(eof . ,stop-at))))
    (unless (eq start (lsp-snippet--index))
      (substring lsp-snippet--str start (lsp-snippet--index)))))

(defun lsp-snippet--any (&optional stop-at)
  (or (lsp-snippet--text stop-at)
      (lsp-snippet--revert-state-on-nil #'lsp-snippet--tabstop)
      (lsp-snippet--revert-state-on-nil #'lsp-snippet--placeholder)
      (lsp-snippet--revert-state-on-nil #'lsp-snippet--choice)
      (lsp-snippet--revert-state-on-nil #'lsp-snippet--variable)))

(defun lsp-snippet--revert-state-on-nil (parse-fn)
  (let ((revert lsp-snippet--curr))
    (or (funcall parse-fn)
        (and (setq lsp-snippet--curr revert) nil))))

(defun lsp-snippet--text (&optional stop-at)
  (when-let ((text (lsp-snippet--escaped-string-to stop-at)))
    (funcall lsp-snippet--text-fn text)))

(defun lsp-snippet--tabstop ()
  (when-let* ((dollar (lsp-snippet--take '(dollar)))
              (curly (or (lsp-snippet--take '(curly-open)) 'skip))
              (number (lsp-snippet--take '(number)))
              (curly-end (or (eq curly 'skip) (lsp-snippet--take '(curly-close)))))
    (funcall lsp-snippet--tabstop-fn
             (string-to-number number))))

(defun lsp-snippet--placeholder ()
  (when-let* ((dollar (lsp-snippet--take '(dollar)))
              (curly-open (lsp-snippet--take '(curly-open)))
              (number (lsp-snippet--take '(number)))
              (colon (lsp-snippet--take '(colon)))
              (placeholder (lsp-snippet--any '(dollar curly-close)))
              (curly-close (lsp-snippet--take '(curly-close))))
    (funcall lsp-snippet--placeholder-fn
             (string-to-number number)
             (car placeholder))))

(defun lsp-snippet--choice ()
  (when-let* ((dollar (lsp-snippet--take '(dollar)))
              (curly-open (lsp-snippet--take '(curly-open)))
              (number (lsp-snippet--take '(number)))
              (pipe (lsp-snippet--take '(pipe))))
    (let ((choices (list))
          start)
      ;; Collect alternatives
      (while (not (eq (lsp-snippet--curr-type) 'pipe))
        (setq start (lsp-snippet--index))
        (lsp-snippet--escaped-string-to '(comma pipe))
        (setq choices
              (nconc choices
                     (list (substring lsp-snippet--str start (lsp-snippet--index)))))
        ;; Pop comma
        (lsp-snippet--take '(comma)))
      (when (and (lsp-snippet--take '(pipe))
                 (lsp-snippet--take '(curly-close)))
        (funcall lsp-snippet--choice-fn
                 (string-to-number number)
                 choices)))))

(defun lsp-snippet--resolve-variable (var)
  (pcase var
    ("CURRENT_YEAR" (format-time-string "%Y"))
    ("CURRENT_YEAR_SHORT" (format-time-string "%y"))
    ("CURRENT_MONTH" (format-time-string "%m"))
    ("CURRENT_DATE" (format-time-string "%d"))
    ("CURRENT_HOUR" (format-time-string "%h"))
    ("CURRENT_MINUTE" (format-time-string "%m"))
    ("CURRENT_SECOND" (format-time-string "%s"))
    ("CURRENT_DAY_NAME" (format-time-string "%A"))
    ("CURRENT_DAY_NAME_SHORT" (format-time-string "%a"))
    ("CURRENT_MONTH_NAME" (format-time-string "%B"))
    ("CURRENT_MONTH_NAME_SHORT" (format-time-string "%b"))
    ("CURRENT_SECONDS_UNIX" (format-time-string "%s"))
    ("SELECTION" (buffer-substring (mark) (point)))
    ("CLIPBOARD" (car kill-ring))
    ("TM_SELECTED_TEXT" (buffer-substring (mark) (point)))
    ("TM_CURRENT_LINE" (thing-at-point 'line t))
    ("TM_CURRENT_WORD" (thing-at-point 'word t))
    ("TM_LINE_INDEX" (number-to-string (1- (line-number-at-pos))))
    ("TM_LINE_NUMBER" (number-to-string (line-number-at-pos)))
    ("TM_FILENAME" (file-name-nondirectory buffer-file-name))
    ("TM_FILENAME_BASE" (file-name-base buffer-file-name))
    ("TM_DIRECTORY" (file-name-directory buffer-file-name))
    ("TM_FILEPATH" buffer-file-name)
    ("RELATIVE_FILEPATH" nil)
    ("BLOCK_COMMENT_START" nil)
    ("BLOCK_COMMENT_END" nil)
    ("LINE_COMMENT" nil)
    ("WORKSPACE_NAME" nil)
    ("WORKSPACE_FOLDER" nil)
    ("RANDOM" (format "%06o" (random 100000)))
    ("RANDOM_HEX" (format "%06x" (random 16777216)))
    ("UUID" nil)))

(defun lsp-snippet--variable-any ()
  (when-let* ((dollar (lsp-snippet--take '(dollar)))
              (curly-open (lsp-snippet--take '(curly-open)))
              (var (lsp-snippet--take '(var)))
              (colon (lsp-snippet--take '(colon)))
              (any (lsp-snippet--any '(dollar curly-close)))
              (curly-close (lsp-snippet--take '(curly-close))))
    (funcall lsp-snippet--variable-fn
             (lsp-snippet--resolve-variable var)
             (car any))))

(defun lsp-snippet--variable-simple ()
  (when (lsp-snippet--take '(dollar))
    (let* ((curly-open (lsp-snippet--take '(curly-open)))
           (var (lsp-snippet--take '(var))))
      (when (and var (or (not curly-open)
                         (lsp-snippet--take '(curly-close))))
        (funcall lsp-snippet--variable-fn
                 (lsp-snippet--resolve-variable var)
                 var)))))

(defun lsp-snippet--variable-regex ()
  (when-let* ((dollar (lsp-snippet--take '(dollar)))
              (curly-open (lsp-snippet--take '(curly-open)))
              (var (lsp-snippet--take '(var)))
              (forwardslash (lsp-snippet--take '(forwardslash)))
              ;; Eat the two ?/
              (until-forwardslash (lsp-snippet--escaped-string-to '(forwardslash)))
              (forwardslash (lsp-snippet--take '(forwardslash)))
              (until-forwardslash (lsp-snippet--escaped-string-to '(forwardslash)))
              (forwardslash (lsp-snippet--take '(forwardslash)))
              ;; Eat the closing ?}
              (until-curly-close (or (lsp-snippet--escaped-string-to '(curly-close)) t))
              (curly-close (lsp-snippet--take '(curly-close))))
    (funcall lsp-snippet--variable-fn
             nil
             (or (lsp-snippet--resolve-variable var) var))))

(defun lsp-snippet--variable ()
  (or (lsp-snippet--revert-state-on-nil #'lsp-snippet--variable-simple)
      (lsp-snippet--revert-state-on-nil #'lsp-snippet--variable-any)
      (lsp-snippet--revert-state-on-nil #'lsp-snippet--variable-regex)))

(provide 'lsp-snippet)
