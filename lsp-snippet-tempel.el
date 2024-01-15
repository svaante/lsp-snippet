;;; lsp-snippet-tempel.el --- convert lsp snippets to tempel snippets -*- lexical-binding: t -*-
;;; Commentary:

;;; Shoving in tempel snippet support into `lsp-mode' in the worst way possible
;;; Call `lsp-snippet-tempel-lsp-mode-init' to enable support globally

;;; Code:
(require 'tempel)
(require 'lsp-snippet)

(defun lsp-snippet-tempel--concat-fn (elements)
  (mapcan 'identity elements))

(defun lsp-snippet-tempel--text-fn (text)
  ;; HACK This is a bit of a mess and will certainly not work
  ;;      satisfactory for all situations.
  (let (after-newline-p )
    (mapcan (lambda (line)
              (let ((left-trim (string-trim-left line)))
                (cond
                 ((not after-newline-p)
                  (setq after-newline-p t)
                  (if (string-empty-p line)
                      (list '>)
                    (list line '>)))
                 ((equal left-trim "")
                  (list 'n '>))
                 (t
                  (list 'n '> left-trim '>)))))
            (split-string text "\n"))))

(defun lsp-snippet-tempel--tabstop-fn (number)
  (cond
   ((zerop number)
    (list 'q))
   (t
    (lsp-snippet-tempel--placeholder-fn number ""))))

(defun lsp-snippet-tempel--choice-fn (number choices)
  (lsp-snippet-tempel--placeholder-fn number (string-join choices ",")))

(defun lsp-snippet-tempel--placeholder-fn (number placeholder)
  (if (zerop number)
      (list 'q)
    (let ((sym (intern (format "tabstop-%d" number))))
      `((p ,placeholder ,sym)))))

(defun lsp-snippet-tempel--variable-fn (resolved fallback)
  (if resolved
      resolved
    `((p ,fallback))))

(defun lsp-snippet-tempel--lsp-mode-expand-snippet (_snippet &optional start end _expand-env)
  (let* ((inhibit-field-text-motion t)
         (snippet (buffer-substring-no-properties start end))
         (template (lsp-snippet-parse snippet)))
    (when template
      (delete-region start end)
      (tempel--insert template (cons start end)))))

(defun lsp-snippet-tempel--eglot-expand-snippet ()
  (lambda (snippet &optional start end)
    (let ((template (lsp-snippet-parse snippet)))
      (when template
        (tempel--insert template (cons (or start (point))
                                       (or end (point))))))))

(defun lsp-snippet-tempel--init ()
  (setq lsp-snippet--concat-fn #'lsp-snippet-tempel--concat-fn)
  (setq lsp-snippet--text-fn #'lsp-snippet-tempel--text-fn)
  (setq lsp-snippet--tabstop-fn #'lsp-snippet-tempel--tabstop-fn)
  (setq lsp-snippet--placeholder-fn #'lsp-snippet-tempel--placeholder-fn)
  (setq lsp-snippet--choice-fn #'lsp-snippet-tempel--choice-fn)
  (setq lsp-snippet--variable-fn #'lsp-snippet-tempel--variable-fn))

;;;###autoload
(defun lsp-snippet-tempel-lsp-mode-init ()
  (lsp-snippet-tempel--init)
  (advice-add 'lsp--expand-snippet :override #'lsp-snippet-tempel--lsp-mode-expand-snippet)
  ;; HACK `lsp-mode' enables snippet based on `(feature 'yasnippet)'
  (provide 'yasnippet))

;;;###autoload
(defun lsp-snippet-tempel-eglot-init ()
  (lsp-snippet-tempel--init)
  (advice-add 'eglot--snippet-expansion-fn :override #'lsp-snippet-tempel--eglot-expand-snippet))

(provide 'lsp-snippet-tempel)
