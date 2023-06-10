;;; lsp-snippet-yasnippet.el --- convert lsp snippets to yasnippet snippets -*- lexical-binding: t -*-
;;; Commentary:

;;; Shoving in yasnippet snippet support into `lsp-mode' in the worst way possible
;;; Call `lsp-snippet-yasnippet-lsp-mode-init' to enable support globaly

;;; Code:
(require 'yasnippet)
(require 'lsp-snippet)

(defun lsp-snippet-yasnippet--escape (text)
  "Escape ?` TEXT with ?\\"
  (replace-regexp-in-string "\\(`\\)"
                            "\\\\\\1"
                            text
                            nil nil 1))

(defun lsp-snippet-yasnippet--concat-fn (elements)
  (apply #'concat (mapcar #'car elements)))

(defun lsp-snippet-yasnippet--text-fn (text)
  (list
   (lsp-snippet-yasnippet--escape text)))

(defun lsp-snippet-yasnippet--tabstop-fn (number)
  (list
   (format "${%s}" number)))

(defun lsp-snippet-yasnippet--choice-fn (number choices)
  (list
   (format "${%s:$$(yas-choose-value '(%s))}"
           number
           (mapconcat
            (lambda (choice)
              (format "\"%s\""
                      (lsp-snippet-yasnippet--escape choice)))
            choices
            " "))))

(defun lsp-snippet-yasnippet--placeholder-fn (number placeholder)
  (list
   (format "${%s:%s}" number placeholder)))

(defun lsp-snippet-yasnippet--variable-fn (resolved fallback)
  (message "THIS %s %s" resolved fallback)
  (list
   (lsp-snippet-yasnippet--escape
    (or resolved fallback ""))))

(defun lsp-snippet-yasnippet--lsp-mode-expand-snippet (_snippet &optional start end expand-env)
  (let* ((inhibit-field-text-motion t)
         (snippet (buffer-substring-no-properties start end))
         (template (lsp-snippet-parse snippet)))
    (when template
      (delete-region start end)
      (yas-expand-snippet snippet start end expand-env))))

(defun lsp-snippet-yasnippet--eglot-expand-snippet ()
  (lambda (snippet &optional start end)
    (let ((template (lsp-snippet-parse snippet)))
      (when template
        (yas-expand-snippet snippet start end)))))

(defun lsp-snippet-yasnippet--init ()
  (setq lsp-snippet--concat-fn #'lsp-snippet-yasnippet--concat-fn)
  (setq lsp-snippet--text-fn #'lsp-snippet-yasnippet--text-fn)
  (setq lsp-snippet--tabstop-fn #'lsp-snippet-yasnippet--tabstop-fn)
  (setq lsp-snippet--placeholder-fn #'lsp-snippet-yasnippet--placeholder-fn)
  (setq lsp-snippet--choice-fn #'lsp-snippet-yasnippet--choice-fn)
  (setq lsp-snippet--variable-fn #'lsp-snippet-yasnippet--variable-fn))

;;;###autoload
(defun lsp-snippet-yasnippet-lsp-mode-init ()
  (lsp-snippet-yasnippet--init)
  (advice-add 'lsp--expand-snippet :override #'lsp-snippet-yasnippet--lsp-mode-expand-snippet))

;;;###autoload
(defun lsp-snippet-yasnippet-eglot-init ()
  (lsp-snippet-yasnippet--init)
  (advice-add 'eglot--snippet-expansion-fn :override #'lsp-snippet-yasnippet--eglot-expand-snippet))

(provide 'lsp-snippet-yasnippet)
