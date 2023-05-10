;;; lsp-snippet-tempel.el --- convert lsp snippets to tempel snippets -*- lexical-binding: t -*-
;;; Commentary:

;;; Shoving in tempel snippet support into `lsp-mode' in the worst way possible
;;; Call `lsp-snippet-tempel-lsp-mode-init' to enable support globaly

;;; Code:
(require 'tempel)
(require 'lsp-snippet)

(defun lsp-snippet-tempel--concat-fn (elements)
  (mapcan 'identity elements))

(defun lsp-snippet-tempel--text-fn (text)
  (cdr (mapcan (lambda (part)
                 (list 'n> part))
               (split-string text "\n"))))

(defun lsp-snippet-tempel--tabstop-fn (number)
  `(p . ,(when (eql number 0) (list 'q))))

(defun lsp-snippet-tempel--placeholder-fn (number placeholder)
  `((p ,placeholder) . ,(when (eql number 0) (list 'q))))

(defun lsp-snippet-tempel--choice-fn (number choices)
  `((p ,(string-join choices ",")) . ,(when (eql number 0) (list 'q))))

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

(defvar lsp-snippet-tempel--allow-modification-guard nil)

(defun lsp-snippet-tempel--allow-modification-hack (&rest app)
  (unless (bound-and-true-p lsp-snippet-tempel--allow-modification-guard)
    (let ((lsp-snippet-tempel--allow-modification-guard t)
          (inhibit-modification-hooks nil))
      (apply app))))

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
  ;; HACK `tempel' removes the placeholder string as in `(p "placeholder")'
  ;; inside a modification hook. As `inhibit-modification-hooks' is non-nil the
  ;; changes won't propagate to `lsp-mode' and buffer contents and the lsp
  ;; server will diverge. To circumvent this we need to add enable modification-hooks
  ;; and guard against stack overflow as the current implementation of `tempel--field-modified'
  ;; will trigger modification hooks.
  (advice-add 'tempel--field-modified :around #'lsp-snippet-tempel--allow-modification-hack)
  (advice-add 'lsp--expand-snippet :override #'lsp-snippet-tempel--lsp-mode-expand-snippet)
  ;; HACK `lsp-mode' enables snippet based on `(feature 'yasnippet)'
  (provide 'yasnippet))

;;;###autoload
(defun lsp-snippet-tempel-eglot-init ()
  (lsp-snippet-tempel--init)
  ;; HACK `tempel' removes the placeholder string as in `(p "placeholder")'
  ;; inside a modification hook. As `inhibit-modification-hooks' is non-nil the
  ;; changes won't propagate to `lsp-mode' and buffer contents and the lsp
  ;; server will diverge. To circumvent this we need to add enable modification-hooks
  ;; and guard against stack overflow as the current implementation of `tempel--field-modified'
  ;; will trigger modification hooks.
  (advice-add 'tempel--field-modified :around #'lsp-snippet-tempel--allow-modification-hack)
  (advice-add 'eglot--snippet-expansion-fn :override #'lsp-snippet-tempel--eglot-expand-snippet))

(provide 'lsp-snippet-tempel)
