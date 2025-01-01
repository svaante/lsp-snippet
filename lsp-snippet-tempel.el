;;; lsp-snippet-tempel.el --- Use tempel as your LSP snippet enging -*- lexical-binding: t -*-
;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Daniel Pettersson
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; Created: 2023
;; License: GPL-3.0-or-later
;; Version: 0.0.1
;; Homepage: https://github.com/svaante/lsp-snippet
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Parse LSP snippets to `tempel' snippets and provide integration
;; with your chosen LSP package.

;; `eglot'    - `lsp-snippet-tempel-eglot-init'
;; `lsp-mode' - `lsp-snippet-tempel-lsp-mode-init'
;; `lspce'    - `lsp-snippet-tempel-lspce-init'

;;; Code:
(require 'tempel)
(require 'lsp-snippet)

(defun lsp-snippet-tempel--concat-fn (elements)
  (mapcan 'identity elements))

(defun lsp-snippet-tempel--text-fn (text)
  ;; HACK This is a bit of a mess and will certainly not work
  ;;      satisfactory for all situations.
  (let (after-newline-p)
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

(defun lsp-snippet-tempel--lspce-expand-snippet ()
  (lambda (snippet &optional _expand-env)
    (let* ((inhibit-field-text-motion t)
           (template (lsp-snippet-parse snippet)))
      (when template
        (tempel--insert template (cons (point) (point)))))))

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
  "Use `tempel' as `lsp-mode's snippet engine."
  (lsp-snippet-tempel--init)
  (advice-add 'lsp--expand-snippet :override #'lsp-snippet-tempel--lsp-mode-expand-snippet)
  ;; HACK `lsp-mode' enables snippet based on (fboundp 'yas-minor-mode)
  (unless (fboundp 'yas-minor-mode)
    (defun yas-minor-mode (&rest _)
      (error "Stub created by `lsp-snippet-tempel-lsp-mode-init'"))))

;;;###autoload
(defun lsp-snippet-tempel-lspce-init ()
  "Use `tempel' as `lspce's snippet engine."
  (lsp-snippet-tempel--init)
  (advice-add 'lspce--snippet-expansion-fn :override #'lsp-snippet-tempel--lspce-expand-snippet)
  ;; HACK `lspce' enables snippet based on `(feature 'yasnippet)'
  (provide 'yasnippet))

;;;###autoload
(defun lsp-snippet-tempel-eglot-init ()
  "Use `tempel' as `eglot's snippet engine."
  (lsp-snippet-tempel--init)
  (advice-add 'eglot--snippet-expansion-fn :override #'lsp-snippet-tempel--eglot-expand-snippet))

(provide 'lsp-snippet-tempel)
;;; lsp-snippet-tempel.el ends here
