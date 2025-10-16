;;; -*- lexical-binding: t; -*-
;;; yf-mode.el --- Financial utilities
;;;
;;; Author: Attila Magyar
;;; URL: http://github.com/zeroflag/yahoo.emacs
;;; Version: 0.1
;;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;;; YF mode
;;;
;;; Code:
;;;
(require 'yf)

(defvar yf-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Example binding: (define-key map (kbd "C-c C-c") #'yf-eval-buffer)
    map)
  "Keymap for `yf-mode'.")

(defvar yf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "<" st)
    (modify-syntax-entry ?\) ">" st)
    st))

(defvar yf-builtin-words
  '("to" "clear" "depth" "dup" "swap"
    "over" "sum" "sumprod" "message"))

(defvar yf-builtin-symbols
  '("+" "-" "*" "/" "?" "." ".s"))

(defvar yf-font-lock-defaults
  `((,(regexp-opt yf-currency-codes 'words) . font-lock-keyword-face)
    (,(regexp-opt yf-builtin-words 'words) . font-lock-builtin-face)
    (,(concat "\\<" yf-ticker-regexp "\\>") . font-lock-type-face)
    ("\\<-?[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
    (,(regexp-opt yf-builtin-symbols) . font-lock-builtin-face)))

(defun yf-indent-line ()
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space))

(define-derived-mode yf-mode prog-mode "YF"
  "Major mode for .yf files."
  :syntax-table yf-mode-syntax-table
  (setq-local font-lock-defaults `(,yf-font-lock-defaults))
  (setq-local comment-start "(")
  (setq-local comment-end ")")
  (setq-local indent-line-function #'yf-indent-line))

(provide 'yf-mode)

;;Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; yf-mode.el ends here
