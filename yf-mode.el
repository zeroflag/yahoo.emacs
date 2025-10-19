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

(defvar yf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "<" st)
    (modify-syntax-entry ?\) ">" st)
    st))

(defvar yf-mode-builtins nil
  "Yf built-in words for syntax highlight")

(defun yf-mode-builtin-words ()
  (unless yf-mode-builtins
    (yf-eval "") ; initialize dictionary
    (setq yf-mode-builtins (yf-word-list)))
  yf-mode-builtins)

(defun yf-mode-completion ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let ((start (car bounds))
            (end (cdr bounds))
            (candidates (yf-mode-builtin-words)))
        (list start end candidates)))))

(defvar yf-font-lock-defaults
  `((,(regexp-opt yf-currency-codes 'words) . font-lock-keyword-face)
    (,(concat "\\<" yf-ticker-regexp "\\>") . font-lock-type-face)
    ("\\<-?[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
    (,(regexp-opt (yf-mode-builtin-words)) . font-lock-builtin-face)))

(define-derived-mode yf-mode prog-mode "YF"
  "Major mode for .yf files."
  :syntax-table yf-mode-syntax-table
  (setq-local font-lock-defaults `(,yf-font-lock-defaults))
  (setq-local completion-at-point-functions '(yf-mode-completion t))
  (setq-local comment-start "(")
  (setq-local comment-end ")"))


(provide 'yf-mode)

;;Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; yf-mode.el ends here
