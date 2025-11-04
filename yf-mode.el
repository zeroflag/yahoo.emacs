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
    (define-key map (kbd "C-c C-e") 'yf-eval-line)
    (define-key map (kbd "C-c C-r") 'yf-eval-region)
    (define-key map (kbd "C-c C-b") 'yf-eval-buffer)
    (define-key map (kbd "C-c C-d") 'yf-delete-overlays)
    map)
  "Key map for YF majod mode.")

(defvar yf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "<" st)
    (modify-syntax-entry ?\) ">" st)
    st))

(defconst yf-builtin-symbols
  '("+" "-" "*" "/" "?" "." ".s" "|" ">" ">=" "<" "<="))

(defun yf-mode-builtin-words ()
  (unless yf-word-list
    (yf-eval "")) ; initialize dictionary with a dummy eval
  (let ((result nil))
    (dolist (each yf-word-list)
      (unless (member each yf-builtin-symbols)
        (push each result)))
    result))

(defun yf-mode-completion ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let* ((start (car bounds))
             (end (cdr bounds))
             (words (yf-mode-builtin-words))
             (all (append words yf-currency-codes)))
        (list start end all)))))

(defvar yf-font-lock-defaults
  `((,(regexp-opt yf-currency-codes 'words) . font-lock-keyword-face)
    (,(concat "\\<" yf-ticker-regexp "\\>") . font-lock-type-face)
    ("\\<-?[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
    (,(regexp-opt
       (mapcar #'upcase (yf-mode-builtin-words)) 'words) . font-lock-builtin-face)
    (,(regexp-opt
       (mapcar #'downcase (yf-mode-builtin-words)) 'words) . font-lock-builtin-face)
    (,(regexp-opt yf-builtin-symbols) . font-lock-builtin-face)))

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
