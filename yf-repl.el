;;; -*- lexical-binding: t; -*-
;;; yf-repl.el --- Financial utilities
;;;
;;; Author: Attila Magyar
;;; URL: http://github.com/zeroflag/yahoo.emacs
;;; Version: 0.1
;;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;;; yf repl mode
;;;
;;; Code:
;;;
(require 'yf)

(defvar yf-repl-stack '())
(defconst yf-repl-buffer-name "*Yahoo Finance REPL*")
(defconst yf-repl-prompt "(yf) $")

(defun yf-repl-insert-prompt ()
  (insert (propertize
           yf-repl-prompt
           'face '(:foreground "magenta" :weight bold)))
  (insert " "))

(defun yf-repl-read-input ()
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun yf-repl-on-line-entered ()
  (interactive)
  (let* ((input (yf-repl-read-input))
         (input (if (string-prefix-p yf-repl-prompt input)
                    (substring input (length yf-repl-prompt))
                  input))
         (input (string-trim input)))
    (setq yf-repl-stack (yf-eval input yf-repl-stack)))
  (goto-char (point-max))
  (insert "\n" (yf-show-stack yf-repl-stack) "\n")
  (yf-repl-insert-prompt))

(defvar yf-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'yf-repl-on-line-entered)
    map)
  "Keymap for `yf-repl-mode'.")

(define-derived-mode yf-repl-mode fundamental-mode "YAHOO-FINANCE-REPL"
  (setq-local inhibit-read-only t)
  (setq-local truncate-lines t)
  (use-local-map yf-repl-mode-map))

(defun yf-repl-start ()
  "Start the Yahoo Finance REPL."
  (interactive)
  (setq yf-repl-stack '())
  (message "Starting Yahoo Finace REPL..")
  (let ((buf (get-buffer-create yf-repl-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'yf-repl-mode)
        (yf-repl-mode)
        (erase-buffer)
        (yf-insert-prompt)))
    (pop-to-buffer buf)))

(provide 'yf-repl)

;;Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; yf-repl.el ends here
