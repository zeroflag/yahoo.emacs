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

(require 'comint)
(require 'yf)
(require 'yf-mode)

(defvar-local yf-repl-last-prompt-end 0)

(defconst yf-repl-name "*YF-REPL*")
(defconst yf-repl-buffer-name (concat "*" yf-repl-name "*"))
(defconst yf-repl-prompt "YF % ")

(defvar yf-repl-history-file
  (expand-file-name "yf-repl-history" user-emacs-directory)
  "File to save command history for `yf-repl-mode'.")

(defun yf-repl-clear ()
  (yf-delete-overlays)
  (comint-clear-buffer))

(defun yf-repl--input-sender (proc input)
  (if (string= input "cls")
      (yf-repl-clear)
    (yf-eval input (- yf-repl-last-prompt-end 1)))
  (let ((output (yf-show-stack)))
    (comint-output-filter proc (concat output "\n"))
    (comint-output-filter proc yf-repl-prompt))
  (setq yf-repl-last-prompt-end (point)))

(define-derived-mode yf-repl-mode comint-mode "YF-REPL"
  :syntax-table yf-mode-syntax-table
  (setq-local font-lock-defaults `(,yf-font-lock-defaults))
  (setq-local completion-at-point-functions '(yf-mode-completion t))
  (setq-local comment-start "(")
  (setq-local comment-end ")")
  (setq-local comint-prompt-regexp (concat "^" (regexp-quote yf-repl-prompt)))
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-input-sender #'yf-repl--input-sender)
  (setq-local comint-highlight-input nil)
  (setq-local comint-process-echoes nil)
  (setq-local comint-prompt-read-only t))

(defun yf-repl-start ()
  "Start the Yahoo Finance REPL."
  (interactive)
  (let* ((buf (get-buffer-create yf-repl-buffer-name)))
    (unless (comint-check-proc buf)
      (with-current-buffer buf
        (yf-repl-mode)
        (apply #'make-comint-in-buffer yf-repl-name buf nil nil)
        (let ((fake-proc (get-buffer-process buf)))
          (set-process-query-on-exit-flag fake-proc nil)
          (set-process-filter fake-proc 'comint-output-filter)
          (set-process-sentinel fake-proc (lambda (&rest _) nil))
          (set-process-buffer fake-proc buf)
          (yf-repl--input-sender fake-proc "\n"))))
    (pop-to-buffer buf)))
    
(defun yf-repl-save-history ()
  "Save the YF REPL command history."
  (interactive)
  (when (and (boundp 'comint-input-ring) comint-input-ring)
    (comint-write-input-ring)))

(defun yf-repl-restart ()
  (interactive)
  (yf-clear)
  (yf-forget)
  (let ((buf (get-buffer yf-repl-buffer-name)))
    (when buf (kill-buffer buf))
    (yf-repl-start)))

(add-hook 'kill-emacs-hook #'yf-repl-save-history)

(provide 'yf-repl)

;;Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; yf-repl.el ends here
