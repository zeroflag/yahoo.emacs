;;; -*- lexical-binding: t; -*-
;;; yahoo.el --- Financial utilities
;;;
;;; Author: Attila Magyar
;;; URL: http://github.com/zeroflag/yahoo.emacs
;;; Version: 0.1
;;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;;; Financial utilities for Emacs
;;;
;;; Code:
;;;
(require 'request)
(require 'json)

(defvar yahoo-api-url
  "https://query1.finance.yahoo.com/v8/finance/chart")

(defun yahoo-api-url (ticker)
  (concat yahoo-api-url "/" ticker))

(defun yahoo-extract (json)
  (let* ((chart    (assoc-default 'chart json))
         (result   (assoc-default 'result chart))
         (data     (aref result 0))
         (meta     (assoc-default 'meta data))
         (price    (assoc-default 'regularMarketPrice meta))
         (currency (assoc-default 'currency meta)))
    (if (string= "GBp" currency)
        (cons (/ price 100) currency)
      (cons price currency))))

(defun yahoo-fetch (ticker callback)
  (request
    (yahoo-api-url ticker)
    :type "GET"
    :headers '(("Accept" . "application/json")
               ("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:58.0) Gecko/20100101 Firefox/58.0"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback (yahoo-extract data))))
    :error (cl-function
            (lambda (&rest args &key error-thrown &allow-other-keys)
              (message "Yahoo API error: %S" error-thrown)))))

(defun yahoo-price-to-string (price)
  (concat (number-to-string (car price)) " " (cdr price) "\n"))

(defun yahoo-stock-price (ticker)
  "Fetch stock price denoted by TICKER and insert it into the buffer."
  (interactive "sTicker: ")
  (yahoo-fetch (string-trim ticker)
               (lambda (result)
                 (move-end-of-line nil)
                 (insert " ")
                 (insert (yahoo-price-to-string result)))))

(defun yahoo-read-ticker-and-get-price ()
  "Get the ticker from the current line and call `yahoo-stock-price`."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (yahoo-stock-price line)))

;(global-set-key (kbd "<f12>") 'yahoo-read-ticker-and-get-price)

;;Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; yahoo.el ends here
