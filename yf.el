;;; -*- lexical-binding: t; -*-
;;; yf.el --- Financial utilities
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

(defconst yf-user-agent
  "Mozilla/5.0 (X11; Linux x86_64; rv:58.0) Gecko/20100101 Firefox/58.0")

(defvar yf-api-url
  "https://query1.finance.yahoo.com/v8/finance/chart")

(defun yf-api-url (ticker)
  (concat yf-api-url "/" ticker))

(defun yf-extract (json)
  (let* ((chart    (assoc-default 'chart json))
         (result   (assoc-default 'result chart))
         (data     (aref result 0))
         (meta     (assoc-default 'meta data))
         (price    (assoc-default 'regularMarketPrice meta))
         (currency (assoc-default 'currency meta)))
    (if (string= "GBp" currency)
        (cons (/ price 100) currency)
      (cons price currency))))

(defun yf-get (ticker)
  (interactive "sTicker: ")
  (let ((response (request (yf-api-url ticker)
                    :type "GET"
                    :sync t
                    :headers '(("Accept" . "application/json")
                               ("User-Agent" . yf-user-agent))
                    :parser 'json-read)))
    (yf-extract (request-response-data response))))

(defun yf-price-to-string (price)
  (concat (number-to-string (car price)) " " (cdr price) "\n"))

(defun yf-insert-stock-price (ticker)
  "Fetch stock price denoted by TICKER and insert it into the buffer."
  (interactive "sTicker: ")
  (let ((result (yf-get (string-trim ticker))))
    (move-end-of-line nil)
    (insert " ")
    (insert (yf-price-to-string result))))

(defun yf-read-ticker-and-insert-price ()
  "Get the ticker from the current line and call `yf-stock-price`."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (yf-insert-stock-price line)))

(provide 'yf)

;;Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; yf.el ends here
