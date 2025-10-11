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
  "Fetch stock price and currency of the given TICKER"
  (interactive "sTicker: ")
  (let ((response (request (yf-api-url ticker)
                    :type "GET"
                    :sync t
                    :headers '(("Accept" . "application/json")
                               ("User-Agent" . yf-user-agent))
                    :parser 'json-read)))
    (yf-extract (request-response-data response))))

;; Ticker prices

(defun yf-get-price (ticker)
  "Fetch stock price of the given TICKER"
  (interactive "sTicker: ")
  (car (yf-get ticker)))

(defun yf-price-to-string (price)
  (concat (number-to-string (car price)) " " (cdr price)))

(defun yf-insert-stock-price (ticker)
  "Fetch stock price denoted by TICKER and insert it into the buffer."
  (interactive "sTicker: ")
  (let ((result (yf-get (string-trim ticker))))
    (move-end-of-line nil)
    (insert " ")
    (insert (yf-price-to-string result))))

(defun yf-resolve-tickers (line)
  "Read the tickers (e.g.: $SPY) from the LINE and replace them with their price."
  (let ((regexp "\\$\\([[:word:]]+\\)"))
    (while (string-match regexp line)
      (let* ((ticker (match-string 1 line))
             (price (yf-price-to-string (yf-get ticker))))
        (setq line (string-replace (concat "$" ticker) price line))))
    line))

;; Exchange rates

(defun yf-xchg-rate (src-currency dst-currency)
  (let* ((ticker (concat src-currency dst-currency "=X"))
         (rate (yf-get ticker)))
    (car rate)))

(defun yf-convert (amount src-currency dst-currency)
  "Convert AMOUNT from SRC-CURRENCY to DST-CURRENCY."
  (interactive)
  (let ((rate (yf-xchg-rate src-currency dst-currency)))
    (* rate amount)))

;; Line parsing

(defun yf-resolve-xchg-rates (line)
  "Read and resolve currency expression (e.g.: 10 usd to huf) from the current line."
  (let ((regexp "\\([[:digit:].]+\\) \\([[:word:]]+\\) TO \\([[:word:]]+\\)"))
    (while (string-match regexp line)
      (let* ((expression (match-string 0 line))
             (amount (string-to-number (match-string 1 line)))
             (src-currency (match-string 2 line))
             (dst-currency (match-string 3 line))
             (result (yf-convert amount src-currency dst-currency)))
        (setq line (string-replace expression (number-to-string result) line))))
    line))

(defun yf-resolve (line)
  "Read and resolve both tickers and currency conversion expression in LINE.

  E.g.:
    $BLK $O
    100 usd to eur
    $SPY to eur"
  (interactive "sExpression: ")
  (let* ((line (yf-resolve-tickers line))
         (line (yf-resolve-xchg-rates line)))
    line))

(defun yf-resolve-in-line ()
  "Read and resolve both tickers and currency conversion expressions in current line."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (line (yf-resolve line)))
    (beginning-of-line)
    (kill-line)
    (insert line)))

(provide 'yf)

;;Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; yf.el ends here
