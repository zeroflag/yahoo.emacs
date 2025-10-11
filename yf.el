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

(defconst yf-default-currency "ANY")

(defconst yf-currency-codes
  `("AED" "AFN" "ALL" "AMD" "ANG" "AOA" "ARS" "AUD" "AWG" "AZN"
    "BAM" "BBD" "BDT" "BGN" "BHD" "BIF" "BMD" "BND" "BOB" "BRL"
    "BSD" "BTN" "BWP" "BYN" "BZD" "CAD" "CDF" "CHF" "CLP" "CNY"
    "COP" "CRC" "CUP" "CVE" "CZK" "DJF" "DKK" "DOP" "DZD" "EGP"
    "ERN" "ETB" "EUR" "FJD" "FKP" "FOK" "GBP" "GEL" "GGP" "GHS"
    "GIP" "GMD" "GNF" "GTQ" "GYD" "HKD" "HNL" "HRK" "HTG" "HUF"
    "IDR" "ILS" "IMP" "INR" "IQD" "IRR" "ISK" "JEP" "JMD" "JOD"
    "JPY" "KES" "KGS" "KHR" "KID" "KMF" "KRW" "KWD" "KYD" "KZT"
    "LAK" "LBP" "LKR" "LRD" "LSL" "LYD" "MAD" "MDL" "MGA" "MKD"
    "MMK" "MNT" "MOP" "MRU" "MUR" "MVR" "MWK" "MXN" "MYR" "MZN"
    "NAD" "NGN" "NIO" "NOK" "NPR" "NZD" "OMR" "PAB" "PEN" "PGK"
    "PHP" "PKR" "PLN" "PYG" "QAR" "RON" "RSD" "RUB" "RWF" "SAR"
    "SBD" "SCR" "SDG" "SEK" "SGD" "SHP" "SLE" "SOS" "SRD" "SSP"
    "STN" "SYP" "SZL" "THB" "TJS" "TMT" "TND" "TOP" "TRY" "TTD"
    "TVD" "TWD" "TZS" "UAH" "UGX" "USD" "UYU" "UZS" "VES" "VND"
    "VUV" "WST" "XAF" "XCD" "XDR" "XOF" "XPF" "YER" "ZAR" "ZMW"
    "ZWL" ,yf-default-currency))

(defconst yf-currency-set (make-hash-table :test 'equal))

(dolist (code yf-currency-codes)
  (puthash code t yf-currency-set))

(defun yf-is-default-currency? (s)
  (string= (upcase s) (upcase yf-default-currency)))

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
  (concat (number-to-string (car price)) " "
          (if (yf-is-default-currency? (cdr price))
              ""
            (cdr price))))

(defun yf-resolve-tickers (line)
  "Read the tickers (e.g.: $SPY) from the LINE and replace them with their price."
  (let ((regexp "\\$\\([[:word:].]+\\)"))
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

;; Expression evaluator

(defun yf-is-currency? (token)
  (gethash (upcase token) yf-currency-set))

(defun yf-check-currency (c1 c2)
  (when (not (or (yf-is-default-currency? c1)
                 (yf-is-default-currency? c2)
                 (string= (upcase c1) (upcase c2))))
    (user-error "Currency mismatch %s - %s" c1 c2)))

(defun yf-add (a b)
  (let ((n1 (car a))
        (n2 (car b))
        (c1 (cdr a))
        (c2 (cdr b)))
    (yf-check-currency c1 c2)
    (cons (+ n1 n2) c1)))

(defun yf-sub (a b)
  (let ((n1 (car a))
        (n2 (car b))
        (c1 (cdr a))
        (c2 (cdr b)))
    (yf-check-currency c1 c2)
    (cons (- n1 n2) c1)))

(defun yf-mul (a b)
  (let ((n1 (car a))
        (n2 (car b))
        (c1 (cdr a))
        (c2 (cdr b)))
    (yf-check-currency c1 c2)
    (cons (* n1 n2) c1)))

(defun yf-div (a b)
  (let ((n1 (car a))
        (n2 (car b))
        (c1 (cdr a))
        (c2 (cdr b)))
    (yf-check-currency c1 c2)
    (cons (/ (float n1) n2) c1)))

(defun yf-tonum (str)
  (if (string-match-p "\\`[+-]?[0-9]+\\(?:\\.[0-9]*\\)?\\'" str)
      (string-to-number str)
    (user-error "Not a number: %s" str)))

(defun yf-to (num-with-currency dst-currency)
  (when (not (yf-is-currency? dst-currency))
    (user-error "Not a valid currency %s" dst-currency))
  (let* ((amount (car num-with-currency))
         (src-currency (cdr num-with-currency)))
    (when (not (yf-is-currency? src-currency))
      (user-error "Not a valid currency %s" src-currency))
    (cond
     ((yf-is-default-currency? src-currency)
      (cons amount dst-currency))
     ((yf-is-default-currency? dst-currency)
      (cons amount src-currency))
     (t
      (cons (yf-convert amount src-currency dst-currency)
            dst-currency)))))

(defun yf-eval-postfix (line)
  "Evaluate LINE containing postfix expression."
  (let* ((stack '())
         (dict (make-hash-table :test #'equal))
         (tokens (split-string line)))
    (puthash "+" (lambda () (push (yf-add (pop stack) (pop stack)) stack)) dict)
    (puthash "*" (lambda () (push (yf-mul (pop stack) (pop stack)) stack)) dict)
    (puthash "." (lambda () (message "%s" (pop stack))) dict)
    (puthash "-" (lambda ()
                   (let ((b (pop stack))
                         (a (pop stack)))
                     (push (yf-sub a b) stack))) dict)
    (puthash "/" (lambda ()
                   (let ((b (pop stack))
                         (a (pop stack)))
                     (push (yf-div a b) stack))) dict)
    (puthash "to" (lambda ()
                    (let ((currency (car tokens)))
                      (push (yf-to (pop stack) currency) stack))
                    (setq tokens (cdr tokens))) dict)
    (while tokens
      (let ((tok (car tokens)))
        (setq tokens (cdr tokens))
        (cond
         ((gethash tok dict)
          (funcall (gethash tok dict)))
         ((yf-is-currency? tok)
          (push (cons (car (pop stack)) tok) stack))
         (t
          (push (cons (yf-tonum tok)
                      yf-default-currency) stack)))))
    stack))

(defun yf-resolve-in-line ()
  "Read and resolve both tickers and currency conversion expressions in current line."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (resolved (yf-resolve-tickers line))
         (result (yf-eval-postfix resolved))
         (result (mapconcat #'yf-price-to-string result " ")))
    (beginning-of-line)
    (kill-line)
    (insert result)))

(provide 'yf)

;;Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; yf.el ends here
