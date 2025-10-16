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
  (concat yf-api-url "/" (url-hexify-string ticker)))

(defvar yf-debug nil)
(defvar yf-overlays '())
(defvar yf-overlay-color "orange")
(defvar yf-cache-ttl-sec 60)

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

(defmacro yf-debug-message (fmt &rest args)
  `(when yf-debug
     (let* ((now (format-time-string "%Y-%m-%d %H:%M:%S"))
            (prefix (format "(yf) [%s] " now)))
       (message (concat prefix ,fmt) ,@args))))

(defun yf-is-default-currency? (s)
  (string= (upcase s) (upcase yf-default-currency)))

(defun yf-extract (json ticker)
  (let* ((chart    (assoc-default 'chart json))
         (result   (assoc-default 'result chart))
         (data     (aref result 0))
         (meta     (assoc-default 'meta data))
         (price    (assoc-default 'regularMarketPrice meta))
         (currency (assoc-default 'currency meta)))
    (when (or (not price) (not currency))
      (error "Missing price and/or currency of '%s'" ticker))
    (if (string= "GBp" currency)
        (cons (/ price 100) currency)
      (cons price currency))))

(defun yf-http-success? (code)
  (and (<= code 299) (>= code 200)))

(defun yf--get (ticker)
  "Fetch stock price and currency of the given TICKER"
  (interactive "sTicker: ")
  (yf-debug-message "Fetching price of %s" ticker)
  (let* ((response (request (yf-api-url ticker)
                     :type "GET"
                     :sync t
                     :headers '(("Accept" . "application/json")
                                ("User-Agent" . yf-user-agent))
                     :parser 'json-read))
         (code (request-response-status-code response)))
    (yf-debug-message "Status code: %d" code)
    (if (yf-http-success? code)
        (yf-extract (request-response-data response) ticker)
      (error "Could not get price of %s. Status code: %d" ticker code))))

(defun yf-memoize (f)
  "Memoize the function F, which can take any number of arguments."
  (let* ((cache (make-hash-table :test 'equal))
         (memoized
          (lambda (&rest args)
            (let* ((entry (gethash args cache))
                   (value (car entry))
                   (timestamp (cdr entry))
                   (now (float-time)))
              (when (or (not value) (> (- now timestamp) yf-cache-ttl-sec))
                (setq value (apply f args))
                (yf-debug-message "Saving '%s' => '%s' to cache. TTL=%d"
                                  args
                                  value
                                  yf-cache-ttl-sec)
                (puthash args (cons value now) cache))
              value))))
    memoized))

(defun yf-get (ticker) "stub" ticker)
(fset 'yf-get (yf-memoize #'yf--get))

(defun yf-add-number-grouping (number &optional separator)
  (let ((num (format "%.2f" number))
        (op (or separator ",")))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
      (setq num (concat
                 (match-string 1 num) op
                 (match-string 2 num))))
    num))

(defun yf-to-string (price)
  (concat (yf-add-number-grouping (car price)) " "
          (if (yf-is-default-currency? (cdr price))
              ""
            (cdr price))))

;; Exchange rates

(defun yf-xchg-rate (src-currency dst-currency)
  (let* ((ticker (concat src-currency dst-currency "=X"))
         (rate (yf-get ticker)))
    (car rate)))

(defun yf-convert (amount src-currency dst-currency)
  "Convert AMOUNT from SRC-CURRENCY to DST-CURRENCY."
  (interactive)
  (if (string= (upcase src-currency)
               (upcase dst-currency))
      amount
    (let ((rate (yf-xchg-rate src-currency dst-currency)))
      (* rate amount))))

;; Expression evaluator

(defun yf-is-currency? (token)
  (gethash (upcase token) yf-currency-set))

(defun yf-currency-match (a b)
  (let ((c1 (cdr a))
        (c2 (cdr b)))
    (or (yf-is-default-currency? c1)
        (yf-is-default-currency? c2)
        (string= (upcase c1) (upcase c2)))))

(defun yf-check-currency (a b)
  (unless (yf-currency-match a b)
    (user-error "Currency mismatch %s - %s" a b)))

(defun yf-sum-currency-groups (xs)
  (if (> (length xs) 1)
      (let ((first (car xs))
            (second (cadr xs)))
        (if (yf-currency-match first second)
            (yf-sum-currency-groups (cons
                                     (yf-add first second)
                                     (cddr xs)))
          (cons first (yf-sum-currency-groups (cdr xs)))))
    xs))

(defun yf-pick-currency (a b)
  (let ((c1 (cdr a))
        (c2 (cdr b)))
    (if (yf-is-default-currency? c1) c2 c1)))

(defun yf-add (a b)
  (let ((n1 (car a))
        (n2 (car b)))
    (yf-check-currency a b)
    (cons (+ n1 n2)
          (yf-pick-currency a b))))

(defun yf-sub (a b)
  (let ((n1 (car a))
        (n2 (car b)))
    (yf-check-currency a b)
    (cons (- n1 n2)
          (yf-pick-currency a b))))

(defun yf-mul (a b)
  (let ((n1 (car a))
        (n2 (car b)))
    (yf-check-currency a b)
    (cons (* n1 n2)
          (yf-pick-currency a b))))

(defun yf-div (a b)
  (let ((n1 (car a))
        (n2 (car b)))
    (yf-check-currency a b)
    (cons (/ (float n1) n2)
          (yf-pick-currency a b))))

(defun yf-prod-pairs (xs)
  (if (< (length xs) 2)
      xs 
    (let ((first (car xs))
          (second (cadr xs)))
      (cons (yf-mul first second)
            (yf-prod-pairs (cddr xs))))))

(defun yf-print-overlay (text tok-start tok-end)
  (let ((overlay (make-overlay (1+ tok-start)
                               (1+ tok-end))))
    (push overlay yf-overlays)
    (overlay-put overlay
                 'after-string
                 (propertize
                  (concat " => " text)
                  'face `(:foreground ,yf-overlay-color)))))

(defun yf-delete-overlays ()
  "Delete all overlays created by YF."
  (interactive)
  (mapc #'delete-overlay yf-overlays)
  (setq yf-overlays nil))

(defun yf-num? (str)
  (string-match-p "\\`[+-]?[0-9]+\\(?:\\.[0-9]*\\)?\\'" str))

(defun yf-to (num-with-currency dst-currency)
  (unless (yf-is-currency? dst-currency)
    (user-error "Not a valid currency %s" dst-currency))
  (let* ((amount (car num-with-currency))
         (dst-currency (upcase dst-currency))
         (src-currency (cdr num-with-currency)))
    (unless (yf-is-currency? src-currency)
      (user-error "Not a valid currency %s" src-currency))
    (cond
     ((yf-is-default-currency? src-currency)
      (cons amount dst-currency))
     ((yf-is-default-currency? dst-currency)
      (cons amount src-currency))
     (t
      (cons (yf-convert amount src-currency dst-currency)
            dst-currency)))))

(defun yf-ticker? (token)
  (string-match "\\$\\([[:word:].=]+\\)" token))

(defun yf-resolve-ticker (token)
  (let ((ticker (match-string 1 token)))
    (yf-get ticker)))

(defun yf-parse (text)
  "Parse TEXT and return tokens in the following format: ( ( token start end ) .. )."
  (let ((pos 0)
        (tokens '()))
    (while (string-match "[^[:space:]\r\n]+" text pos)
      (let ((start (match-beginning 0))
            (end   (match-end 0))
            (token (match-string 0 text)))
        (push (list token start end) tokens)
        (setq pos end)))
    (nreverse tokens)))

(defun yf-tok (tokens) (caar tokens))
(defun yf-tok-start (tokens) (cadr (car tokens)))
(defun yf-tok-end (tokens) (caddr (car tokens)))

(defun yf-eval (text &optional stack offset)
  "Evaluate TEXT containing postfix expression."
  (interactive)
  (yf-delete-overlays)
  (let* ((dict (make-hash-table :test #'equal))
         (tokens (yf-parse text))
         (index 0)
         (size (length tokens))
         (progress (make-progress-reporter "[yf] Running.. " 0 size))
         (tok-start 0)
         (tok-end 0)
         (tok-offset (or offset 0)))
    (puthash "+" (lambda () (push (yf-add (pop stack) (pop stack)) stack)) dict)
    (puthash "*" (lambda () (push (yf-mul (pop stack) (pop stack)) stack)) dict)
    (puthash "." (lambda () (yf-print-overlay (yf-to-string (pop stack)) tok-start tok-end)) dict)
    (puthash ".s" (lambda () (yf-print-overlay (yf-show-stack stack) tok-start tok-end)) dict)
    (puthash "message" (lambda () (message (yf-to-string (pop stack)))) dict)
    (puthash "?" (lambda () (yf-print-overlay (yf-to-string (car stack)) tok-start tok-end)) dict)
    (puthash "-"
             (lambda ()
               (let ((b (pop stack))
                     (a (pop stack)))
                 (push (yf-sub a b) stack)))
             dict)
    (puthash "/"
             (lambda ()
               (let ((b (pop stack))
                     (a (pop stack)))
                 (push (yf-div a b) stack)))
             dict)
    (puthash "sum" (lambda () (setq stack (yf-sum-currency-groups stack))) dict)
    (puthash "sumprod"
             (lambda ()
               (setq stack (yf-prod-pairs stack))
               (setq stack (yf-sum-currency-groups stack)))
             dict)
    (puthash "swap"
             (lambda ()
               (let ((a (pop stack))
                     (b (pop stack)))
                 (push a stack)
                 (push b stack)))
             dict)
    (puthash "dup" (lambda () (push (car stack) stack)) dict)
    (puthash "over" (lambda () (push (cadr stack) stack)) dict)
    (puthash "drop" (lambda () (pop stack)) dict)
    (puthash "clear" (lambda () (setq stack '())) dict)
    (puthash "depth"
             (lambda () (push (cons (length stack)
                                    yf-default-currency)
                              stack))
             dict)
    (puthash "to"
             (lambda ()
               (let ((currency (yf-tok tokens)))
                 (push (yf-to (pop stack) currency) stack))
               (setq tokens (cdr tokens))) ;; consume next
             dict) 
    (puthash "("
             (lambda ()
               (while (and tokens
                           (not (string= ")" (yf-tok tokens))))
                 (setq tokens (cdr tokens)))
               (setq tokens (cdr tokens)))
             dict)
    (while tokens
      (let* ((tok (yf-tok tokens)))
        (setq tok-start (+ tok-offset (yf-tok-start tokens)))
        (setq tok-end (+ tok-offset (yf-tok-end tokens)))
        (setq tokens (cdr tokens))
        (yf-debug-message "Eval token: '%s' at: %d-%d" tok tok-start tok-end)
        (cond
         ((gethash tok dict)
          (funcall (gethash tok dict)))
         ((yf-is-currency? tok)
          (push (cons (car (pop stack))
                      (upcase tok)) stack))
         ((yf-num? tok)
          (push (cons (string-to-number tok)
                      yf-default-currency) stack))
         ((yf-ticker? tok)
          (push (yf-resolve-ticker tok) stack))
         (t
          (user-error
           "Unkown word: %s at: %d-%d" tok tok-start tok-end)))
        (progress-reporter-update progress index)
        (setq index (1+ index))
        (sit-for 0)))
    (progress-reporter-done progress)
    stack))

(defun yf-show-stack (stack)
  (mapconcat #'yf-to-string (nreverse stack) " "))

(defun yf-eval-current-line ()
  "Read and eval current line by resolving tickers and currency conversions."
  (interactive)
  (let* ((line (thing-at-point 'line t))
         (offset (- (line-beginning-position) 1))
         (stack (yf-eval line nil offset)))
    (message (yf-show-stack stack))))

(defun yf-eval-buffer ()
  "Read and eval current buffer by resolving tickers and currency conversions."
  (interactive)
  (let* ((text (buffer-string))
         (stack (yf-eval text)))
    (message (yf-show-stack stack))))

(defvar yf-repl-stack '())
(defconst yf-repl-buffer-name "*Yahoo Finance REPL*")
(defconst yf-repl-prompt "(yf) $")

(defun yf-insert-prompt ()
  (insert (propertize
           yf-repl-prompt
           'face '(:foreground "magenta" :weight bold)))
  (insert " "))

(defun yf-read-input ()
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun yf-on-line-entered ()
  (interactive)
  (let* ((input (yf-read-input))
         (input (if (string-prefix-p yf-repl-prompt input)
                    (substring input (length yf-repl-prompt))
                  input))
         (input (string-trim input)))
    (setq yf-repl-stack (yf-eval input yf-repl-stack)))
  (goto-char (point-max))
  (insert "\n" (yf-show-stack yf-repl-stack) "\n")
  (yf-insert-prompt))

(defvar yf-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'yf-on-line-entered)
    map)
  "Keymap for `yf-repl-mode'.")

(define-derived-mode yf-repl-mode fundamental-mode "YAHOO-FINANCE-REPL"
  (setq-local inhibit-read-only t)
  (setq-local truncate-lines t)
  (use-local-map yf-repl-mode-map))

(defun yf-start-repl ()
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

(provide 'yf)

;;Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; yf.el ends here
