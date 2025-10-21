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
(defvar yf-overlay-color "green")
(defvar yf-cache-ttl-sec (* 5 60))

(defconst yf-default-currency "ANY")
(defconst yf-ticker-regexp "\\$\\([[:word:].=]+\\)")

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

(defun yf-new-ht () (make-hash-table :test #'equal))

(defconst yf-currency-set (yf-new-ht))

(dolist (code yf-currency-codes)
  (puthash code t yf-currency-set))

(defvar-local yf-stack nil
  "Main stack used by the interpreter.")

(defvar-local yf-dict (yf-new-ht)
  "Dictionary used by the interpreter.")

(defvar-local yf-word-list nil
  "List of built-in and user defined words")

(defvar-local yf-mode 'interpret
  "Mode of the interpreter")

(defvar-local yf-quotation-cnt 0)
(defvar-local yf-tok-start 0)
(defvar-local yf-tok-end 0)

(defmacro yf-debug-message (fmt &rest args)
  `(when yf-debug
     (let* ((now (format-time-string "%Y-%m-%d %H:%M:%S"))
            (prefix (format "[yf] [%s] " now)))
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

(defun yf-money? (pair)
  (and (numberp (car pair))
       (stringp (cdr pair))))

(defun yf-to-string (item)
  (cond
   ((null item)
    "NIL")
   ((eq item t)
    "TRUE")
   ((yf-money? item)
    (let* ((amount (car item))
           (currency (cdr item))
           (prefix (yf-add-number-grouping amount))
           (suffix (if (yf-is-default-currency? currency)
                       ""
                     (concat " " currency))))
      (concat prefix suffix)))
   ((listp item) ; quotation
    (concat "[ "
            (substring (format "%s" (reverse item)) 1 -1)
            " ]"))
   (t
    (format "%s" item))))

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

(defun yf-expect-money (xs)
  (dolist (x xs)
    (unless (yf-money? x)
      (user-error "TypeError: expected money, got %s"
                  (yf-to-string x)))))

(defun yf-add (a b)
  (yf-expect-money (list a b))
  (let ((n1 (car a))
        (n2 (car b)))
    (yf-check-currency a b)
    (cons (+ n1 n2)
          (yf-pick-currency a b))))

(defun yf-sub (a b)
  (yf-expect-money (list a b))
  (let ((n1 (car a))
        (n2 (car b)))
    (yf-check-currency a b)
    (cons (- n1 n2)
          (yf-pick-currency a b))))

(defun yf-mul (a b)
  (yf-expect-money (list a b))
  (let ((n1 (car a))
        (n2 (car b)))
    (yf-check-currency a b)
    (cons (* n1 n2)
          (yf-pick-currency a b))))

(defun yf-div (a b)
  (yf-expect-money (list a b))
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

(defun yf-lt (a b)
  (yf-expect-money (list a b))
  (let ((n1 (car a))
        (n2 (car b)))
    (yf-check-currency a b)
    (< n2 n1)))

(defun yf-lte (a b)
  (yf-expect-money (list a b))
  (let ((n1 (car a))
        (n2 (car b)))
    (yf-check-currency a b)
    (<= n2 n1)))

(defun yf-gt (a b)
  (yf-expect-money (list a b))
  (let ((n1 (car a))
        (n2 (car b)))
    (yf-check-currency a b)
    (> n2 n1)))

(defun yf-gte (a b)
  (yf-expect-money (list a b))
  (let ((n1 (car a))
        (n2 (car b)))
    (yf-check-currency a b)
    (>= n2 n1)))

(defun yf-callq (quotation)
  (setq yf-stack (yf--eval (list (reverse quotation))))) ; box

(defun yf-print-overlay (text)
  (let ((overlay (make-overlay (1+ yf-tok-start)
                               (1+ yf-tok-end))))
    (push overlay yf-overlays)
    (overlay-put overlay
                 'after-string
                 (concat
                  (propertize " => " 'face `(:foreground "brown"))
                  (propertize text 'face `(:foreground ,yf-overlay-color))))))

(defun yf-delete-overlays ()
  "Delete all overlays created by YF."
  (interactive)
  (mapc #'delete-overlay yf-overlays)
  (setq yf-overlays nil))

(defun yf-refresh-word-list ()
  (let (words)
    (maphash (lambda (k _v) (push k words)) yf-dict)
    (setq yf-word-list words)))

(defun yf-join (xs &optional sep)
  (mapconcat #'identity (reverse xs) (or sep " ")))

(defun yf-words ()
  (yf-join yf-word-list))

(defun yf-num? (str)
  (string-match-p "\\`[+-]?[0-9]+\\(?:\\.[0-9]*\\)?\\'" str))

(defun yf-to (money dst-currency)
  (unless (yf-is-currency? dst-currency)
    (user-error "Not a valid currency %s" dst-currency))
  (let* ((amount (car money))
         (dst-currency (upcase dst-currency))
         (src-currency (cdr money)))
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
  (string-match yf-ticker-regexp token))

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

(defun yf-tok (tokens-box)
  (let ((tokens (car tokens-box)))
    (upcase (caar tokens))))

(defun yf-tok-start (tokens-box)
  (let ((tokens (car tokens-box)))
    (cadr (car tokens))))

(defun yf-tok-end (tokens-box)
  (let ((tokens (car tokens-box)))
    (caddr (car tokens))))

(defun yf-pop ()
  (unless yf-stack
    (user-error "[yf] Stack underflow."))
  (pop yf-stack))

(defun yf-push (item) (push item yf-stack))
(defun yf-tos () (car yf-stack))
(defun yf-tos2 () (cadr yf-stack))
(defun yf-clear () (setq yf-stack '()))

(defun yf-forget ()
  (setq yf-word-list '())
  (setq yf-dict (yf-new-ht)))

(defun yf-def-- (name lambda)
  ;; TODO fixme
  "Define a new word with NAME and LAMBDA."
  (let ((name (upcase name)))
    (unless (gethash name yf-dict)
      (puthash name lambda yf-dict))))

(defun yf-def (name lambda)
  (puthash (upcase name) lambda yf-dict))

(defun yf-add-to-quotation (tok start end)
  (yf-push (cons (list tok start end)
                 (yf-pop))))

(defun yf-eval-quotation (tok start end)
  (cond
   ((string= "]" tok)
    (when (< yf-quotation-cnt 1)
      (user-error "Unexpected end of quotation '%s' at: %d-%d"
                  tok start end))
    (setq yf-quotation-cnt (1- yf-quotation-cnt))
    (if (= 0 yf-quotation-cnt)
        (setq yf-mode 'interpret)
      (yf-add-to-quotation tok start end)))
   ((string= "[" tok)
                                        ; nested quotation
    (setq yf-quotation-cnt (1+ yf-quotation-cnt))
    (yf-add-to-quotation tok start end))
   (t ; normal token
    (yf-add-to-quotation tok start end))))

(defun yf-eval (text &optional offset)
  (interactive)
  (let ((tokens-box (list (yf-parse text))))
    (yf-def "+" (lambda (tokens-box) (yf-push (yf-add (yf-pop) (yf-pop)))))
    (yf-def "*" (lambda (tokens-box) (yf-push (yf-mul (yf-pop) (yf-pop)))))
    (yf-def "." (lambda (tokens-box) (yf-print-overlay (yf-to-string (yf-pop)))))
    (yf-def ".S" (lambda (tokens-box) (yf-print-overlay (yf-show-stack))))
    (yf-def "MESSAGE" (lambda (tokens-box) (message (yf-to-string (yf-pop)))))
    (yf-def "?" (lambda (tokens-box) (yf-print-overlay (yf-to-string (yf-tos)))))
    (yf-def "-"
            (lambda (tokens-box)
              (let ((b (yf-pop))
                    (a (yf-pop)))
                (yf-push (yf-sub a b)))))
    (yf-def "/"
            (lambda (tokens-box)
              (let ((b (yf-pop))
                    (a (yf-pop)))
                (yf-push (yf-div a b)))))
    (yf-def "<" (lambda (tokens-box) (yf-push (yf-lt (yf-pop) (yf-pop)))))
    (yf-def "<=" (lambda (tokens-box) (yf-push (yf-lte (yf-pop) (yf-pop)))))
    (yf-def ">" (lambda (tokens-box) (yf-push (yf-gt (yf-pop) (yf-pop)))))
    (yf-def ">=" (lambda (tokens-box) (yf-push (yf-gte (yf-pop) (yf-pop)))))
    (yf-def "SUM" (lambda (tokens-box) (setq yf-stack (yf-sum-currency-groups yf-stack))))
    (yf-def "SUMPROD"
            (lambda (tokens-box)
              (setq yf-stack (yf-prod-pairs yf-stack))
              (setq yf-stack (yf-sum-currency-groups yf-stack))))
    (yf-def "SWAP"
            (lambda (tokens-box)
              (let ((a (yf-pop))
                    (b (yf-pop)))
                (yf-push a)
                (yf-push b))))
    (yf-def "ROT"
            (lambda (tokens-box)
              (let ((a (yf-pop))
                    (b (yf-pop))
                    (c (yf-pop)))
                (yf-push b)
                (yf-push a)
                (yf-push c))))
    (yf-def "-ROT"
            (lambda (tokens-box)
              (let ((a (yf-pop))
                    (b (yf-pop))
                    (c (yf-pop)))
                (yf-push a)
                (yf-push c)
                (yf-push b))))
    (yf-def "TUCK"
            (lambda (tokens-box)
              (let ((a (yf-pop))
                    (b (yf-pop)))
                (yf-push a)
                (yf-push b)
                (yf-push a))))
    (yf-def "SHIFT"
            (lambda (tokens-box)
              (let ((tos (yf-pop)))
                (setq yf-stack (append yf-stack (list tos))))))
    (yf-def "DUP" (lambda (tokens-box) (yf-push (yf-tos))))
    (yf-def "OVER" (lambda (tokens-box) (yf-push (yf-tos2))))
    (yf-def "DROP" (lambda (tokens-box) (yf-pop)))
    (yf-def "CLEAR" (lambda (tokens-box) (yf-clear)))
    (yf-def "DEPTH"
            (lambda (tokens-box) (yf-push (cons (length yf-stack)
                                      yf-default-currency))))
    (yf-def "TO"
            (lambda (tokens-box)
              (let ((currency (yf-tok tokens-box)))
                (yf-push (yf-to (yf-pop) currency)))
              (yf-next tokens-box)))
    (yf-def "CONST"
            (lambda (tokens-box)
              (let ((name (yf-tok tokens-box))
                    (val (yf-pop)))
                (yf-debug-message "Define constant %s with value %s" name val)
                (yf-def name (lambda () (yf-push val)))
                (yf-refresh-word-list))
              (yf-next tokens-box)))
    (yf-def "["
            (lambda (tokens-box)
              (setq yf-mode 'quotation)
              (setq yf-quotation-cnt 1)
              (yf-push nil))) ; list to collect quotation items
    (yf-def "WHEN"
            (lambda (tokens-box)
              (let ((body (yf-pop))
                    (cond (yf-pop)))
                (yf-callq cond)
                (when (yf-pop)
                  (yf-callq body)))))
    (yf-def "UNLESS"
            (lambda (tokens-box)
              (let ((body (yf-pop))
                    (cond (yf-pop)))
                (yf-callq cond)
                (unless (yf-pop)
                  (yf-callq body)))))
    (yf-def "IF"
            (lambda (tokens-box)
              (let ((body1 (yf-pop))
                    (body2 (yf-pop))
                    (cond (yf-pop)))
                (yf-callq cond)
                (if (yf-pop)
                    (yf-callq body1)
                  (yf-callq body2)))))
    (yf-def "WHILE"
            (lambda (tokens-box)
              (let ((body (yf-pop))
                    (cond (yf-pop)))
                (yf-callq cond)
                (while (yf-pop)
                  (yf-callq body)
                  (yf-callq cond)))))
    (yf-def "CALL" (lambda (tokens-box) (yf-callq (yf-pop))))
    (yf-def "TIMES"
            (lambda (tokens-box)
              (let ((count (car (yf-pop)))
                    (code (yf-pop)))
                (dotimes (_ count)
                  (yf-callq code)))))
    (yf-def "WORDS" (lambda (tokens-box) (yf-print-overlay (yf-words))))
    (yf-def "("
            (lambda (tokens-box)
              (while (and (car tokens-box)
                          (not (string= ")" (yf-tok tokens-box))))
                (yf-next tokens-box))
              (yf-next tokens-box)))
    (setq yf-tok-start 0)
    (setq yf-tok-end 0)
    (yf--eval tokens-box offset)))

(defun yf-next (tokens-box)
  (let ((tokens (car tokens-box)))
    (setcar tokens-box (cdr tokens))))

(defun yf--eval (tokens-box &optional offset)
  "Evaluate TEXT containing postfix expression."
  (let* ((tok nil)
         (index 0)
         (size (length (car tokens-box)))
         (progress (make-progress-reporter "[yf] busy.. " 0 size))
         (tok-offset (or offset 0)))
    (unless yf-word-list
      (yf-refresh-word-list))
    (while (car tokens-box)
      (setq tok (yf-tok tokens-box))
      (setq yf-tok-start (+ tok-offset (yf-tok-start tokens-box)))
      (setq yf-tok-end (+ tok-offset (yf-tok-end tokens-box)))
      (yf-next tokens-box)
      (yf-debug-message "Eval token: '%s' at: %d-%d" tok yf-tok-start yf-tok-end)
      (cond
       ((eq 'quotation yf-mode)
        (yf-eval-quotation tok yf-tok-start yf-tok-end))
       ((gethash tok yf-dict)
        (funcall (gethash tok yf-dict) tokens-box))
       ((yf-is-currency? tok)
        (yf-push (cons (car (yf-pop))
                       (upcase tok))))
       ((yf-num? tok)
        (yf-push (cons (string-to-number tok)
                       yf-default-currency)))
       ((yf-ticker? tok)
        (yf-push (yf-resolve-ticker tok)))
       (t
        (user-error
         "Unkown word: %s at: %d-%d" tok yf-tok-start yf-tok-end)))
      (progress-reporter-update progress index)
      (setq index (1+ index))
      (sit-for 0))
    (progress-reporter-done progress))
  yf-stack)

(defun yf-show-stack ()
  (mapconcat #'yf-to-string (reverse yf-stack) " "))

(defun yf-clean-state ()
  (yf-delete-overlays)
  (yf-clear)
  (yf-forget))

(defun yf-eval-current-line ()
  "Read and eval current line as YF code."
  (interactive)
  (yf-clean-state)
  (let* ((line (thing-at-point 'line t))
         (offset (- (line-beginning-position) 1)))
    (yf-eval line offset)
    (message (yf-show-stack))))

(defun yf-eval-region (start end)
  "Read and eval the region between START and END as YF code."
  (interactive "r")
  (yf-clean-state)
  (let* ((text (buffer-substring-no-properties start end))
         (offset (- start 1)))
    (yf-eval text offset)
    (message (yf-show-stack))))

(defun yf-eval-buffer ()
  "Read and eval current buffer as YF code."
  (interactive)
  (yf-clean-state)
  (yf-eval (buffer-string))
  (message (yf-show-stack)))

(provide 'yf)

;;Local Variables:
;; byte-compile-warnings: (not docstrings)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; yf.el ends here
