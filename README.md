# yf.el

![{master}](https://github.com/zeroflag/yahoo.emacs/actions/workflows/ci.yml/badge.svg)

## 📝 Overview

`yf.el` is a DSL and a runtime for Emacs for multi-currency portfolio tracking. 

It's written in Emacs Lisp, with live price fetching from **Yahoo Finance**, memoized caching, and debug overlays.


<img src="imgs/demo.gif" alt="fib" />

## ⚙️ Installation (Doom Emacs)

### packages.el

```lisp
(package! yf
  :recipe (:host github :repo "zeroflag/yahoo.emacs"))
```

### config.el

```lisp
(use-package! yf-mode
  :mode "\\.yf"
  :defer t
  :commands (yf-eval-current-line
             yf-eval-buffer
             yf-delete-overlays)
  :init
  (map! :leader
        :desc "Eval line as yf code."
        "y e" #'yf-eval-current-line)
  (map! :leader
        :desc "Eval buffer as yf code."
        "y b" #'yf-eval-buffer)
  (map! :leader
        :desc "Eval region as yf code."
        "y r" #'yf-eval-region)
  (map! :leader
        :desc "Delete all overlays created by yf."
        "y d" #'yf-delete-overlays))

(use-package! yf-repl
  :defer t
  :commands yf-repl-start
  :init
  (map! :leader
        :desc "Start Yahoo Finace REPL."
        "y s" #'yf-repl-start))
```

## 💡 Example

```Forth
( Simple portfolio tracker and F.I.R.E calculator )
2500 EUR     CONST MONTHLY
MONTHLY 12 * CONST EXPENSE ( annual expense )

( Holdings USD )
$AMZN        50
$SBUX        300
$SCHD        2500
$DIVO        500
$QQQ         50
( Holdings EUR )
$SXR8.DE     750
$VWCE.DE     1600
$P911.DE     500
( Holdings GBP )
$ULVR.L      250

SUMPROD .S

[ TO EUR SHIFT ] DEPTH 1 - TIMES ( convert all to EUR )

SUM ?

EXPENSE / ANY ?

[ 25 > ] [ "You can retire now." ] [ "Keep working.." ] IF
.

```

→ Evaluates your current holdings, converts them into your `BASE` currency, and divides by your annual expenses.


## 🔤 Words and Stack Effects

Below is the current word reference, grouped by category.

### 🔧 Core Stack Manipulation

| Word   | Stack Effect    | Description                           |
| ------ | --------------- | ------------------------------------- |
| `DROP` | `x →`           | Discards the top of the stack         |
| `DUP`  | `x → x x`       | Duplicates top element                |
| `OVER` | `a b → a b a`   | Copies second element to top          |
| `SWAP` | `a b → b a`     | Swaps top two elements                |
| `ROT`  | `a b c → b c a` | Rotates top three elements            |
| `-ROT` | `a b c → c a b` | Inverse rotate                        |
| `.S`   | `… → …`         | Prints stack contents (for debugging) |

---

### 🧠 Logic, Control & Defining 

| Word      | Stack Effect                            | Description                                   |
| --------- | --------------------------------------- | --------------------------------------------- |
| `CONST`   | `value name →`                          | Defines a constant word                                             |
| `SUMPROD` | `— → total`                             | Multiplies each logged holding by its price, sums across currencies |
| `SUM`     | `list → number`                         | Sums numeric list                                                   |
| `WHEN`    | `[cond] [then] →`                       | Executes `then` quotation if `cond` is true   |
| `FORGE`   | `[quote] name →`                        | Defines a new word (quotation literal)        |
| `DEPTH`   | `— → n`                                 | Pushes current stack depth                    |
| `TIMES`   | `[quote] n →`                           | Executes quotation `n` times                  |

---

### 🖨️ Output & Debugging

| Word     | Stack Effect   | Description                                          |
| ---------| -------------- | ---------------------------------------------------- |
| `PRINC`  | `string →`     | Prints a string without newline to stdout            |
| `MESSAGE`| `string →`     | Prints a string without newline to message buffer    |
| `.`      | `string →`     | Prints the top of the stack as an overlay            |
| `.S`     | `— →`          | Displays current stack contents as an overlay        |

---

### 🌐 Yahoo Finance Integration

| Word                 | Stack Effect                                | Description                                        |
| -------------------- | ------------------------------------------- | -------------------------------------------------- |
| `$TICKER`            | `— → (price . "CUR")`                       | Fetches live Yahoo Finance price for ticker symbol |
| `TO`                 | `(price . "CUR") → (price . "CUR")`         | Converst from currency1 to currency2               |

Examples:

```Forth
$AAPL .            => 180.25 USD
10 USD TO EUR .    => 8.61 EUR
```

## 📜 License

MIT License © 2025 — You.
Use it freely, modify, and share — but please be kind to the Yahoo Finance API.
