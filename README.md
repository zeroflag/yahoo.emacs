# Yahoo Stock Price Package for Emacs

![{master}](https://github.com/zeroflag/yahoo.emacs/actions/workflows/ci.yml/badge.svg)

## Doom Emacs Config

### packages.el

```lisp
(package! yf
  :recipe (:host github :repo "zeroflag/yahoo.emacs"))
```

### config.el

```lisp
(use-package! yf
  :defer t
  :commands (yf-eval-current-line)
  :init
  (map! :leader
        :desc "Resolve tickers and currencies in the current line"
        "y r" #'yf-eval-current-line))
```

### Examples

```lisp
(yf-get "QQQ")
(yf-get-price "QQQ")
(yf-convert 100 "USD" "EUR")
```
