# Yahoo Stock Price Package for Emacs

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
  :commands (yf-read-ticker-and-insert-price yf-convert-line-and-insert-result)
  :init
  (map! :leader
        :desc "Read the ticker from the current line, and insert the price"
        "y y" #'yf-read-ticker-and-insert-price)
  (map! :leader
        :desc "Parse a currency conversion expression (e.g.: 1 usd to huf) from the current line."
        "y c" #'yf-convert-line-and-insert-result))
```

### Examples

```lisp
(yf-get "QQQ")
(yf-get-price "QQQ")
(yf-convert 100 "USD" "EUR")
```
