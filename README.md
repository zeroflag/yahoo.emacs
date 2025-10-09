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
  :commands yf-read-ticker-and-insert-price
  :defer t
  :init
  (map! :leader
        :desc "Read the ticker from the current line, and insert the price"
        "y y" #'yf-read-ticker-and-insert-price))
```

```lisp
(yf-get "QQQ")
(yf-get-price "QQQ")
```
