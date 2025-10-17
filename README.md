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
(use-package! yf-mode
  :mode "\\.yf"
  :defer t
  :commands (yf-eval-current-line
             yf-eval-buffer)
  :init
  (map! :leader
        :desc "Eval line as Yahoo Finance postfix expression."
        "y e" #'yf-eval-current-line)
  (map! :leader
        :desc "Eval buffer as Yahoo Finance postfix expression."
        "y b" #'yf-eval-buffer)
  (map! :leader
        :desc "Delete all overlays created by yf."
        "y d" #'yf-delete-overlays)
  (map! :leader
        :desc "Start Yahoo Finace REPL."
        "y s" #'yf-repl-start))

(use-package! yf-repl
  :defer t
  :commands yf-repl-start
  :init
  (map! :leader
        :desc "Start Yahoo Finace REPL."
        "y s" #'yf-repl-start))
```

### Examples

```lisp
(yf-get "QQQ")
(yf-convert 100 "USD" "EUR")
```
