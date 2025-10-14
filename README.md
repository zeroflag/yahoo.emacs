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
  :commands (yf-eval-current-line
             yf-eval-buffer
             yf-repl-mode
             yf-start-repl)
  :init
  (map! :leader
        :desc "Eval line as Yahoo Finance postfix expression."
        "y e" #'yf-eval-current-line)
  (map! :leader
        :desc "Eval buffer as Yahoo Finance postfix expression."
        "y b" #'yf-eval-buffer)
  (map! :leader
        :desc "Start Yahoo Finace REPL."
        "y s" #'yf-start-repl)
  (map! :leader
        :desc "Switch to Yahoo Finance REPL Mode."
        "y r" #'yf-repl-mode))
```

### Examples

```lisp
(yf-get "QQQ")
(yf-convert 100 "USD" "EUR")
```
