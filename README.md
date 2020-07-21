[![GitHub license](https://img.shields.io/github/license/damon-kwok/v-mode)](https://github.com/damon-kwok/v-mode/blob/master/COPYING)
[![MELPA](http://melpa.org/packages/v-mode-badge.svg)](http://melpa.org/#/v-mode)
[![MELPA Stable](http://stable.melpa.org/packages/v-mode-badge.svg)](http://stable.melpa.org/#/v-mode)

# V Mode

An Emacs major mode for the [V](https://vlang.io/) programming language.

- Screenshot

![screenshot](https://github.com/damon-kwok/v-mode/blob/master/screenshot.png)

## Features
- [X] Syntax highlighting (font-lock)
- [X] Indentation
- [x] `imenu`
- [x] TODO highlighting
- [x] Rainbow delimiters
- [x] Whitespace character dsiplay
- [x] Fill column indicator
- [x] Code folding
- [x] Workspaces
- [x] `V` mode menu
- [x] Auto format on save
- [x] Build command integration
- [ ] REPL
- [ ] Go to definition (using `ctags`)
- [ ] `Keywords` and `Methods name` autocomplete (using `company-mode`)

## Installation

### Using MELPA
This package can be obtain from
[MELPA](http://melpa.org/#/v-mode) or
[MELPA Stable](http://stable.melpa.org/#/v-mode). The `master`
branch is continuously deployed to `MELPA`, and released versions are
deployed to `MELPA Stable`.

<kbd>M-x package-install [RET] v-mode [RET]</kbd>

Right now `v-mode` doesn't take a lot of configuration (i.e.
it's too simple to need any).

```elisp
(require 'v-mode)
(define-key ponylang-mode-map (kbd "M-z") v-menu)
(define-key ponylang-mode-map (kbd "<f6>")  v-menu)
(define-key ponylang-mode-map (kbd "C-c C-f") v-format-buffer)
```

### Using [use-package](https://github.com/jwiegley/use-package) and [straight.el](https://github.com/raxod502/straight.el)

```elisp
(use-package v-mode
  :straight (v-mode
             :type git
             :host github
             :repo "damon-kwok/v-mode"
             :files ("tokens" "v-mode.el"))
  :config
  :bind-keymap
  ("M-z" . v-menu)
  ("<f6>" . v-menu)
  ("C-c C-f" . v-format-buffer)
  :mode ("\\.v\\'" . v-mode))
```
