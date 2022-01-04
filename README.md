<a href="https://www.gnu.org/software/emacs/"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right"></a>
[![GitHub license](https://img.shields.io/github/license/damon-kwok/v-mode?logo=gnu&.svg)](https://github.com/damon-kwok/v-mode/blob/master/COPYING)
[![Sponsor](https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg)](https://www.patreon.com/DamonKwok)
[![MELPA](http://melpa.org/packages/v-mode-badge.svg)](http://melpa.org/#/v-mode)
<!-- [![MELPA Stable](http://stable.melpa.org/packages/v-mode-badge.svg)](http://stable.melpa.org/#/v-mode) -->

# V Mode

An Emacs major mode for the [V](https://vlang.io/) programming language.

- Screenshot

![screenshot](https://github.com/damon-kwok/v-mode/blob/master/screenshot.png)

## Features

- [x] Syntax highlighting (font-lock)
- [x] Indentation
- [x] Workspace support
- [x] Auto format on save
- [x] Compilation integration
- [x] Code navigation (using `imenu`)
- [x] Go to definition (using `ctags`)
- [x] Code completion (using `company-mode`)
- [ ] REPL
<!-- - [x] TODO highlighting -->
<!-- - [x] Rainbow delimiters -->
<!-- - [x] Whitespace character dsiplay -->
<!-- - [x] Fill column indicator -->
<!-- - [x] `V` mode menu -->
<!-- - [x] Code folding -->

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
(define-key v-mode-map (kbd "M-z") 'v-menu)
(define-key v-mode-map (kbd "<f6>")  'v-menu)
(define-key v-mode-map (kbd "C-c C-f") 'v-format-buffer)
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
  :mode ("\\(\\.v?v\\|\\.vsh\\)$" . 'v-mode))
```
