;; -*- lexical-binding: t -*-
;;; v-mode.el --- A major mode for the V programming language
;;
;; Authors: Damon Kwok <damon-kwok@outlook.com>
;; Version: 0.0.1
;; URL: https://github.com/damon-kwok/v-mode
;; Keywords: languages programming
;; Package-Requires: ((dash "2.17.0") (hydra "0.15.0") (hl-todo "3.1.2") (yafolding "0.4.1") (yasnippet "0.14.0") (rainbow-delimiters "2.1.4") (fill-column-indicator "1.90"))
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2020 Damon Kwok
;;
;;; Commentary:
;;
;; Description:
;;
;; This is a major mode for the V programming language
;;
;; For more details, see the project page at
;; https://github.com/damon-kwok/v-mode
;;
;; Installation:
;;
;; The simple way is to use package.el:
;;
;;   M-x package-install v-mode
;;
;; Or, copy v-mode.el to some location in your emacs load
;; path. Then add "(require 'v-mode)" to your emacs initialization
;; (.emacs, init.el, or something).
;;
;; Example config:
;;
;;   (require 'v-mode)
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'cl)
(require 'js)
(require 'dash)
(require 'xref)
(require 'hydra)
(require 'imenu)
(require 'hl-todo)
(require 'easymenu)
(require 'yafolding)
(require 'yasnippet)
(require 'whitespace)
(require 'rainbow-delimiters)
(require 'fill-column-indicator)

(defvar v-mode-hook nil)
(defcustom v-indent-trigger-commands    ;
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `v-indent-line' call."
  :type '(repeat symbol)
  :group 'v)

(defconst v-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; fontify " using v-keywords

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?= ?! ?< ?>))
      (modify-syntax-entry i "." table))

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 124" table)

    ;; /* */ comments, which can be nested
    (modify-syntax-entry ?* ". 23bn" table)

    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)

    ;; string
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)

    ;; Don't treat underscores as whitespace
    (modify-syntax-entry ?_ "w" table) table))

(defvar v-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    ;; (define-key map (kbd "<C-return>") 'yafolding-toggle-element) ;
    map)
  "Keymap for V major mode")

(defconst v-keywords
  '("go" "in" "is" "or"                 ;
     "if" "else" "for" "match")
  "V language keywords.")

(defconst v-declaration-keywords        ;
  '("type" "interface" "struct" "enum" "fn")
  "V declaration keywords.")

(defconst v-preprocessor-keywords       ;
  '("module"  "import" "pub" "const"    ;
     "go" "__global" "inline")
  "V declaration keywords.")

(defconst v-careful-keywords
  '("break" "continue" "return" "goto"  ;
     "defer" "panic"                    ;
     "as" "assert"  "unsafe" "mut")
  "V language careful keywords.")

(defconst v-builtin-keywords
  '("string" "bool"                         ;
     "i8" "i16" "int" "i64" "i128"          ;
     "byte" "u16" "u32" "u64" "u128"        ;
     "rune"                                 ;
     "f32" "f64"                            ;
     "byteptr" "voidptr" "charptr" "size_t" ;
     "any" "any_int" "any_float")
  "V language keywords.")

(defconst v-constants                   ;
  '("false" "true" "none")
  "Common constants.")

(defconst v-operator-functions
  '()
  "V language operators functions.")

;; create the regex string for each class of keywords

(defconst v-keywords-regexp (regexp-opt v-keywords 'words)
  "Regular expression for matching keywords.")

(defconst v-declaration-keywords-regexp ;
  (regexp-opt v-declaration-keywords 'words)
  "Regular expression for matching declaration keywords.")

(defconst v-preprocessor-keywords-regexp ;
  (regexp-opt v-preprocessor-keywords 'words)
  "Regular expression for matching preprocessor keywords.")

(defconst v-careful-keywords-regexp     ;
  (regexp-opt v-careful-keywords 'words)
  "Regular expression for matching careful keywords.")

(defconst v-builtin-keywords-regexp (regexp-opt v-builtin-keywords 'words)
  "Regular expression for matching builtin type.")

(defconst v-constant-regexp             ;
  (regexp-opt v-constants 'words)
  "Regular expression for matching constants.")

(defconst v-operator-functions-regexp   ;
  (regexp-opt v-operator-functions 'words)
  "Regular expression for matching operator functions.")

(defconst v-font-lock-keywords
  `(
     ;; builtin
     (,v-builtin-keywords-regexp . font-lock-builtin-face)

     ;; careful
     (,v-careful-keywords-regexp . font-lock-warning-face)

     ;; declaration
     (,v-declaration-keywords-regexp . font-lock-keyword-face)

     ;; preprocessor
     (,v-preprocessor-keywords-regexp . font-lock-preprocessor-face)

     ;; delimiter: modifier
     ("\\(->\\|=>\\|\\.>\\|:>\\|:=\\|\\.\\.\\||\\)" 1 'font-lock-keyword-face)

     ;; delimiter: . , ; separate
     ("\\($?[.,;]+\\)" 1 'font-lock-comment-delimiter-face)

     ;; delimiter: operator symbols
     ("\\($?[+-/*//%~=<>]+\\)$?,?" 1 'font-lock-negation-char-face)
     ("\\($?[?^!&]+\\)" 1 'font-lock-warning-face)

     ;; delimiter: = : separate
     ("[^+-/*//%~^!=<>]\\([=:]\\)[^+-/*//%~^!=<>]" 1 'font-lock-comment-delimiter-face)

     ;; delimiter: brackets
     ("\\(\\[\\|\\]\\|[(){}]\\)" 1 'font-lock-comment-delimiter-face)

     ;; operator methods
     (,v-operator-functions-regexp . font-lock-builtin-face)

     ;; macro
     ("#\\(?:include\\|if\\|ifdef\\|else\\|elif\\|endif\\)" . 'font-lock-builtin-face)

     ;; method definitions
     ("\\(?:fn\\)\s+\\($?[a-z_][A-Za-z0-9_]*\\)" 1 'font-lock-function-name-face)

     ;; type
     ("\\([A-Z][A-Za-z0-9_]*\\)" 1 'font-lock-type-face)

     ;; constants references
     (,v-constant-regexp . font-lock-constant-face)

     ;; @
     ("@[A-Za-z_]*[A-Z-a-z0-9_]*" . 'font-lock-builtin-face)

     ;; method references
     ("\\([a-z_]$?[a-z0-9_]?+\\)$?[ \t]?(+" 1 'font-lock-function-name-face)

     ;; parameter
     ("\\(?:(\\|,\\)\\([a-z_][a-z0-9_']*\\)\\([^ \t\r\n,:)]*\\)" 1 'font-lock-variable-name-face)
     ("\\(?:(\\|,\\)[ \t]+\\([a-z_][a-z0-9_']*\\)\\([^ \t\r\n,:)]*\\)" 1
       'font-lock-variable-name-face)

     ;; tuple references
     ("[.]$?[ \t]?\\($?_[1-9]$?[0-9]?*\\)" 1 'font-lock-variable-name-face)

     ;; keywords
     (,v-keywords-regexp . font-lock-keyword-face) ;;font-lock-keyword-face

     ;; character literals
     ("\\('[\\].'\\)" 1 'font-lock-constant-face)

     ;; numeric literals
     ("[ \t/+-/*//=><([,;]\\([0-9]+[0-9a-zA-Z_]*\\)+" 1 'font-lock-constant-face)

     ;; variable references
     ("\\([a-z_]+[a-z0-9_']*\\)+" 1 'font-lock-variable-name-face))
  "An alist mapping regexes to font-lock faces.")

(defun v-beginning-of-defun
  (&optional
    count)
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (odin-paren-level)))
    (while (and (not (odin-line-is-defun))
             (not (bobp))
             (> orig-level 0))
      (setq orig-level (odin-paren-level))
      (while (>= (odin-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (if (odin-line-is-defun)
    (beginning-of-line)))

(defun v-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (odin-paren-level)))
    (when (> orig-level 0)
      (odin-beginning-of-defun)
      (end-of-line)
      (setq orig-level (odin-paren-level))
      (skip-chars-forward "^}")
      (while (>= (odin-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

(defalias 'v-parent-mode                ;
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

(defun v-folding-hide-element (&optional RETRY)
  "Hide current element."
  (interactive)
  (let* ((region (yafolding-get-element-region))
         (beg (car region))
         (end (cadr region)))
    (if (and (eq RETRY nil) (= beg end))
        (progn
          (yafolding-go-parent-element)
          (yafolding-hide-element 1))
      (yafolding-hide-region beg end))))

;;;###autoload
(define-derived-mode v-mode v-parent-mode
  "V"
  "Major mode for editing V files."
  :syntax-table v-mode-syntax-table
  (setq bidi-paragraph-direction 'left-to-right)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start "/*")
  (setq-local comment-start "*/")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local electric-indent-chars (append "{}():;," electric-indent-chars))
  (setq-local beginning-of-defun-function 'v-beginning-of-defun)
  (setq-local end-of-defun-function 'v-end-of-defun)
  (setq-local indent-line-function 'js-indent-line)

  ;; (setq-local font-lock-defaults        ;
  ;; '(v-font-lock-keywords ;
  ;; nil nil nil nil         ;
  ;; (font-lock-syntactic-face-function . v-mode-syntactic-face-function)))
  (setq-local font-lock-defaults '(v-font-lock-keywords))
  (font-lock-fontify-buffer)

  ;; (setq-local syntax-propertize-function v-syntax-propertize-function)

  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local buffer-file-coding-system 'utf-8-unix)
  ;;
  (hl-todo-mode)
  (setq-local hl-todo-keyword-faces
    '(("TODO" . "green")
       ("FIXME" . "yellow")
       ("DEBUG" . "DarkCyan")
       ("GOTCHA" . "red")
       ("STUB" . "DarkGreen")))
  (whitespace-mode)
  (setq-local whitespace-style
    '(face spaces tabs newline space-mark tab-mark newline-mark trailing))
  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq-local whitespace-display-mappings
    ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
    '((space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
       (newline-mark 10 [182 10]) ; LINE FEED,
       (tab-mark 9 [9655 9] [92 9])))

  ;; (setq-local whitespace-style '(face trailing))
  (setq-local fci-rule-column 80)
  (setq-local fci-handle-truncate-lines nil)
  (setq-local fci-rule-width 1)
  (setq-local fci-rule-color "grey30")

  (rainbow-delimiters-mode t)

  (defalias 'yafolding-hide-element 'v-folding-hide-element)
  (yafolding-mode t)

  (setq-local imenu-generic-expression
    '(("TODO" ".*TODO:[ \t]*\\(.*\\)$" 1)
       ("fn" "^[ \t]*fn[ \t]+(.*)[ \t]+\\(.*\\)[ \t]*(.*)" 1)
       ("struct" "^[ \t]*struct[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("import" "^[ \t]*import[ \t]+\\([a-zA-Z0-9_]+\\)" 1)))
  (imenu-add-to-menubar "Index"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.v\\'" . v-mode))

;;
(provide 'v-mode)
;; v-mode.el ends here
