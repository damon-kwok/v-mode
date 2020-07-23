;;; v-mode.el --- A major mode for the V programming language
;;
;; Authors: Damon Kwok <damon-kwok@outlook.com>
;; Version: 0.0.1
;; URL: https://github.com/damon-kwok/v-mode
;; Keywords: languages programming
;; Package-Requires: ((emacs "25.1") (dash "2.17.0") (hydra "0.15.0") (hl-todo "3.1.2") (yafolding "0.4.1") (yasnippet "0.14.0") (company-ctags "0.0.4") (rainbow-delimiters "2.1.4") (fill-column-indicator "1.90"))
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
;; Or, copy v-mode.el to some location in your Emacs load
;; path.  Then add "(require 'v-mode)" to your Emacs initialization
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

(require 'cl-lib)
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
(require 'company-ctags)
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
  "Keymap for V major mode.")

(defconst v-keywords '("if" "else" "for" "match")
  "V language keywords.")

(defconst v-declaration-keywords        ;
  '("type" "interface" "struct" "enum" "fn")
  "V declaration keywords.")

(defconst v-preprocessor-keywords '("module" "pub" "const")
  "V preprocessor keywords.")

(defconst v-careful-keywords
  '("import"                            ;
     "break" "continue" "return" "goto" ;
     "defer" "panic" "error"            ;
     "in" "is" "or"                     ;
     "go" "inline" "live"               ;
     "as" "assert"  "unsafe" "mut"      ;
     "__global" "C")
  "V language careful keywords.")

(defconst v-builtin-keywords
  '("string" "bool"                         ;
     "i8" "i16" "int" "i64" "i128"          ;
     "byte" "u16" "u32" "u64" "u128"        ;
     "rune"                                 ;
     "f32" "f64"                            ;
     "byteptr" "voidptr" "charptr" "size_t" ;
     "any" "any_int" "any_float"            ;
     "it")
  "V language keywords.")

(defconst v-constants                   ;
  '("false" "true" "none")
  "Common constants.")

(defconst v-operator-functions '()
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

     ;; @ # $
     ;; ("#\\(?:include\\|flag\\)" . 'font-lock-builtin-face)
     ("[@#$][A-Za-z_]*[A-Z-a-z0-9_]*" . 'font-lock-warning-face)

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

     ;; numeric literals
     ;; ("[^A-Za-z_]\\([0-9][A-Za-z0-9_]*\\)" 1 'font-lock-constant-face)
     ("[ \t/+-/*//=><([{,;&|%]\\([0-9][A-Za-z0-9_]*\\)" 1 'font-lock-constant-face)

     ;; operator methods
     (,v-operator-functions-regexp . font-lock-builtin-face)

     ;; method definitions
     ("\\(?:fn\\)\s+\\($?[a-z_][A-Za-z0-9_]*\\)" 1 'font-lock-function-name-face)

     ;; type
     ("\\([A-Z][A-Za-z0-9_]*\\)" 1 'font-lock-type-face)

     ;; constants references
     (,v-constant-regexp . font-lock-constant-face)

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

     ;; variable references
     ("\\($?_?[a-z]+[a-z_0-9]*\\)" 1 'font-lock-variable-name-face))
  "An alist mapping regexes to font-lock faces.")

(defun v-project-root-p (PATH)
  "Return t if directory `PATH' is the root of the V project."
  (setq-local files '("v.mod" "make.bat" "Makefile" ;
                       "Dockerfile" ".editorconfig" ".gitignore"))
  (setq-local foundp nil)
  (while (and files
           (not foundp))
    (let* ((filename (car files))
            (filepath (concat (file-name-as-directory PATH) filename)))
      (setq-local files (cdr files))
      (setq-local foundp (file-exists-p filepath))))
  foundp)

(defun v-project-root
  (&optional
    PATH)
  "Return the root of the V project.
Optional argument PATH ."
  (let* ((bufdir (if buffer-file-name   ;
                   (file-name-directory buffer-file-name) default-directory))
          (curdir (if PATH (file-name-as-directory PATH) bufdir))
          (parent (file-name-directory (directory-file-name curdir))))
    (if (or (not parent)
          (string= parent curdir)
          (string= parent "/")
          (v-project-root-p curdir))    ;
      curdir                            ;
      (v-project-root parent))))

(defun v-project-name ()
  "Return V project name."
  (file-name-base (directory-file-name (v-project-root))))

(defun v-project-file-exists-p (FILENAME)
  "Return t if file `FILENAME' exists."
  (file-exists-p (concat (v-project-root) FILENAME)))

(defun v-run-command (COMMAND &optional PATH)
  "Return `COMMAND' in the root of the V project.
Optional argument PATH ."
  (setq default-directory (if PATH PATH (v-project-root PATH)))
  (compile COMMAND))

(defun v-project-build ()
  "Build project with v."
  (interactive)
  (if (v-project-file-exists-p "Makefile")
    (v-run-command "make")
    (v-run-command "v .")))

(defun v-project-init ()
  "Run corral `init' command."
  (interactive)
  (unless (v-project-file-exists-p "v.mod")
    (v-run-command "v init")))

(defun v-project-update ()
  "Run corral `update' command."
  (interactive)
  (if (v-project-file-exists-p "v.mod")
    (v-run-command "v update")))

(defun v-project-open ()
  "Open `v.mod' file."
  (interactive)
  (if (v-project-file-exists-p "v.mod")
    (find-file (concat (v-project-root) "v.mod"))))

(defun v-buffer-dirname ()
  "Return current buffer directory file name."
  (directory-file-name (if buffer-file-name (file-name-directory buffer-file-name)
                         default-directory)))

(defun v-project-run ()
  "Run project."
  (interactive)
  (let* ((bin1 (concat (v-project-root) "bin/" (v-project-name)))
          (bin2 (concat (v-project-root) "/" (v-project-name)))
          (bin3 (concat (v-buffer-dirname) "/" (v-project-name))))
    (if (file-exists-p bin1)
      (v-run-command bin1)
      (if (file-exists-p bin2)
        (v-run-command bin2)
        (if (file-exists-p bin3)
          (v-run-command bin3))))))

(easy-menu-define v-mode-menu v-mode-map ;
  "Menu for V mode."                     ;
  '("V"                                  ;
     ["Build" v-project-build t]         ;
     ["Run" v-project-run t]             ;
     ["Init" v-project-init t]           ;
     ["Open" v-project-open t]           ;
     ["Update" v-project-update t]       ;
     "---"                               ;
     ("Community"                        ;
       ["News" (v-run-command "xdg-open https://twitter.com/v_language") t]
       ["Discord" (v-run-command "xdg-open https://discord.gg/vlang") t]
       ["Open an issue" (v-run-command "xdg-open https://github.com/vlang/v/issues") t]
       ["Tutorial" (v-run-command "xdg-open https://github.com/vlang/v/blob/master/doc/docs.md") t]
       ["Awesome-V" ("xdg-open https://github.com/vlang/awesome-v") t]
       ["Contribute" (v-run-command
                       "xdg-open https://github.com/vlang/v/blob/master/CONTRIBUTING.md") t]
       ["Supporter" (v-run-command "xdg-open https://patreon.com/vlang") t])))

(defun v-banner-default ()
  "V banner."
  "
  __   __
  \\ \\ / /
   \\ V /
    \\_/
")

(defhydra v-hydra-menu
  (:color blue
    :hint none)
  "
%s(v-banner-default)
  Project     |  _i_: Init      _u_: Update     _o_: v.mod
              |  _b_: Build     _r_: Run
  Community   |  _1_: News      _2_: Discord    _3_: OpenIssue
              |  _4_: Tutorial  _5_: Awesome-V  _6_: Sponsors  _0_: Contribute
  _q_: Quit"                            ;
  ("b" v-project-build "Build")
  ("r" v-project-run "Run")
  ("o" v-project-open "Open v.mod")
  ("i" v-project-init "v init")
  ("u" v-project-update "v udate")
  ("1" (v-run-command "xdg-open https://twitter.com/v_language") "News")
  ("2" (v-run-command "xdg-open https://discord.gg/vlang") "Discord")
  ("3" (v-run-command "xdg-open https://github.com/vlang/v/issues") "Open an issue")
  ("4" (v-run-command "xdg-open https://github.com/vlang/v/blob/master/doc/docs.md") "Docs")
  ("5" (v-run-command "xdg-open https://github.com/vlang/awesome-v") "Awesome-V")
  ("6" (v-run-command "xdg-open https://patreon.com/vlang") "Supporter")
  ("0" (v-run-command "xdg-open https://github.com/vlang/v/blob/master/CONTRIBUTING.md")
    "Contribute")
  ("q" nil "Quit"))

(defun v-menu ()
  "Open v hydra menu."
  (interactive)
  (v-hydra-menu/body))

(defun v-folding-hide-element
  (&optional
    RETRY)
  "Hide current element.
Optional argument RETRY ."
  (interactive)
  (let* ((region (yafolding-get-element-region))
          (beg (car region))
          (end (cadr region)))
    (if (and (eq RETRY nil)
          (= beg end))
      (progn (yafolding-go-parent-element)
        (yafolding-hide-element 1))
      (yafolding-hide-region beg end))))

(defun v-build-tags ()
  "Build tags for current project."
  (interactive)
  (let ((tags-buffer (get-buffer "TAGS"))
         (tags-buffer2 (get-buffer (format "TAGS<%s>" (v-project-name)))))
    (if tags-buffer (kill-buffer tags-buffer))
    (if tags-buffer2 (kill-buffer tags-buffer2)))
  (let* ((v-path (string-trim (shell-command-to-string "which v")))
          (v-executable (string-trim (shell-command-to-string (concat "readlink -f " v-path))))
          (packages-path (concat (file-name-directory v-executable) "vlib") )
          (ctags-params                 ;
            (concat  "ctags --languages=-v --langdef=v --langmap=v:.v "
              "--regex-v=/[ \\t]*fn[ \\t]+(.*)[ \\t]+(.*)/\\2/f,function/ "
              "--regex-v=/[ \\t]*struct[ \\t]+([a-zA-Z0-9_]+)/\\1/s,struct/ "
              "--regex-v=/[ \\t]*interface[ \\t]+([a-zA-Z0-9_]+)/\\1/i,interface/ "
              "--regex-v=/[ \\t]*type[ \\t]+([a-zA-Z0-9_]+)/\\1/t,type/ "
              "--regex-v=/[ \\t]*enum[ \\t]+([a-zA-Z0-9_]+)/\\1/e,enum/ "
              "--regex-v=/[ \\t]*module[ \\t]+([a-zA-Z0-9_]+)/\\1/m,module/ " ;
              "-e -R . " packages-path)))
    (if (file-exists-p packages-path)
      (progn
        (setq default-directory (v-project-root))
        (let (result (shell-command-to-string ctags-params))
          (if (not (eq "" result))
            (message "ctags:%s" result)))
        (v-load-tags)))))

(defun v-load-tags
  (&optional
    BUILD)
  "Visit tags table.
Optional argument BUILD ."
  (interactive)
  (let* ((tags-file (concat (v-project-root) "TAGS")))
    (if (file-exists-p tags-file)
      (progn (visit-tags-table (concat (v-project-root) "TAGS")))
      (if BUILD (v-build-tags)))))

(defun v-format-buffer ()
  "Format the current buffer using the v fmt."
  (interactive)
  (shell-command (concat  "v fmt -w " (buffer-file-name)))
  (revert-buffer
    :ignore-auto
    :noconfirm))

(defun v-after-save-hook ()
  "After save hook."
  (v-format-buffer)
  (if (not (executable-find "ctags"))
    (message "Could not locate executable '%s'" "ctags")
    (v-build-tags)))

(defalias 'v-parent-mode                ;
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode v-mode v-parent-mode
  "v"
  "Major mode for editing V files."
  :syntax-table v-mode-syntax-table
  (setq bidi-paragraph-direction 'left-to-right)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start "/*")
  (setq-local comment-start "*/")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local electric-indent-chars (append "{}():;," electric-indent-chars))
  (setq-local indent-line-function 'js-indent-line)
  ;;
  ;; (setq-local font-lock-defaults        ;
  ;; '(v-font-lock-keywords ;
  ;; nil nil nil nil         ;
  ;; (font-lock-syntactic-face-function . v-mode-syntactic-face-function)))
  (setq-local font-lock-defaults '(v-font-lock-keywords))
  (font-lock-ensure)
  ;;
  ;; (setq-local syntax-propertize-function v-syntax-propertize-function)
  ;;
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local buffer-file-coding-system 'utf-8-unix)
  ;;
  (hl-todo-mode)
  (setq-local hl-todo-keyword-faces ;;
    '(("TODO" . "green")
       ("FIXME" . "yellow")
       ("DEBUG" . "DarkCyan")
       ("GOTCHA" . "red")
       ("STUB" . "DarkGreen")))
  (whitespace-mode)
  (setq-local whitespace-style ;;
    '(face spaces tabs newline space-mark tab-mark newline-mark trailing))
  ;; Make whitespace-mode and whitespace-newline-mode
  ;; use “¶” for end of line char and “▷” for tab.
  (setq-local whitespace-display-mappings
    ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
    '((space-mark 32 [183]
        [46])         ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
       (newline-mark 10 [182 10])       ; LINE FEED,
       (tab-mark 9 [9655 9]
         [92 9])))

  ;; (setq-local whitespace-style '(face trailing))
  (setq-local fci-rule-column 80)
  (setq-local fci-handle-truncate-lines nil)
  (setq-local fci-rule-width 1)
  (setq-local fci-rule-color "grey30")
  ;;
  (rainbow-delimiters-mode t)
  ;;
  (defalias 'yafolding-hide-element 'v-folding-hide-element)
  (yafolding-mode t)
  ;;
  (setq-local imenu-generic-expression ;;
    '(("TODO" ".*TODO:[ \t]*\\(.*\\)$" 1)
       ("fn" "[ \t]*fn[ \t]+(.*)[ \t]+\\(.*\\)[ \t]*(.*)" 1)
       ("struct" "[ \t]*struct[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("interface" "[ \t]*interface[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("type" "[ \t]*type[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("enum" "[ \t]*enum[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("import" "[ \t]*import[ \t]+\\([a-zA-Z0-9_]+\\)" 1)))
  (imenu-add-to-menubar "Index")
  ;;
  (add-hook 'after-save-hook 'v-after-save-hook nil t)
  (v-load-tags))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.v\\'" . v-mode))

;;
(provide 'v-mode)

;;; v-mode.el ends here
