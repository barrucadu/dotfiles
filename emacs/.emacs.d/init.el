;;; Bootstrap package management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;; User interface
;;;; Get rid of tool bar, menu bar, and scroll bars.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;;; No blinking and beeping.
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))
(setq ring-bell-function #'ignore)

;;;; No splash screen on startup
(setq inhibit-splash-screen t)

;;;; Zenburn colour theme
(use-package color-theme
  :ensure t)

(use-package zenburn-theme
  :ensure t
  :init (load-theme 'zenburn 'no-confirm))

;;;; Highlights
;;;;; ...cursor on window scroll
(use-package beacon
  :ensure t
  :init (beacon-mode 1)
  :diminish beacon-mode)

;;;;; ...current line
(use-package hl-line
  :init (global-hl-line-mode 1))

;;;;; ...TODOs in buffers
(use-package hl-todo
  :ensure t
  :defer t
  :init (global-hl-todo-mode))

;;;;; ...matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))

;;;;; ...sentences
(setq sentence-end-double-space nil) ; Adopt the double-space convention?
(use-package hl-sentence
  :ensure t
  :config
  (set-face-attribute 'hl-sentence-face nil :foreground "#bbbbff")
  (dolist (hook '(LaTeX-mode-hook markdown-mode-hook text-mode-hook))
    (add-hook hook #'hl-sentence-mode)))

;;;; The mode line
(line-number-mode)
(column-number-mode)

(use-package spaceline-config
  :ensure spaceline
  :config (spaceline-compile
    'barrucadu
    '(((buffer-modified buffer-size input-method) :face highlight-face)
      anzu
      '(buffer-id remote-host buffer-encoding-abbrev)
      ((point-position line-column buffer-position selection-info)
       :separator " | ")
      major-mode
      process
      (flycheck-error flycheck-warning flycheck-info)
      ((minor-modes :separator spaceline-minor-modes-separator) :when active))
    '((version-control :when active)))
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-barrucadu)))))

(use-package powerline
  :ensure t
  :after spaceline-config
  :config (setq powerline-height (truncate (* 1.0 (frame-char-height)))
                powerline-default-separator 'utf-8))

;;; Key bindings
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3
        which-key-sort-order 'which-key-prefix-then-key-order)

  (which-key-declare-prefixes
    ;; Global prefixes and minor modes
    "C-c @" "outline"
    "C-c !" "flycheck"
    ;; Prefixes for my personal bindings
    "C-c b"   "bufers"
    "C-c c"   "comments"
    "C-c f"   "files"
    "C-c g g" "github/gist"
    "C-c g"   "git"
    "C-c l"   "language/spelling"
    "C-c m"   "major-mode"
    "C-c o"   "cursors"
    "C-c s"   "search"
    "C-c t"   "toggle"
    "C-c x"   "text")

  ;; Prefixes for major modes
  (which-key-declare-prefixes-for-mode 'markdown-mode
    "C-c TAB" "markdown/images"
    "C-c C-a" "markdown/links"
    "C-c C-c" "markdown/process"
    "C-c C-s" "markdown/style"
    "C-c C-t" "markdown/header"
    "C-c C-x" "markdown/structure")

  (which-key-declare-prefixes-for-mode 'haskell-mode
    "C-c m" "haskell/personal"
    "C-c m i" "haskell/imports")

  (which-key-declare-prefixes-for-mode 'go-mode
    "C-c m" "go/personal")
  :diminish which-key-mode)

;;; Programming
;;;; Code checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;;; Commenting code
(use-package newcomment
  :bind (("C-c c d" . comment-dwim)
         ("C-c c l" . comment-line)
         ("C-c c r" . comment-region)))

;;;; Haskell
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :bind (:map haskell-mode-map
              ("M-."       . haskell-mode-jump-to-def-or-tag)
              ("C-c m i j" . haskell-navigate-imports)
              ("C-c m i s" . haskell-sort-imports)
              ("C-c m i a" . haskell-align-imports)))

(use-package haskell-compile
  :ensure haskell-mode
  :bind (:map haskell-mode-map
              ("C-c m c" . haskell-compile)
              ("<f5>"    . haskell-compile))
  :config (setq haskell-compile-cabal-build-command "stack build"))

(use-package cabal-mode
  :ensure haskell-mode
  :defer t)

;;;; Python
(use-package python
  :mode ("``.py``'" . python-mode)
  :config (add-hook 'python-mode-hook (lambda () (setq fill-column 79))))

(use-package anaconda-mode
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(use-package pip-requirements
  :ensure t
  :defer t)

;;;; Go
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :commands (godoc gofmt gofmt-before-save)
  :bind (:map go-mode-map
              ("C-c m f" . gofmt)
              ("C-c m i" . go-goto-imports)
              ("C-c m r" . go-remove-unused-imports))
  :init (progn (defun barrucadu/maybe-gofmt-before-save ()
                 (when (eq major-mode 'go-mode)
                   (gofmt-before-save)))
               (add-hook 'before-save-hook 'barrucadu/maybe-gofmt-before-save)))

;;;; LaTeX
(use-package tex-site
  :ensure auctex)

(use-package tex
  :ensure auctex
  :defer t
  :config (setq TeX-parse-self t
                TeX-auto-save t
                TeX-electric-sub-and-superscript t))

(use-package tex-buf
  :ensure auctex
  :defer t
  :config (setq TeX-save-query nil))

(use-package tex-style
  :ensure auctex
  :defer t
  :config (setq LaTeX-csquotes-close-quote "}"
                LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode
  :ensure auctex
  :defer t)

(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode))

(use-package bibtex
  :defer t
  :config
  (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  (bibtex-set-dialect 'biblatex))

;;;; Lua
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

;;;; Markup languages
(use-package rst
  :mode ("\\.rst\\'" . rst-mode)
  :config
  (setq rst-indent-literal-minimized 3
        rst-indent-literal-normal 3)
  (bind-key "C-=" nil rst-mode-map)
  (bind-key "C-c C-j" #'rst-insert-list rst-mode-map)
  (bind-key "M-RET" #'rst-insert-list rst-mode-map))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'\\|\\.markdown\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'"
  :config (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config (add-hook 'json-mode-hook (lambda () (setq-local js-indent-level 4))))

(use-package json-reformat
  :ensure t
  :defer t
  :bind (("C-c x j" . json-reformat-region)))

;;;; Nix
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;;;; Shell scripting
(use-package sh-script
  :defer t
  :mode ("\\.zsh\\'" . sh-mode)
  :config (setq sh-indentation 2
                sh-basic-offset 2))

;;;; TOML
(use-package toml-mode
  :ensure t
  :mode ("\\.toml``'" . toml-mode))

;;; Version control
(use-package vc-hooks
  :defer t
  :config (setq vc-follow-symlinks t))

(use-package diff-hl
  :ensure t
  :defer t
  :init
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (unless (display-graphic-p) (diff-hl-margin-mode))
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package magit
  :ensure t
  :bind (("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-buffer-line)
         ("C-c g p" . magit-pull))
  :config (setq magit-save-repository-buffers 'dontask
                magit-refs-show-commit-count 'all
                magit-revision-show-gravatars nil))

(use-package git-commit
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gitattributes-mode
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :bind (("C-c g t" . git-timemachine)))

(use-package gh
  :defer t
  :config (setq gh-profile-default-profile "barrucadu"))

(use-package magit-gh-pulls
  :ensure t
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package github-clone
  :ensure t
  :bind ("C-c g g c" . github-clone))

(use-package gist
  :ensure t
  :bind (("C-c g g l" . gist-list)
         ("C-c g g b" . gist-region-or-buffer)))

;;; Everything else!
;;;; No backup files
(setq make-backup-files nil)

;;;; Line killing
(setq kill-whole-line t)

;;;; Disable overzealous indenting
(electric-indent-mode 0)

;;;; Window management
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "C-c b x") 'switch-to-previous-buffer)

;;;; Whitespace management
(use-package whitespace-cleanup-mode
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish (whitespace-cleanup-mode . " [W]"))

(use-package whitespace
  :bind (("C-c t w" . whitespace-mode))
  :config (setq whitespace-line-column nil)
  :diminish whitespace-mode)

(global-set-key (kbd "C-c x d") 'delete-horizontal-space)

(setq-default show-trailing-whitespace t)

;;;; File navigation
(global-set-key (kbd "C-x g") 'goto-line)

;;;; Undo
(global-set-key (kbd "C-x C-u") 'undo)

;;;; Distraction-free writing
(use-package writeroom-mode
  :ensure t
  :bind (("C-c t r" . writeroom-mode)))

(use-package focus
  :ensure t
  :bind (("C-c t f" . focus-mode)))

;;;; Use anzu search-and-replace
(use-package anzu
  :ensure t
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         ("M-r" . anzu-replace-at-cursor-thing)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (global-anzu-mode)
  :config (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

;;;; Autosave when defocussing a buffer
(use-package focus-autosave-mode
  :ensure t
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

;;;; Make buffer names unique
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;;;; Save place in files
(use-package saveplace
  :init (setq-default sace-place t)
  :config (setq save-place-file "~/.emacs.d/saved-places"))

;;;; Edit files as root
(use-package sudo-edit
  :ensure t
  :bind (("C-c f s" . sudo-edit)
         ("C-c f S" . sudo-edit-current-file)))

;;;; Regexp replace with in-buffer display
(use-package visual-regexp
  :ensure t
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace)))

;;;; Change yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Disable tabs
(setq-default indent-tabs-mode nil
              tab-width 8)

;;;; Require a trailing newline
(setq require-final-newline t)

;;;; Folding
(use-package outshine
  :ensure t
  :defer t
  :commands (outshine-hook-function)
  :init
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  :config (require 'outshine))

;;;; Edit text with multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-c o <SPC>" . mc/vertical-align-with-space)
         ("C-c o a"     . mc/vertical-align)
         ("C-c o e"     . mc/mark-more-like-this-extended)
         ("C-c o h"     . mc/mark-all-like-this-dwim)
         ("C-c o l"     . mc/edit-lines)
         ("C-c o n"     . mc/mark-next-line-like-this)
         ("C-c o p"     . mc/mark-previous-line-like-this)
         ("C-c o r"     . vc/mc-mark)
         ("C-c o C-a"   . mc/edit-beginnings-of-lines)
         ("C-c o C-e"   . mc/edit-ends-of-lines)
         ("C-c o C-s"   . mc/mark-all-in-region)))

;;;; Expand region by semantic units
(use-package expand-region
  :ensure t
  :bind (("C-c v" . er/expand-region)))

;;;; Spell checking
(use-package ispell
  :defer t
  :config (setq ispell-program-name (executable-find "aspell")
                ispell-dictionary "en_GB"
                ispell-silently-savep t
                ispell-choices-win-default-height 5))

(use-package flyspell
  :bind (("C-c t s" . flyspell-mode)
         ("C-c l b" . flyspell-buffer))
  :init (progn (dolist (hook '(text-mode-hook message-mode-hook))
                 (add-hook hook 'turn-on-flyspell))
               (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config (progn (setq flyspell-use-meta-tab nil
                       flyspell-issue-welcome-flag nil
                       flyspell-issue-message-flag nil)
                 (define-key flyspell-mode-map "\M-\t" nil)
                 (define-key flyspell-mouse-map [down-mouse-2] nil)
                 (define-key flyspell-mouse-map [mouse-2] nil))
  :diminish flyspell-mode)

;;;; org-mode
(setq initial-major-mode 'org-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
