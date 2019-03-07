;;; init.el --- Initialisation for emacs
;;; Commentary:
;; My configuration
;;; Code:

;;;; Basics

(setq user-full-name "Michael Walker")
(setq user-mail-address "mike@barrucadu.co.uk")

;;; Package management
(require 'package)

(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Automatically install missing packages
(setq use-package-always-ensure t)

;; Disable automatic loading of all installed packages
(setq package-enable-at-startup nil)

;;; Better defaults
;; Disable UI widgets
(tool-bar-mode 0)
(menu-bar-mode 0)
(when (display-graphic-p)
  (set-scroll-bar-mode nil))

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Highlight the current line
(global-hl-line-mode 1)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable automatic indentation on newline
(electric-indent-mode 0)

;; Disable the bell
(setq ring-bell-function 'ignore)

;; Disable the splash screen, giving an empty *scratch* buffer open in
;; org-mode
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq initial-major-mode 'org-mode)

;; Don't make backup files - everything I edit is version-controlled
(setq make-backup-files nil)

;; C-k at the start of a line also removes the newline
(setq kill-whole-line t)

;; Add a newline to the end of files
(setq require-final-newline t)

;; Uniquify buffer names by prepending the folder name
(setq uniquify-buffer-name-style 'forward)

;; Don't ask to follow symlinks to version-controlled files, just do
;; it
(setq vc-follow-symlinks t)

;; Start scrolling smoothly when the point is more than 7 lines away
;; from the centre
(setq scroll-margin 7)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)

;; Indent with spaces, and display literal tab characters as 8 spaces
;; wide (these are buffer-local variables so use setq-default instead
;; of setq)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Write customised variables to ~/.emacs/custom.el - and don't load
;; it (making this configuration self-contained)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;;; Appearance

;; Colour theme
(use-package color-theme)
(use-package gruvbox-theme
  :init (load-theme 'gruvbox 'no-confirm))

;; Colour nested brackets differently
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (conf-mode . rainbow-delimiters-mode)))

;; The mode line
(setq-default mode-line-format
  (list
    "%b "
    '(:eval (when buffer-read-only    "[read-only] "))
    '(:eval (when (buffer-modified-p) "[modified] "))
    "<"
    '(:eval (cond ((evil-normal-state-p)   "normal")
                  ((evil-emacs-state-p)    "emacs")
                  ((evil-insert-state-p)   "insert")
                  ((evil-motion-state-p)   "motion")
                  ((evil-visual-state-p)   "visual")
                  ((evil-operator-state-p) "operator")
                  ((evil-replace-state-p)  "replace")
                  (t "?")))
    "> (%c, %l ~ %p) - "
    '(:eval mode-line-modes)
    "%-"))

;;;; Keybindings

(defun barrucadu/evil-next-line (orig-fun &rest args)
  "Make evil-next-line use visual lines."
  (if visual-line-mode
      (apply 'evil-next-visual-line args)
    (apply orig-fun args)))

(defun barrucadu/evil-previous-line (orig-fun &rest args)
  "Make evil-previous-line use visual lines."
  (if visual-line-mode
      (apply 'evil-previous-visual-line args)
    (apply orig-fun args)))

(use-package evil
  :ensure t
  :demand t
  :config
  (evil-mode 1)
  (defalias 'evil-insert-state 'evil-emacs-state)
  (setq evil-want-visual-char-semi-exclusive t
        evil-emacs-state-cursor  '("red" box)
        evil-normal-state-cursor '("green" box)
        evil-visual-state-cursor '("orange" box))
  (setq-default evil-cross-lines t)
  (advice-add 'evil-next-line :around 'barrucadu/evil-next-line)
  (advice-add 'evil-previous-line :around 'barrucadu/evil-previous-line)
  :hook
  (git-commit-mode . evil-emacs-state))

(use-package which-key
  :init    (which-key-mode)
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.05))

(defconst *top-level-leader*  "C-c")
(defconst *major-mode-leader* "C-c m")

(defun barrucadu/switch-to-prev-buffer ()
  "Switch to the last-used buffer.
Unlike 'switch-to-prev-buffer', performing this function twice gets you back to the same buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(use-package general
  :config
  (general-evil-setup t)
  ;; Leaders
  (general-create-definer bind-in-top-level
    :prefix *top-level-leader*)
  (general-create-definer bind-in-major-mode
    :prefix *major-mode-leader*)
  (bind-in-top-level
    ;; Prefixes
    "!"   '(nil :which-key "fly{check,spell} prefix")
    "! t" '(nil :which-key "toggle")
    "b"   '(nil :which-key "buffers prefix")
    "c"   '(nil :which-key "comments prefix")
    "f"   '(nil :which-key "files prefix")
    "g"   '(nil :which-key "git prefix")
    "m"   '(nil :which-key "major mode prefix")
    "p"   '(nil :which-key "projectile prefix")
    "s"   '(nil :which-key "search prefix")
    "w"   '(nil :which-key "whitespace prefix")
    ;; Keys
    "b x" 'barrucadu/switch-to-prev-buffer
    "c d" 'comment-dwim
    "c l" 'comment-line
    "c r" 'comment-region
    "s f" 'isearch-forward-regexp
    "s b" 'isearch-backward-regexp
    "w d" 'delete-horizontal-space
    "G"   'goto-line))


;;;; Main configuration

;;; org-mode
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-src-tab-acts-natively t
      org-src-fontify-natively  t)

;;; flycheck / flyspell
(use-package flycheck
  :init (global-flycheck-mode)
  :general
  (bind-in-top-level
    "! t c" 'flycheck-mode))

(use-package flyspell
  :diminish flyspell-mode
  :general
  (bind-in-top-level
   "! t s" 'flyspell-mode
   "! w" 'flyspell-correct-word-before-point)
  :hook
  ((text-mode  . flyspell-mode)
   (prog-mode  . flyspell-prog-mode)
   (org-mode   . flyspell-mode)
   (LaTeX-mode . flyspell-mode))
  :config
  (setq flyspell-use-meta-tab nil
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  (define-key flyspell-mode-map (kbd "C-c $") nil)
  (define-key flyspell-mode-map (kbd "M-t") nil)
  (define-key flyspell-mouse-map [down-mouse-2] nil)
  (define-key flyspell-mouse-map [mouse-2] nil))

(use-package ispell
  :defer t
  :config
  (setq ispell-program-name   (executable-find "aspell")
        ispell-dictionary     "en_GB"
        ispell-silently-savep t))

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode latex-mode rst-mode))

(add-to-list 'flycheck-checkers 'proselint t)
(flycheck-add-next-checker 'tex-chktex  'proselint t)
(flycheck-add-next-checker 'tex-lacheck 'proselint t)

(use-package flycheck-vale
  :after flycheck
  :hook (flycheck-mode . flycheck-vale-setup)
  :config
  (flycheck-add-next-checker 'proselint 'vale t)
  (flycheck-add-mode 'vale 'latex-mode))

;;; Accounting
(use-package flycheck-ledger)
(use-package ledger-mode
  :mode "\\.ledger\\'\\|\\.journal\\'"
  :config
  (setq ledger-binary-path                 (executable-find "hledger")
        ledger-mode-should-check-version    nil
        ledger-init-file-name               " "
        ledger-post-amount-alignment-column 80
        ledger-highlight-xact-under-point   nil))

;;; Programming

;; Elixir
(use-package elixir-mode
  :mode "\\.ex\\'\\|\\.exs\\'")

;; Forth
(use-package forth-mode
  :mode "\\.fs\\'")

;; Haskell
(use-package haskell-mode
  :mode "\\.hs\\'"
  :general
  (bind-in-major-mode
   :major-modes t
   :keymaps 'haskell-mode-map
   "i"   '(nil :which-key "imports")
   "i j" 'haskell-navigate-imports
   "i s" 'haskell-sort-imports
   "i a" 'haskell-align-imports))

(use-package haskell-cabal-mode
  :mode "\\.cabal\\'"
  :ensure haskell-mode)

;; JSON
(use-package json-mode
  :mode "\\.json\\'"
  :hook
  (json-mode . (lambda () (setq-local js-indent-level 4))))

;; Python
(use-package python
  :mode ("``.py``'" . python-mode))

;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :commands (godoc gofmt gofmt-before-save)
  :general
  (bind-in-major-mode
   :major-modes t
   :keymaps 'go-mode-map
   "f" 'gofmt
   "i" 'go-goto-imports
   "r" 'go-remove-unused-imports)
  :hook
  (before-save . (lambda ()
                   (when (eq major-mode 'go-mode)
                     (gofmt-before-save)))))

;; Groovy
(use-package groovy-mode
  :mode "\\.groovy\\'")

;; Lua
(use-package lua-mode
  :mode "\\.lua\\'")

;; Nix
(use-package nix-mode
  :mode "\\.nix\\'")

;; Puppet
(use-package puppet-mode
  :mode "\\.pp\\'")

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

;; Ruby
(setq ruby-insert-encoding-magic-comment nil)

;; Scala
(use-package scala-mode
  :mode "\\.scala\\'")

;; Shell
(use-package sh-script
  :mode ("\\.zsh\\'" . sh-mode)
  :config
  (setq sh-basic-offset 2))

;; Terraform
(use-package terraform-mode
  :mode ("\\.tf``'" . terraform-mode))

;; TOML
(use-package toml-mode
  :mode ("\\.toml``'" . toml-mode))

;; Typescript
(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'"))

;; YAML
(use-package yaml-mode
  :mode "\\.yaml\\'"
  :hook (yaml-mode . (lambda () (run-hooks 'prog-mode-hook))))

;;; Writing
(use-package markdown-mode
  :mode "\\.md\\'\\|\\.markdown\\'")

;; LaTeX
(setq TeX-parse-self t
      TeX-electric-sub-and-superscript t
      TeX-master 'dwim
      bibtex-entry-format `(opts-or-alts numerical-fields page-dashes last-comma delimiters unify-case sort-fields)
      bibtex-entry-delimiters 'braces
      bibtex-field-delimiters 'double-quotes
      bibtex-comma-after-last-field nil)

(defvar barrucadu/bibtex-fields-ignore-list
  '("abstract" "acmid" "address" "annotation" "articleno" "eprint"
    "file" "isbn" "issn" "issue_date" "keywords" "language" "location"
    "month" "numpages" "url"))

(defun barrucadu/bibtex-clean-entry-drop-fields ()
  (save-excursion
    (let (bounds)
      (when (looking-at bibtex-entry-maybe-empty-head)
        (goto-char (match-end 0))
        (while (setq bounds (bibtex-parse-field))
          (goto-char (bibtex-start-of-field bounds))
          (if (member (bibtex-name-in-field bounds)
                      barrucadu/bibtex-fields-ignore-list)
              (kill-region (caar bounds) (nth 3 bounds))
            (goto-char (bibtex-end-of-field bounds))))))))

(defun barrucadu/bibtex-clean-entry-newline ()
  (save-excursion
    (progn (bibtex-end-of-entry) (left-char) (newline))))

(add-hook 'bibtex-clean-entry-hook 'barrucadu/bibtex-clean-entry-newline)
(add-hook 'bibtex-clean-entry-hook 'barrucadu/bibtex-clean-entry-drop-fields)

;;; Version control
(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (unless (display-graphic-p) (diff-hl-margin-mode))
  :hook
  ((dired-mode . diff-hl-dired-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package magit
  :general
  (bind-in-top-level
   "g c" 'magit-clone
   "g s" 'magit-status
   "g b" 'magit-blame
   "g l" 'magit-log-buffer-line
   "g p" 'magit-pull)
  :init
  (setq magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all
        magit-revision-show-gravatars nil
        magit-repository-directories `(("~" . 2))
        magit-repolist-columns
        '(("Name"    25 magit-repolist-column-ident                  ())
          ("Version" 25 magit-repolist-column-version                ())
          ("Dirty"    1 magit-repolist-column-dirty                  ())
          ("Unpulled" 3 magit-repolist-column-unpulled-from-upstream ((:right-align t)))
          ("Unpushed" 3 magit-repolist-column-unpushed-to-upstream   ((:right-align t)))
          ("Path"    99 magit-repolist-column-path                   ())))
  :config
  (define-key magit-file-mode-map (kbd "C-c M-g") nil)
  (setq magit-bury-buffer-function
        (lambda (con)
          (kill-buffer-and-window))))

(use-package evil-magit
  :after magit)

(use-package git-timemachine
  :general
  (bind-in-top-level
   "g t" 'git-timemachine)
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))


;;;; Miscellaneous

;;; surround.vim
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;; Increment / decrement numbers
(use-package evil-numbers
  :general
  (general-define-key
    :keymaps 'evil-ex-map
    "+" 'evil-numbers/inc-at-pt
    "-" 'evil-numbers/dec-at-pt))

;;; Whitespace
(use-package whitespace-cleanup-mode
  :diminish (whitespace-cleanup-mode . " [W]")
  :general
  (bind-in-top-level
   "w b" 'whitespace-cleanup
   "w r" 'whitespace-cleanup-region)
  :hook
  ((prog-mode . whitespace-cleanup-mode)
   (text-mode . whitespace-cleanup-mode)
   (conf-mode . whitespace-cleanup-mode)))

(use-package whitespace
  :diminish whitespace-mode
  :general
  (bind-in-top-level
   "w m" 'whitespace-mode)
  :config (setq whitespace-line-column nil))

;;; Searching and replacing
(use-package visual-regexp
  :general
  (bind-in-top-level
   "s r" 'vr/query-replace
   "s R" 'vr/replace))

;;; Helm
(use-package helm
  :bind ("M-x" . helm-M-x)
  :init (helm-mode 1)
  :diminish helm-mode)

(use-package helm-buffers
  :ensure helm
  :general
  (general-define-key
   [remap switch-to-buffer] 'helm-mini)
  (bind-in-top-level
    "b b" 'helm-mini)
  (general-define-key
    :keymaps 'evil-ex-map
    "b" 'helm-mini)
  :config (setq helm-buffers-fuzzy-matching t))

(use-package helm-files
  :ensure helm
  :general
  (general-define-key
   [remap find-file] 'helm-find-files)
  (bind-in-top-level
   "f f" 'helm-for-files
   "f r" 'helm-recentf)
  (general-define-key
    :keymaps 'evil-ex-map
    "f f" 'helm-find-files)
  :config
  (setq helm-recentf-fuzzy-match t
        helm-ff-file-name-history-use-recentf t
        helm-ff-search-library-in-sexp t))

;;; Projectile
(use-package projectile
  :init (projectile-mode)
  :config
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :after projectile
  :general
  (bind-in-top-level
   "p f" 'helm-projectile
   "p d" 'helm-projectile-find-dir
   "p o" 'helm-projectile-find-other-file
   "p s" 'helm-projectile-grep)
  (general-define-key
    :keymaps 'evil-ex-map
    "f p" 'helm-projectile)
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
