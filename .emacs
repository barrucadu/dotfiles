;; -*- lisp -*-

; Interface
(menu-bar-mode      0)
(column-number-mode t)
(display-time-mode  t)
(global-hl-line-mode 1)
(set-face-background 'hl-line "#111111")

; UTF-8
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system       'utf-8)

; Extra Files
(add-to-list 'load-path "~/code/elisp/")

(load "phrases.el"  nil t t)
(load "latex.el"    nil t t)
(load "phpxhtml.el" nil t t)

(require 'color-theme)
(require 'setnu+)
(require 'w3m-load)

(autoload 'php-mode       "php-mode.el"      "PHP Mode."          t)
(autoload 'pkgbuild-mode  "pkgbuild-mode.el" "PKGBUILD Mode."     t)
(autoload 'css-mode       "css-mode.el"      "CSS Mode"           t)
(autoload 'python-mode    "python-mode.el"   "Python Mode."       t)
(autoload 'hide-mode-line "hide-mode-line"                    nil t)

; File Associations
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)
                                ("/PKGBUILD$"      . pkgbuild-mode)
                                ("/*..css$"        . css-mode)
                                ("/*..rdf$"        . xml-mode)
                                ("/*..xsl$"        . xml-mode)
                                ("/*..xrdf$"       . xml-mode)
                                ("/*..rdfs$"       . xml-mode)
                                ("/*..py$"         . python-mode)
                                ("/*..tex$"        . latex-mode)
                                ("/*..sh$"         . shell-script-mode)
                                ("/*..red$"        . asm-mode)
                                ("/*..todo$"       . org-mode))
                              auto-mode-alist))

; Key Bindings
(global-set-key [?\C-n]       'setnu-mode)
(global-set-key [?\C-x ?n]    'setnu-mode)
(global-set-key [?\C-x ?t]    'toggle-truncate-lines)
(global-set-key [?\C-x ?w]    'delete-horizontal-space)
(global-set-key [?\C-x ?W]    'delete-trailing-whitespace)
(global-set-key [?\C-x ?@]    'w3m)
(global-set-key [?\C-x ?g]    'goto-line)
(global-set-key [?\C-x ?i]    'indent-according-to-mode)
(global-set-key [?\C-x ?\C-u] 'undo)
(global-set-key [?\C-x ?m]    'hide-mode-line)
(global-set-key [?\C-x ?\C-m] 'hide-mode-line)
(global-set-key [?\M-O ?a]    'backward-paragraph)
(global-set-key [?\M-O ?b]    'forward-paragraph)
(global-set-key [?\M-O ?c]    'forward-word)
(global-set-key [?\M-O ?d]    'backward-word)
(global-set-key [?\M-[ ?3 ?^] 'backward-kill-word)

; Functions
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

; Hooks
(add-hook 'php-mode-hook          (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'pkgbuild-mode-hook     (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'css-mode-hook          (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'xml-mode-hook          (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'python-mode-hook       (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'latex-mode-hook        (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'shell-script-mode-hook (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'conf-mode-hook         (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'lisp-mode-hook         (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'scheme-mode-hook       (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'text-mode-hook         (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'c-mode-hook            (lambda () (setnu-mode) (indent-according-to-mode) (hide-mode-line)))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

; Miscellaneous
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default ispell-program-name "aspell")
(setq-default indent-tabs-mode     nil)
(setq default-tab-width            4)
(setq make-backup-files            nil)
(setq kill-whole-line              t)
(setq inhibit-splash-screen        t)
(setq org-return-follows-link      t)
(setq org-log-done                 'time)
