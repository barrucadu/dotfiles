;; -*- lisp -*-

;;;;; Package Management ;;;;;
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;;;;; Configuration ;;;;;
(defun load-directory (base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (cond ((file-regular-p name) (load-file name))
            ((and (file-directory-p name)
                  (not (equal f ".."))
                  (not (equal f "."))) (load-directory name))))))

(load-directory "~/.emacs.d/init")

;;;;; Appearence ;;;;;
; Colour theme
(load-theme 'manoj-dark t)

; Hide menu bar
(menu-bar-mode 0)

; Mode line
(column-number-mode t)
(display-time-mode t)

(setq default-mode-line-format '("-"
				 mode-line-mule-info
				 mode-line-modified
				 mode-line-frame-identification
				 mode-line-buffer-identification
				 "  "
				 global-mode-string
				 "   %[(" mode-name mode-line-process minor-mode-alist "%n"")%]--"
				 (line-number-mode "L%l--")
				 (column-number-mode "C%c--")
				 (-3 . "%p")
				 "-%-"))

;;;;; Behaviour ;;;;;
; Initial mode
(setq initial-major-mode 'org-mode)

; Transparent encryption
(require 'epa-file)
(epa-file-enable)

; Tabs instead of spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; Spell checking
(setq ispell-program-name "aspell")

; Backup files
(setq make-backup-files nil)

; Line-killing
(setq kill-whole-line t)

; Splash screen
(setq inhibit-splash-screen t)

; Highlight matching parentheses and other such characters
(show-paren-mode 1)
(setq show-paren-delay 0)

; Change yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

; Completion
(setq default-abbrev-mode t)
(global-set-key "\M- " 'hippie-expand)

; Disable overzealous indenting
(electric-indent-mode 0)

;;;;; Keybindings ;;;;;
; Helper functions
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

; Bindings
(global-set-key (kbd "C-x w")   'delete-horizontal-space)
(global-set-key (kbd "C-x g")   'goto-line)
(global-set-key (kbd "C-x C-u") 'undo)
(global-set-key (kbd "C-x C-x") 'switch-to-previous-buffer)