;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Michael Walker"
      user-mail-address "mike@barrucadu.co.uk")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-acario-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; Replace insert-state with emacs-state
(advice-add 'evil-insert-state :override #'evil-emacs-state)

;; Don't try to get smart with me.
(advice-add 'doom/backward-to-bol-or-indent :override #'beginning-of-line)
(advice-add 'doom/forward-to-last-non-comment-or-eol :override #'end-of-line)

;; C-k at the start of a line also removes the newline
(setq kill-whole-line t)

;; Projectile
(setq projectile-project-search-path '("~/projects/" "~/work/"))

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

;; Start scrolling smoothly when the point is more than 7 lines away
;; from the centre
(setq scroll-margin 7
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Years of muscle-memory can't be overturned now!
(setq sentence-end-double-space t)

;; MacOS UK keyboard
(when (eq system-type 'darwin)
  (define-key key-translation-map (kbd "M-2") (kbd "€"))
  (define-key key-translation-map (kbd "M-3") (kbd "#")))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; hledger
(use-package! ledger-mode
  :mode "\\.ledger\\'\\|\\.journal\\'"
  :config
  (setq ledger-binary-path (executable-find "hledger")
        ledger-mode-should-check-version nil
        ledger-init-file-name " "
        ledger-post-amount-alignment-column 80
        ledger-highlight-xact-under-point nil)
  (add-hook 'ledger-mode-hook
            (lambda ()
              (setq-local tab-always-indent 'complete
                          completion-cycle-threshold t
                          ledger-complete-in-steps t))))

;; Search & replace
(use-package! visual-regexp-steroids
  :config
  (require 'pcre2el)
  (setq vr/engine 'pcre2el)
  (map! :leader
        :desc "Regular expression query-replace"
        "s r" #'vr/query-replace)
  (map! :leader
        :desc "Regular expression replace"
        "s R" #'vr/replace))

;; Left / right to navigate whole folders
(after! helm
  (customize-set-variable 'helm-ff-lynx-style-map t))

;; jsonnet
(use-package! jsonnet-mode :defer t)
