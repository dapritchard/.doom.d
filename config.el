;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "David Pritchard"
      user-mail-address "david.al.pritchard@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(general-def
  "C-9" 'previous-buffer
  "C-0" 'next-buffer
  "M-[" 'scroll-down-line
  "M-]" 'scroll-up-line)

(general-def 'normal
  "9" 'evil-digit-argument-or-evil-beginning-of-line
  "0" 'evil-end-of-line)

(use-package! evil
  :custom
  (evil-disable-insert-state-bindings t))

(use-package! avy
  :custom
  (avy-all-windows t)
  :general
  ("M-g M-g" 'avy-goto-line))

(use-package! ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  :general
  ("M-o" 'ace-window))

;; adding the symbol `tramp-own-remote-path' adds the PATHs used by the remote
;; user to the search path. See
;; https://www.gnu.org/software/tramp/#Remote-programs for details.
(use-package! tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package! undo-tree
  :config
  (global-undo-tree-mode 1))


;; vim has a concept of a jump-list, which is managed by the 'better-jumper'
;; package, which are seemingly triggered by motions and can be jumped around
;; using "C-o" and "C-i". You can manually add a position to the jump list by
;; using (the non-interactive function) `evil-set-jump'.

;; manage jumps "per-buffer", rather than "per-window."
(setq better-jumper-context 'buffer)

;; by default bound to `company/complete'
(general-def 'insert "C-SPC")

;; by default bound to `evil-scroll-up'
(general-def 'motion "C-u")

;; So that we can press e.g. C-u C-SPC C-SPC C-SPC to pop the mark three times.
(setq set-mark-command-repeat-pop t)


(use-package! smartparens
  :config
  (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'comint-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'text-mode-hook #'turn-on-smartparens-mode))

(use-package evil-smartparens
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

;; TODO move this into a smartparens block.  See https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
(general-def
  "s-f" 'sp-down-sexp
  "s-F" 'sp-backward-down-sexp
  "s-a" 'sp-beginning-of-sexp
  "s-e" 'sp-end-of-sexp
  "s-v" 'sp-up-sexp
  "s-V" 'sp-backward-up-sexp
  "C-M-t" 'sp-transpose-sexp
  "s-W" 'sp-kill-sexp
  "s-w" 'sp-copy-sexp
  "M-d" 'sp-unwrap-sexp
  "M-D" 'sp-backward-unwrap-sexp
  "s-g" 'sp-forward-slurp-sexp
  "s-G" 'sp-backward-slurp-sexp
  "s-b" 'sp-forward-barf-sexp
  "s-B" 'sp-backward-barf-sexp
  "s-q" 'sp-forward-whitespace
  "s-s" 'sp-split-sexp
  "s-S" 'sp-join-sexp
  "s-r" 'sp-rewrap-sexp)
