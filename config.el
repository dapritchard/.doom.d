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
(setq doom-theme 'doom-spacegrey)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

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
(use-package exec-path-from-shell
  :config
  (when (display-graphic-p)
    (exec-path-from-shell-initialize)))


;; general keybindings --------------------------------------------------------

(general-def
  "C-9" #'previous-buffer
  "C-0" #'next-buffer
  "M-[" #'scroll-down-line
  "M-]" #'scroll-up-line)

(general-def '(normal motion)
  "9" #'evil-digit-argument-or-evil-beginning-of-line
  "0" #'evil-end-of-line)

;; by default "C-j" is bound to an alias for newline in insert mode, and is
;; bound `electric-newline-and-maybe-indent' in the global map, but I like to
;; use this keybinding in some minor modes and fall back to the global
;; definition otherwise
(general-def 'insert
  "C-j" nil)


;; truncate lines management --------------------------------------------------

;; the default behavior for when lines are longer than the window size is line
;; wrapping, but set programming modes to truncate lines instead. Note that we
;; could use `(set-default truncate-lines t)' to set this globally, but I prefer
;; line wrap for comint modes.
(defun use-truncate-lines () (setq-local truncate-lines t))
(add-hook 'prog-mode-hook 'use-truncate-lines)
(add-hook 'dired-mode 'use-truncate-lines)
(add-hook 'yaml-mode 'use-truncate-lines)
(add-hook 'ivy-occur-grep-mode 'use-truncate-lines)



;; window movement ------------------------------------------------------------

(general-def
  "s-h" #'evil-window-left
  "s-j" #'evil-window-down
  "s-k" #'evil-window-up
  "s-l" #'evil-window-right
)


;; dired ----------------------------------------------------------------------

;; ;; `dired-find-file-other-window' is bound to "g-O", but I can never remember it
;; (general-def 'normal dired-mode-map
;;   "o" #'dired-find-file-other-window)


;; info -----------------------------------------------------------------------

;; `Info-scroll-down' is DEL in normal state, but `Info-state-up' doesn't have a
;; keybinding, so let's make it next to DEL, noting that "=" is unused.
;; Additionally, I swap the keybindings for the two, since I'm most often
;; reaching for `Info-scroll-down'.
(general-def 'normal 'Info-mode-map
  "=" #'Info-scroll-down
  "DEL" #'Info-scroll-up)



;; Ibuffer --------------------------------------------------------------------

(require 'ibuffer)
(require 'ibuf-ext)

;; inform Ibuffer to use the saved filter groups
(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; specify groupings for Ibuffer entries.  See https://www.emacswiki.org/emacs/IbufferMode for
;; more details.
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("R" (mode . ess-r-mode))
               ("Python" (mode . python-mode))
               ("C/C++" (or (mode . c-mode)
                            (mode . c++-mode)))
               ("LaTeX" (or (mode . latex-mode)
                            (mode . bibtex-mode)))
               ("shell" (mode . sh-mode))
               ("Lisp" (or (mode . lisp-mode)
                           (mode . scheme-mode)))
               ("emacs" (or (mode . lisp-interaction-mode)
                            (mode . emacs-lisp-mode)))
               ("dired" (mode . dired-mode))
               ("processes" (or (mode . inferior-ess-r-mode)
                                (mode . inferior-ess-mode)
                                (mode . inferior-python-mode)
                                (mode . term-mode)
                                (mode . shell-mode)
                                (mode . slime-repl-mode)
                                (mode . geiser-repl-mode)))
               ("Org" (mode . org-mode))
               ("documentation" (or (mode . Info-mode)
                                    (mode . helpful-mode)
                                    (mode . Man-mode)
                                    (mode . ess-r-help-mode)))))))

;; change the width of the first column.  See
;; https://emacs.stackexchange.com/a/623/15552
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 40 40 :left :elide) ; change: the two 40 values were originally 18's
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

;; ignore buffers created by polymode
(add-to-list 'ibuffer-never-show-predicates "\\*help\\[R\\]\\(.*\\)\\[head-tail\\]")
(add-to-list 'ibuffer-never-show-predicates "\\*help\\[R\\]\\(.*\\)\\*\\[R\\]")


(use-package! evil
  :custom
  (evil-disable-insert-state-bindings t))

(use-package! avy
  :custom
  (avy-all-windows t)
  :general
  ("M-g M-g" 'avy-goto-line))

;; HACK: iedit creates an annoying buffer at load time if the C-; keybinding is
;; already taken, so load it first
(require 'iedit)
(general-def
  "C-;" #'avy-goto-char-2)

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
  (global-undo-tree-mode))

;; ;; highlight both files and directories in treemacs based on their git status
;; (setq +treemacs-git-mode 'deferred)
;; (setq doom-themes-treemacs-theme "doom-atom")
;; (setq doom-themes-treemacs-theme "doom-colors")
(after! treemacs
  (setq doom-themes-treemacs-theme "doom-colors"))

(general-def
  "M-{" #'er/expand-region
  "M-}" #'er/contract-region
  "M-O" #'er/mark-defun
  "M-P" #'er/mark-paragraph)

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


;; use Ibuffer for Buffer List.  This is also 'SPC b i'.
(global-set-key (kbd "C-x C-b") 'ibuffer)



(use-package! smartparens
  :config
  (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'comint-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'text-mode-hook #'turn-on-smartparens-mode))

(use-package evil-smartparens
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

;; the "Smartparens config" section of
;; ~/.emacs.d/modules/config/default/config.el has a section that calls
;; `sp-pair' for the (), [], and {} pairs with an argument to `:unless' that has
;; the effect of preventing the closing part of a pair from being inserted
;; before a word or another one of the same opening pair. The following commands
;; undoes those changes.
(sp-pair "(" nil :unless nil)
(sp-pair "[" nil :unless nil)
(sp-pair "{" nil :unless nil)

;; TODO move this into a smartparens block.  See https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
(general-def
  "s-d" 'sp-down-sexp
  "s-D" 'sp-backward-down-sexp
  "s-a" 'sp-beginning-of-sexp
  "s-e" 'sp-end-of-sexp
  "s-f" 'sp-up-sexp
  "s-F" 'sp-backward-up-sexp
  "C-M-t" 'sp-transpose-sexp
  "s-W" 'sp-kill-sexp
  "s-w" 'sp-copy-sexp
  "s-x" 'sp-unwrap-sexp
  "s-X" 'sp-backward-unwrap-sexp
  "s-g" 'sp-forward-slurp-sexp
  "s-G" 'sp-backward-slurp-sexp
  "s-v" 'sp-forward-barf-sexp
  "s-V" 'sp-backward-barf-sexp
  "s-q" 'sp-forward-whitespace
  "s-s" 'sp-split-sexp
  "s-S" 'sp-join-sexp
  "s-r" 'sp-rewrap-sexp)


;; ESS -------------------------------------------------------------------------

(after! ess

  (setq ess-style 'RStudio                               ; set the indentation style to mimic RStudio's
        ess-indent-with-fancy-comments nil               ; always indent comments to current code depth
        ess-plain-first-buffername nil                   ; name the first R process R:1 for consistency
        ess-auto-width 'window                           ; synchronize R's "width" option to the window width
        ess-roxy-str "#'"                                ; so Roxygen comments are #' not ##'
        inferior-R-args "--no-restore-data --no-save")   ; command line parameters when starting R

  ;; prevent adding an additional hash to comments (i.e. so that comments are # not ##)
  (add-hook 'ess-mode-hook (lambda () (setq-local comment-add 0)))

  (general-def 'ess-mode-map
    ";" #'ess-insert-assign
    "C-j" #'ess-eval-region-or-line-visibly-and-step)

  (general-def 'ess-r-mode-map
    "C-S-m" (lambda () (interactive) (insert " %>% "))
    "C-c C-h" #'dp-ess-eval-word)

  (general-def 'inferior-ess-mode-map
    ";" 'ess-insert-assign
    "C-S-m" (lambda () (interactive) (insert " %>% ")))

  ;; (load! "lisp/ess+/ess-history.el")
  )


;; nicer `open-line' ----------------------------------------------------------

(defun open-line-and-indent ()
  "Like `open-line', but with proper indentation."
  (interactive)
  (save-excursion
    (newline-and-indent)))

;; ;; replace `open-line'
;; (global-set-key [remap open-line] #'open-line-and-indent)


;; uncollected functions ------------------------------------------------------

(defun whitespace-cleanup-force ()
  "Like `whitespace-cleanup', but always cleans everything.
The `whitespace-cleanup' function only cleans the types of
whitespace that are included in `whitespace-style'. However, this
variable also controls what you see in `whitespace-mode', and
sometimes you don't want to visualize every type of whitespace,
but when you call `whitespace-cleanup-force' you still want to
zap it."
  (interactive)
  (let* ((whitespace-style '(tabs spaces trailing lines space-before-tab
                                  newline indentation empty space-after-tab
                                  space-mark tab-mark newline-mark)))
    (whitespace-cleanup)))
