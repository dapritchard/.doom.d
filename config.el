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

;; swap the location of the meta and super keys
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; ;; In ~/.emacs.d/docs/faq.org it says that doom env already provides this
;; ;; functionality. See the "Doom can't find my executables/doesn't inherit the
;; ;; correct PATH" section.
;; (use-package exec-path-from-shell
;;   :config
;;   (when (display-graphic-p)
;;     (exec-path-from-shell-initialize)))

;; from ~/.emacs.d/docs/faq.org
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; general keybindings --------------------------------------------------------

(general-def
  "C-9" #'previous-buffer
  "C-0" #'next-buffer
  "M-[" #'scroll-down-line
  "M-]" #'scroll-up-line)

(general-def '(normal motion)
  "9" #'evil-digit-argument-or-evil-beginning-of-line
  "0" #'evil-end-of-line)

;; ;; TAB is bound to `better-jumper-jump-forward', which is also bound to "C-i",
;; ;; so let's make it perform indentation since that is what I'm used to from
;; ;; regular Emacs
;; (general-def '(normal visual motion)
;;   "TAB" #'evil-indent)

;; by default "C-j" is bound to an alias for newline in insert mode, and is
;; bound `electric-newline-and-maybe-indent' in the global map, but I like to
;; use this keybinding in some minor modes and fall back to the global
;; definition otherwise
(general-def 'insert
  "C-j" nil)

;; TODO: keybinding for refreshing ivy-occur
;; https://www.reddit.com/r/emacs/comments/99n7er/question_refresh_occur_buffer/


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

(general-def 'doom-leader-toggle-map
  "t" #'toggle-truncate-lines)

;; window movement ------------------------------------------------------------

(general-def
  "s-h" #'evil-window-left
  "s-j" #'evil-window-down
  "s-k" #'evil-window-up
  "s-l" #'evil-window-right)


;; dired ----------------------------------------------------------------------

;; ;; `dired-find-file-other-window' is bound to "g-O", but I can never remember it
;; (general-def 'normal dired-mode-map
;;   "o" #'dired-find-file-other-window)


;; Info -----------------------------------------------------------------------

;; FIXME: the keybindings below get overwritten somehow, I have to exectute them
;; by re-exectute the command each time I restart Emacs.

;; `Info-scroll-down' is DEL in normal state, but `Info-state-up' doesn't have a
;; keybinding, so let's make it next to DEL, noting that "=" is unused.
;; Additionally, I swap the keybindings for the two, since I'm most often
;; reaching for `Info-scroll-down'.
(general-def 'normal 'Info-mode-map
  "=" #'Info-scroll-down
  "DEL" #'Info-scroll-up)


;; Ibuffer --------------------------------------------------------------------

;; FIXME: we shouldn't have to load these packages in the config
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


;; abbrev ----------------------------------------------------------------------

(use-package! abbrev
  :defer 1
  :custom
  (abbrev-mode 1)
  :config
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)))


;; org -------------------------------------------------------------------------

(defun org-todo-todo ()
  (org-todo "TODO"))

(use-package! org

  :custom

  ;; Allows you to create a new node when refiling
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; Used by some of the Capture templates to construct the location of to-do
  ;; notes
  (+org-capture-todo-file "inbox.org")

  ;; Setting this to a non-nil value enables logging of state changes into a
  ;; per-entry drawer. A value of `t' corresponds to the LOGBOOK drawer.
  (org-log-into-drawer t)

  :config

  ;; add keybindings
  (map! :after org-agenda
        :map org-agenda-mode-map
        :localleader
        "l" #'org-agenda-log-mode)

  ;; update the list of included Org modules
  (add-to-list 'org-modules 'org-checklist)

  ;; Global to-do keywords. Based on Doom's default settings, but with added
  ;; logging (i.e. ! and @). NOTE: for some reason this doesn't work if I place
  ;; it in the `:custom' section.
  (setq
   org-todo-keywords
   '((sequence "TODO(t!)" "PROJ(p)" "STRT(s!)" "WAIT(w@)" "HOLD(h@)" "|" "DONE(d!)" "KILL(k@)")
     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))

  ;; Shadow the existing Capture "todo" entry. FIXME: both "t"s still show up in
  ;; the Capture templates.
  (add-to-list 'org-capture-templates
               '("t" "Personal todo" entry
                 (file+headline +org-capture-todo-file "Inbox")
                 "* %?\n\n%a" :prepend t)))

;; https://fuco1.github.io/2019-02-02-Org-mode-and-google-calendar-sync.html
(use-package org-gcal
  :after org
  :config
  (setq org-gcal-client-id "647603884391-2ljnnlbqocg6ub4glfod475og1atndhr.apps.googleusercontent.com"
        org-gcal-client-secret "7pTkJSrjULYw0Fx16VvPknnj"
        org-gcal-file-alist '(("dpritchard@novisci.com" . "~/Documents/org/gcal-work.org"))
        org-gcal-header-alist '(("dpritchard@novisci.com" . "#+PROPERTY: TIMELINE_FACE \"pink\"\n"))
        org-gcal-auto-archive nil
        org-gcal-notify-p nil)
  (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
  (add-hook 'org-capture-after-finalize-hook 'org-gcal-fetch)
  ;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


(use-package org-mru-clock
  :custom
  (org-mru-clock-how-many 30 "Increase the number of clock entries to look up")
  (org-mru-clock-completing-read #'ivy-completing-read "Use Ivy for completion")
  :general
  (:keymaps 'doom-leader-notes-map
   :wk-full-keys nil
   "h" '(:prefix-command org-mru-clock-map :which-key "org-mru-clock"))
  ('org-mru-clock-map
   :wk-full-keys nil
   "a" 'org-mru-clock-add-note
   "h" 'org-mru-clock-to-history
   "i" 'org-mru-clock-in
   "n" 'org-mru-clock-show-narrowed
   "g" 'org-mru-clock-goto
   "s" 'org-mru-clock-select-recent-task)
  :init
  (add-hook minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))


;; projectile ------------------------------------------------------------------

;; add projectile keybinds to "i" and "d" to mirror the "buffer" versions of
;; these, and move the functions currently bound to those keys to "I" and "D".
;; These changes overwrite the original binding of "D", which by default is
;; bound to `+default/discover-projects' (there is no original binding to "I").
(use-package! projectile
  :general
  (:keymaps 'doom-leader-project-map
   :wk-full-keys nil
   "i" '(projectile-ibuffer :which-key "Ibuffer in project")
   "d" '(projectile-find-dir :which-key "Dired open in project")
   "I" '(projectile-invalidate-cache :which-key "Invalidate project cache")
   "D" '(projectile-remove-known-project :which-key "Remove known project")))

(use-package! evil
  :custom
  (evil-disable-insert-state-bindings t))

(use-package! avy
  :custom
  (avy-all-windows t)
  :general
  (:keymaps 'evilem-map
   "g" #'avy-goto-line))

;; set the ace-window keys to the home row, and give `ace-window' the keybinding
;; that was originally assigned to `doom/window-enlargen' (and find an unused
;; binding for that function)
(use-package! ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-background nil
        aw-dispatch-always t)
  :general
  (:keymaps 'evil-window-map
   "o" #'ace-window
   "e" #'doom/window-enlargen))

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


(use-package! smartparens
  :config
  (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'comint-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'text-mode-hook #'turn-on-smartparens-mode))

(use-package evil-smartparens
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  ;; `evil-sp-override' overwrites `exchange-point-and-mark's keybinding of "o",
  ;; so let's restore that and give it a new keybinding
  :general
  ('visual
   "o" #'exchange-point-and-mark
   "C-M-o" #'evil-sp-override))

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
        inferior-R-args "--no-restore-data --no-save"    ; command line parameters when starting R
        ess-directory-function
        (lambda ()
          (plist-get (ess-r-package-info) :root)))       ; suggest the package root (when applicable) when launching a new R process

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
  (load! "lisp/ess+/ess-utils.el")
  )


;; Dhall -----------------------------------------------------------------------

(use-package! dhall-mode
  :ensure t
  :mode "\\.dhall\\'")


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
  (let* ((whitespace-style
          '(tabs
            spaces
            trailing
            lines
            space-before-tab
            newline
            indentation
            empty
            space-after-tab
            space-mark
            tab-mark
            newline-mark)))
    (whitespace-cleanup)))

(load! "lisp/utils/title-case.el")
(general-def "M-T" #'dp-title-case-region-or-line)

(load! "lisp/utils/align-regexp-variants.el")
(general-def '(normal visual)
  "+" #'align-regexp-=)
