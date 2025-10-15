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
(setq org-directory "~/Dev/org/")

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


;; Begin personal customizations -----------------------------------------------

;; Doom freezing after `doom upgrade' on 2024-07-30.
;; https://github.com/doomemacs/doomemacs/issues/7628#issuecomment-1917808642
;; https://www.reddit.com/r/emacs/comments/197zbtu/how_to_prevent_emacs_freezing_on_macos_seeking/
(fset 'epg-wait-for-status 'ignore)
;; https://discord.com/channels/406534637242810369/1273640403974750379/1273640403974750379
(setq diff-hl-update-async nil)

;; swap the location of the meta and super keys
(setq mac-command-modifier 'meta
      mac-option-modifier 'super
      mac-right-option-modifier 'super)

;; ;; In ~/.emacs.d/docs/faq.org it says that doom env already provides this
;; ;; functionality. See the "Doom can't find my executables/doesn't inherit the
;; ;; correct PATH" section.
;; (use-package exec-path-from-shell
;;   :config
;;   (when (display-graphic-p)
;;     (exec-path-from-shell-initialize)))

;; ;; from ~/.emacs.d/docs/faq.org
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; ;; https://github.com/hlissner/doom-emacs/issues/3862
;; ;; https://www.reddit.com/r/emacs/comments/i4uh6w/emacs_hanging_on_osx/
;; (after! which-key
;;   (setq which-key-allow-imprecise-window-fit nil))

;; This advice inserts a comment when hitting RET or "o", etc., which is the
;; default vim behavior. However, on average I find that behavior to be more
;; annoying than helpful so I turn it off
(advice-remove 'newline-and-indent
               #'+default--newline-indent-and-continue-comments-a)
(advice-remove 'evil-open-below
               #'+evil--insert-newline-below-and-respect-comments-a)


;; load config files -----------------------------------------------------------

(load! "lisp/utils/notify-macos.el")
(load! "lisp/org-clock-csv-utils/org-clock-csv-utils.el")


;; general keybindings --------------------------------------------------------

(general-def
  ;; ;; Commented out to prevent me stretching too frequently (arm fatigue)
  ;; "C-9" #'previous-buffer
  ;; "C-0" #'next-buffer
  "M-[" #'scroll-down-line
  "M-]" #'scroll-up-line
  "C-\\" #'text-scale-increase
  "s-t" #'transpose-frame
  "M-c" 'kill-ring-save
  "M-v" 'yank)

;; TODO: add mac copy/paste keybindings: https://emacs.stackexchange.com/questions/62227/enable-os-x-keys-in-emacs

(general-def '(normal motion)
  ;; ;; Commented out to prevent me from reaching for keys on the edges of the
  ;; ;; keyboard too frequently (arm fatigue)
  ;; "8" #'basic-save-buffer
  ;; "9" #'evil-beginning-of-visual-line
  ;; "0" #'evil-last-non-blank
  "-" #'kill-current-buffer
  "RET" #'evil-last-non-blank
  "<delete>"  #'evil-beginning-of-visual-line
  )

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

;; https://stackoverflow.com/a/6845470/5518304 for opening certain filetypes
;; with an external viewer

;; ;; `dired-find-file-other-window' is bound to "g-O", but I can never remember it
;; (general-def 'normal dired-mode-map
;;   "o" #'dired-find-file-other-window)

(after! dired
  ;; Disable dired-omit-mode by default (can still toggle with M-x dired-omit-mode)
  (setq dired-omit-mode nil)

  (general-unbind 'normal 'dired-mode-map
    "-"))

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


;; compilation -----------------------------------------------------------------

(defvar notify-macos-sound-preference "pop"
  "Defines a default sound that can be used when providing a
  macOS notification. The default available sound choices can be
  found in /System/Library/Sounds for macOS 10.15.7")

(defun compilation-finish-notify-macos (buffer msg)
  "Provide a system notification based on a compilation buffer.
This function is useful when added to the hook
`compilation-finish-functions'."
  ;; Another potentially useful value is `compilation-directory', however macOS
  ;; notifications truncates our messages so there isn't enough space to include
  ;; that information.
  (let* ((cmd-args (buffer-local-value 'compilation-arguments buffer))
         (cmd-dir (buffer-local-value 'compilation-directory buffer))
         (cmd-args-cmd (car cmd-args))
         (msg-status (if (string= msg "finished\n") "success 🎉" "failure 💣"))
         (msg-title (concat "Compilation: " msg-status)))
    (notify-macos cmd-dir msg-title cmd-args-cmd notify-macos-sound-preference)))

(add-hook 'compilation-finish-functions 'compilation-finish-notify-macos)


;; persp / workspace -----------------------------------------------------------

(general-def 'doom-leader-workspace-map
  "1" #'+workspace/other)


;; multiple-cursors
;; https://github.com/gabesoft/evil-mc/issues/83


;; artist mode -----------------------------------------------------------------

;; Not easy (possible?) to use a middle click with Apple Magic Mouse (or is it
;; macOS in general?), so remap the edit menu to the right click.
;; https://stackoverflow.com/a/24045691/5518304
;; (define-key artist-mode-map [down-mouse-3] 'artist-mouse-choose-operation)


;; org -------------------------------------------------------------------------

;; ;; (setq org-agenda-file-regexp "\\`[^.].*\\.org\\(_archive\\)?\\'")
;; (setq org-agenda-start-day "-7d")
;; ;; (setq org-agenda-start-day "-3d")
;; ;; (setq org-agenda-start-day "0d")
;; (setq org-agenda-span 15)
;; ;; (setq org-agenda-span 10)
;; ;; (setq org-agenda-span 5)

;; (add-to-list 'org-capture-templates
;;                '("t" "Personal todo" entry
;;                  (file+headline +org-capture-todo-file "Inbox")
;;                  "* %?\n%a Entered on %U" :prepend t))

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

  ;; Show only entries that have received clocked time on that day.
  (org-agenda-log-mode-items '(clock))

  ;; Specify which files are used for agenda display. These are relative to
  ;; `org-directory'
  (org-agenda-files '("gtd.org" "inbox.org"))

  ;; Specify what directory Org attach stores files to
  (org-attach-id-dir (expand-file-name "~/data/org-attach-data"))

  ;; Record a link to the attached file in `org-stored-links' (default is a link
  ;; to the original file location)
  (org-attach-store-link-p 'file)

  ;; Prevent Org from clocking out when the clocked entry is marked as DONE
  (org-clock-out-when-done nil)

  ;; The length at which the web page title that is extracted by `org-cliplink'
  ;; is truncated
  (org-cliplink-max-length 200)

  :config

  ;; Add keybindings. Why did I use `map!' here rather than a 'general' command
  ;; like I usually do?
  ;; FIXME: this doesn't work?
  (map! :after org-agenda
        :map org-agenda-mode-map
        :localleader
        "l" #'org-agenda-log-mode)

  ;; update the list of included Org modules
  (add-to-list 'org-modules 'org-checklist)

  ;; ;; Global to-do keywords. Based on Doom's default settings, but with added
  ;; ;; logging (i.e. ! and @). NOTE: for some reason this doesn't work if I place
  ;; ;; it in the `:custom' section.
  ;; (setq
  ;;  org-todo-keywords
  ;;  '((sequence "TODO(t!)" "PROJ(p)" "STRT(s!)" "WAIT(w@)" "HOLD(h@)" "|" "DONE(d!)" "KILL(k@)")
  ;;    (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))

  ;; ;; Attempting to move this to org-roam-capture-templates 2023-06-19
  ;; (add-to-list 'org-capture-templates
  ;;              `("k" "Knowledgebase" entry
  ;;                (file "knowledgebase.org")
  ;;                (file ,(expand-file-name (file-name-concat org-directory
  ;;                                         "capture-template-knowledgebase.txt")))
  ;;                :refile-targets ((("knowledgebase.org") :maxlevel . 3))))

  ;; Shadow the existing Capture "todo" entry. FIXME: both "t"s still show up in
  ;; the Capture templates.
  (add-to-list 'org-capture-templates
               '("t" "Personal todo" entry
                 (file+headline +org-capture-todo-file "Inbox")
                 "* %?\n%a Entered on %U" :prepend t))

  ;; For an overview of custom agenda commands see
  ;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html and
  ;; https://orgmode.org/manual/Custom-Agenda-Views.html, but really the
  ;; `org-agenda-custom-commands' docstring provides better information than
  ;; either of the previously mentioned sources. Also see
  ;; https://github.com/rougier/emacs-gtd#agenda-setup-ii for the example this
  ;; definition was based upon
  (setq org-agenda-custom-commands
        `((;; key
           "g"
           ;; desc
           "Getting Things Done (GTD)"
           ;; (cmd1 cmd2 ...)
           ((todo "STRT"
                  (;; (org-agenda-skip-function
                   ;;  '(org-agenda-skip-entry-if 'deadline))
                   ;; (org-agenda-prefix-format "  %-12:c [%e] ")
                   (org-agenda-overriding-header "In progress\n")
                   (org-agenda-over)))
            (agenda nil
                    ((org-agenda-entry-types '(:deadline))
                     ;; (org-agenda-format-date "")
                     (org-deadline-warning-days 365)
                     (org-agenda-show-all-dates nil)
                     (org-agenda-time-grid nil)
                     (org-agenda-skip-deadline-if-done t)
                     ;; (org-agenda-include-deadlines nil)
                     (org-agenda-overriding-header "\nDeadlines\n")))
            (todo "TODO"
                  (;; (org-agenda-skip-function
                   ;;  '(org-agenda-skip-entry-if 'deadline))
                   ;; (org-agenda-prefix-format "  %?-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks\n")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            (todo "IDEA"
                  (;; (org-agenda-skip-function
                   ;;  '(org-agenda-skip-entry-if 'deadline))
                   ;; (org-agenda-prefix-format "  %?-12:c [%e] ")
                   (org-agenda-overriding-header "\nSomeday\n"))))
           ;; match
           ((org-agenda-tag-filter-preset '("-@dev"))
            (org-agenda-prefix-format "  %?-12t% s")))))

  ;; Org Agenda settings
  (setq org-agenda-start-day "-14d" ;; the starting day relative to today
        org-agenda-span 22)        ;; the total number of days that Org Agenda displays

  ;; Org Agenda settings
  (setq
   ;; the starting day relative to today
   org-agenda-start-day "-14d"
   ;; the total number of days that Org Agenda displays. Note that
   ;; `string-to-number' discards any characters after the number before
   ;; converting to an integer
   org-agenda-span (+ 8 (abs (string-to-number org-agenda-start-day))))

  ;; Keep the clock open between sessions (e.g. if you want to restart Emacs)
  ;; https://orgmode.org/manual/Clocking-Work-Time.html#Clocking-Work-Time
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate)

  (load! "lisp/org-utilities/knowledgebase-open.el")
  (load! "lisp/org-utilities/knowledgebase-counsel-goto.el"))

;; HACK: I simply want to list all headlines in 20230619210159-knowledgebase.org
;; but I couldn't figure out how to do it without adding a tag to everything and
;; then filtering on the tag
(setq dp-agenda-custom-commands-knowledgebase
      '(("f"
         "File store"
         tags
         "knowledgebase"
         ((org-agenda-prefix-format "  ")
          (org-agenda-sorting-strategy '(alpha-up))))))

(defun dp-agenda-knowledgebase ()
  "Open the knowledgebase agenda"
  (interactive)
  (let* ((knowledgebase-path (file-name-concat org-roam-directory
                                               "20230619210159-knowledgebase.org"))
         (org-agenda-files (list knowledgebase-path))
         (org-agenda-custom-commands dp-agenda-custom-commands-knowledgebase))
    (org-agenda nil "f")
    (dp-define-key-org-agenda-knowledgebase-open)
    (dp-define-key-counsel-knowledgebase-open)))

(general-def 'doom-leader-notes-map
  "K" #'dp-agenda-knowledgebase)


;; ;; Why doesn't the `use-package!' version work?
;; (use-package! evil-org
;;   :general
;;   (:keymaps 'evil-org-mode-map
;;    "g b" #'outline-up-heading))
(after! evil-org
  (general-def 'motion evil-org-mode-map
    "g b" #'outline-up-heading
    "g j" #'outline-next-heading
    "g k" #'outline-previous-heading))

;; ;; https://fuco1.github.io/2019-02-02-Org-mode-and-google-calendar-sync.html
;; (use-package org-gcal
;;   :after org
;;   :config
;;   (setq org-gcal-client-id "647603884391-2ljnnlbqocg6ub4glfod475og1atndhr.apps.googleusercontent.com"
;;         org-gcal-client-secret "7pTkJSrjULYw0Fx16VvPknnj"
;;         org-gcal-file-alist '(("dpritchard@novisci.com" . "~/Dev/org/gcal-work.org"))
;;         org-gcal-header-alist '(("dpritchard@novisci.com" . "#+PROPERTY: TIMELINE_FACE \"pink\"\n"))
;;         org-gcal-auto-archive nil
;;         org-gcal-notify-p nil)
;;   (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
;;   (add-hook 'org-capture-after-finalize-hook 'org-gcal-fetch)
;;   ;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request
;;   (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

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
  ;; :config
  ;; (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook)
  )

;; ;; https://github.com/atheriel/org-clock-csv
;; (use-package! org-clock-csv)
(load "~/Dev/org-clock-csv/org-clock-csv.el")

(after! org
  (setq org-roam-directory (expand-file-name "~/Dev/org-roam")
        dp-knowledgebase-path (expand-file-name "20230619210159-knowledgebase.org"
                                                org-roam-directory))
  (org-roam-db-autosync-mode))

(load! "lisp/org-effort/org-effort.el")
(general-def 'motion org-mode-map
    "SPC m m" #'dp-org-set-effort)

(defun dp-popup-knowledgebase ()
  "Open knowledgebase in a popup buffer"
  (interactive)
  (+popup-buffer (get-buffer "20230619210159-knowledgebase.org")))

;; (general-def 'doom-leader-notes-map
;;   "K" #'dp-popup-knowledgebase)

(general-def 'doom-leader-notes-map
  "k" #'dp-knowledgebase-read)

(defun dp-org-set-property-id ()
  "Creates a new Org ID and sets the corresponding property"
  (interactive)
  (org-set-property "ID" (org-id-get-create)))


;; org-roam --------------------------------------------------------------------

(load! "lisp/org-utilities/org-create-inactive-timestamp-now.el")
(load! "lisp/org-roam-utilities/org-roam-utilities.el")

(general-def 'doom-leader-notes-map
  "i" #'dp-capture-knowledgebase)

;; (use-package! org-roam
;;   :after (org)

;;   :config

;;   ;; TODO: add `%^{CREATED_BY}p' property
;;   (add-to-list 'org-roam-capture-templates
;;                `("k"
;;                  "knowledgebase"
;;                  entry
;;                  ;; (file ,(expand-file-name (file-name-concat org-directory
;;                  ;;                                            "capture-template-knowledgebase.txt")))
;;                  "* ${title}%?
;; :PROPERTIES:
;; :CREATION_TIMESTAMP: %U
;; :END:"
;;                  :target (file ,(file-name-concat org-roam-directory
;;                                                   "20230619210159-knowledgebase.org"))
;;                  ))
;;   )

;; (add-hook 'org-capture-prepare-finalize-hook 'nom/org-roam-capture-create-id)

;; (defun nom/org-roam-capture-create-id ()
;;   "Create id for captured note and add it to org-roam-capture-template."
;;   (when (and (not org-note-abort)
;;              (org-roam-capture-p))
;;     (org-roam-capture--put :id (org-id-get-create))))

;; (org-set-property "ID" (org-id-get-create))


;; anki-editor -----------------------------------------------------------------

(use-package! anki-editor
  :config
  (setq anki-editor-create-decks nil
        anki-editor-org-tags-as-anki-tags t))


;; switch workspace shortcuts --------------------------------------------------

(defun dp-workspace-switch-config ()
  "Switch to the org workspace and maximize gtd.org"
  (interactive)
  (let ((+workspaces-switch-project-function
         (lambda (_)
           (progn (find-file (expand-file-name "~/.doom.d/config.el"))
                  (doom/window-maximize-buffer)))))
    (+workspaces-switch-to-project-h (expand-file-name "~/.doom.d"))))

(defun dp-workspace-switch-org ()
  "Switch to the org workspace and maximize gtd.org"
  (interactive)
  (let ((+workspaces-switch-project-function
         (lambda (_)
           (progn (find-file (expand-file-name "~/Dev/org/gtd.org"))
                  (doom/window-maximize-buffer)))))
    (+workspaces-switch-to-project-h (expand-file-name "~/Dev/org"))))

(defun dp-workspace-switch-pw ()
  "Switch to the pw file"
  (interactive)
  (let ((+workspaces-switch-project-function
         (lambda (_)
           (progn (find-file (expand-file-name "~/Documents/pw.gpg"))
                  (doom/window-maximize-buffer)))))
    (+workspaces-switch-to-project-h (expand-file-name "~/Documents"))
    (+workspace/rename "pw")))

(general-def 'doom-leader-workspace-map
  "c" #'dp-workspace-switch-config
  "o" #'dp-workspace-switch-org
  "p" #'dp-workspace-switch-pw)


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

;; Note that setting `evil-respect-visual-line-mode' here doesn't work and has
;; to be performed before `evil' is loaded, hence it has been placed in init.el.
;; https://www.reddit.com/r/DoomEmacs/comments/nzpfy1/comment/h1r9bpa/
(use-package! evil
  :custom
  (evil-disable-insert-state-bindings t)
  (evil-move-beyond-eol t))

(use-package! avy
  :custom
  (avy-all-windows t)
  :general
  (:keymaps 'evilem-map
   "g" #'avy-goto-line))

;; set the ace-window keys to the home row, and give `ace-window' the keybinding
;; that was originally assigned to `doom/window-enlargen' (and find an unused
;; binding for that function)
(general-def 'evil-window-map
  "o" #'ace-window
  "e" #'doom/window-enlargen)
(after! ace-window
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-background nil
        aw-dispatch-always t))

;; adding the symbol `tramp-own-remote-path' adds the PATHs used by the remote
;; user to the search path. See
;; https://www.gnu.org/software/tramp/#Remote-programs for details.
(use-package! tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; (use-package! undo-tree
;;   :config
;;   (global-undo-tree-mode))

;; ;; highlight both files and directories in treemacs based on their git status
;; (setq +treemacs-git-mode 'deferred)
;; (setq doom-themes-treemacs-theme "doom-atom")
;; (setq doom-themes-treemacs-theme "doom-colors")
(after! treemacs
  (general-def 'evil-treemacs-state-map
  "C-=" #'text-scale-increase
  "C--" #'text-scale-decrease)
  (setq doom-themes-treemacs-theme "doom-colors")
  ;; https://github.com/Alexander-Miller/treemacs/issues/228#issuecomment-403585786
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

(general-def
  "M-{" #'er/expand-region
  "M-}" #'er/contract-region
  "M-O" #'er/mark-defun
  "M-P" #'er/mark-paragraph
  "s-o" #'er/mark-outside-pairs
  "s-i" #'er/mark-inside-pairs)


(use-package! smartparens
  :config
  (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'comint-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'text-mode-hook #'turn-on-smartparens-mode))

(use-package! evil-smartparens
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


;; deadgrep --------------------------------------------------------------------

(use-package! deadgrep)


;; helpful ---------------------------------------------------------------------

(setq find-function-C-source-directory "~/Documents/software/emacs")


;; comint ----------------------------------------------------------------------

;; customize comint (command interpreter) settings, as described in the ESS
;; manual, section 4.3
(eval-after-load "comint"
   '(progn
      ;; (define-key comint-mode-map [up]
      ;;   'comint-previous-matching-input-from-input)
      ;; (define-key comint-mode-map [down]
      ;;   'comint-next-matching-input-from-input)
      ;; ;; also recommended for ESS use --
      (setq comint-scroll-to-bottom-on-output 'others)
      (setq comint-scroll-show-maximum-output t)
      ;; ;; somewhat extreme, almost disabling writing in *R*, *shell* buffers above prompt:
      ;; (setq comint-scroll-to-bottom-on-input 'this)
      ))

(general-def 'normal 'comint-mode-map
  "RET" #'comint-send-input)

(general-def 'insert 'comint-mode-map
  [up] nil ; previously `comint-previous-input'
  [down] nil) ; previously `comint-next-input'

(defvar +shell--popup-buffer nil
  "Most recent popup shell buffer cached by `dp-+shell/toggle'.")

(defun +shell--process-sentinel (process event)
  "Close popup window when shell process exits"
  (when (memq (process-status process) '(exit signal))
    (when-let* ((buf (process-buffer process))
                ;; Only close if this buffer is our cached popup buffer
                ((eq buf +shell--popup-buffer))
                (win (get-buffer-window buf 'visible)))
      (quit-restore-window win nil)
      (setq +shell--popup-buffer nil))))

;;;###autoload
(defun dp-+shell/toggle (&optional arg)
  "Toggle a bottom popup `shell' in the project root.

* If the popup is visible **and selected**, hide it (`quit-window`).
* Otherwise show it, creating the buffer if necessary.
* With `C-u` (\\[universal-argument]) always make a *fresh* shell
  in the current directory instead of re‑using the cached one."
  (interactive "P")
  (let* ((project-root (or (doom-project-root) default-directory))
         (bufname      (format "*doom:shell-popup:%s*" project-root))
         (buf          (get-buffer bufname))
         (win          (and buf (get-buffer-window buf 'visible))))
    (cond
     ;; ─────────────────────────────────────────────────────────────── hide ──
     ((and win (eq (selected-window) win))
      (quit-window nil win))                 ;closes side‑window & leaves layout
     ;; ─────────────────────────────────────────────────────────────── show ──
     (t
      (unless (and buf (buffer-live-p buf)
                   (get-buffer-process buf)  ; Check if process is still alive
                   (not arg))
        ;; (re)create the buffer
        (let ((default-directory project-root))
          (setq buf (shell bufname)))        ;Comint‑based shell
        (with-current-buffer buf
          (doom-mark-buffer-as-real-h)       ;so Doom won’t auto‑GC it
          ;; Close window when shell process exits (e.g., Ctrl-d)
          (when-let ((proc (get-buffer-process buf)))
            (set-process-sentinel proc #'+shell--process-sentinel))))
      (setq +shell--popup-buffer buf)
      ;; display‑buffer‑in‑side‑window provides automatic slot/size handling
      (pop-to-buffer
       buf
       '((display-buffer-reuse-window
          display-buffer-in-side-window)
         (side . bottom) (slot . 1)
         (window-height . 0.3)
         (select . t)))))))                  ;stay in the shell window

;;; ---------------------------------------------------------------------------
;;; Key binding – “Shell (popup)” appears under  SPC o c
;;; ---------------------------------------------------------------------------
(map! :leader
      (:prefix ("o" . "open")
       :desc "Shell (popup)" "c" #'dp-+shell/toggle))


;; magit -----------------------------------------------------------------------

(use-package! magit

  :general
  ;; ;; git-gutter seems to have been removed in favor of hl-diff
  ;; (:keymaps 'doom-leader-git-map
  ;;  :wk-full-keys nil
  ;;  "d" '(git-gutter:popup-diff :which-key "Popup hunk diff")
  ;;  "w" '(git-gutter:update-all-windows :which-key "Update window's gutter")
  ;;  "W" '(git-gutter:update-all-windows :which-key "Update all window's gutters"))

  :config
  ;; Don't query for confirmation when the summary line is too long
  (delete 'overlong-summary-line git-commit-style-convention-checks))

;; A guide for setting up forge:
;; https://gist.github.com/Azeirah/542f1db12e3ef904abfc7e9c2e83310e
(after! auth-source
  (setq auth-sources '("~/.authinfo")))


;; ESS -------------------------------------------------------------------------

(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter)
  (smartparens-mode 1))
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

;; TODO: see https://github.com/emacs-ess/ESS/issues/1137 for a possible config
;; tweak?

(defun ess-r-package-root (&optional dir)
  "Return the path of the current package as a string."
  (plist-get (ess-r-package-info dir) :root))

;; FIXME: does this work for a remote process?
(defun dp-r-package-or-project-root (&optional dir)
  "Return the path of the current R package or project as a string."
  (or (ess-r-package-root dir) (projectile-project-root dir)))

(after! ess

  (setq ess-style 'RStudio                                     ; set the indentation style to mimic RStudio's
        ess-indent-with-fancy-comments nil                     ; always indent comments to current code depth
        ess-plain-first-buffername nil                         ; name the first R process R:1 for consistency
        ess-auto-width 'window                                 ; synchronize R's "width" option to the window width
        ess-roxy-str "#'"                                      ; so Roxygen comments are #' not ##'
        inferior-R-args "--no-restore-data --no-save"          ; command line parameters when starting R
        ;; ess-directory-function #'dp-r-package-or-project-root
        ) ; suggest the package or project root when launching a new R process

  ;; prevent adding an additional hash to comments (i.e. so that comments are # not ##)
  (add-hook 'ess-mode-hook (lambda () (setq-local comment-add 0)))
  (add-hook 'ess-mode-hook (lambda () (setq-local ansi-color-for-comint-mode 'filter)))

  (general-def 'ess-mode-map
    ";" #'ess-insert-assign
    "C-j" #'ess-eval-region-or-line-visibly-and-step)

  ;; this clobbers the keybinding to "C-c C-k" for `ess-force-buffer-current'
  (general-def 'ess-r-mode-map
    "C-S-m" (lambda () (interactive) (insert " %>% "))
    "C-c C-h" #'dp-ess-eval-word
    "C-c C-k" #'dp-ess-str-word)

  (general-def 'inferior-ess-mode-map
    ";" 'ess-insert-assign
    "C-S-m" (lambda () (interactive) (insert " %>% ")))

  ;; (load! "lisp/ess+/ess-history.el")
  ;; (load! "lisp/ess+/ess-utils.el")
  )


;; Haskell ---------------------------------------------------------------------

(general-def 'haskell-mode-map
  "C-c C-s" #'haskell-interactive-bring)  ; clobbers `haskell-mode-toggle-scc-at-point'

;; ;; TODO: why doesn't this work? It says that `haskell-hoogle' is undefined when using SPC m s
;; (general-def 'haskell-mode-map
;;   "C-c C-s" #'haskell-interactive-bring   ; clobbers `haskell-mode-toggle-scc-at-point'
;;   "SPC m s" #'haskell-hoogle)

; with regards to `turn-off-smartparens-strict-mode': for some reason the `-'
; character gets read as a delimiter so that pressing `-' causes an error. It
; would be nice to fix the root of this issue rather than using this workaround
;
; subword-mode makes word boundaries appear within camel-case words. See
; https://www.gnu.org/software/emacs/manual/html_node/emacs/MixedCase-Words.html
(after! haskell
  (add-hook 'haskell-mode-hook #'subword-mode)
  (add-hook 'haskell-mode-hook #'turn-off-smartparens-strict-mode)
  (load! "lisp/utils/stack-workaround.el"))


;; Dhall -----------------------------------------------------------------------

;; https://docs.dhall-lang.org/howtos/Text-Editor-Configuration.html#emacs
(use-package! dhall-mode
  :hook (dhall-mode . lsp)
  :config
  (setq dhall-use-header-line nil))

;; (after! dhall
;;   (setq dhall-format-at-save nil))


;; AsciiDoc --------------------------------------------------------------------

(add-hook! adoc-mode
  (add-to-list 'auto-mode-alist '("\\.adoc" . adoc-mode))
  (flyspell-mode t))


;; nicer `open-line' ----------------------------------------------------------

(defun open-line-and-indent ()
  "Like `open-line', but with proper indentation."
  (interactive)
  (save-excursion
    (newline-and-indent)))

;; ;; replace `open-line'
;; (global-set-key [remap open-line] #'open-line-and-indent)


;; Python ---------------------------------------------------------------------

(after! python
  (setq python-fill-docstring-style 'pep-257-nn))

;; Configure lsp-mode and lsp-pyright for Python. This is a fallback for files
;; that don't have a direnv setup
(after! lsp-mode
  (setq lsp-pyright-python-executable-cmd "python"
        lsp-pyright-venv-path ".venv"))

;; https://github.com/emacs-lsp/lsp-mode/issues/3390
;; https://emacs.stackexchange.com/questions/13489/how-do-i-get-emacs-to-recognize-my-python-3-virtual-environment
;; https://www.reddit.com/r/emacs/comments/ejc1az/comment/fcwz0gk/?utm_source=share&utm_medium=web2x&context=3
(use-package pyvenv
  :config
  (pyvenv-mode 1)
  (add-hook 'python-mode-hook
            (lambda ()
              (envrc-reload)
              (let ((venv-path (or (getenv "VIRTUAL_ENV")
                                   (locate-dominating-file default-directory ".venv"))))
                (when venv-path
                  (pyvenv-activate venv-path))))))

;; Use python-shell-interpreter to respect the virtual environment
(setq python-shell-interpreter "python3")

;; Update lsp-pyright configuration dynamically based on direnv's VIRTUAL_ENV
(add-hook 'python-mode-hook
          (lambda ()
            (envrc-reload)
            (let ((venv-path (getenv "VIRTUAL_ENV")))
              (when venv-path
                (setq lsp-pyright-venv-path venv-path
                      lsp-pyright-python-executable-cmd (concat venv-path "/bin/python"))))))


;; LaTeX -----------------------------------------------------------------------

(add-hook 'latex-mode-hook #'git-gutter-mode)


;; LSP -------------------------------------------------------------------------

(use-package! lsp-mode
  ;; :general
  ;; ('
  ;;  "a" 'org-mru-clock-add-note
  ;;  "h" 'org-mru-clock-to-history
  ;;  "i" 'org-mru-clock-in
  ;;  "n" 'org-mru-clock-show-narrowed
  ;;  "g" 'org-mru-clock-goto
  ;;  "s" 'org-mru-clock-select-recent-task)
  :config
  ;; Add the `env' directory to the list of directories to ignore for the LSP
  ;; client under the assumption/convention that this is where virtual
  ;; environments will be stored in my Python projects
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]venv\\'"))

;; (use-package! lsp-ui
;;   :config
;;   (general-def 'normal "'" lsp-ui-mode-map)
;;   (general-def 'lsp-ui-mode-map
;;     "c" #'dp-lsp-ui-doc-toggle-show-with-cursor
;;     "e" #'flycheck-list-errors
;;     "g" #'lsp-ui-doc-glance
;;     "G" #'lsp-ui-doc-show
;;     "m" #'dp-lsp-ui-doc-toggle-show-with-mouse
;;     "s" #'lsp-ui-sideline-toggle-symbols-info))

(defun dp-lsp-ui-doc-toggle-show-with-cursor ()
  "Toggle the value of `lsp-ui-doc-show-with-cursor'"
  (interactive)
  (if lsp-ui-doc-show-with-cursor
      (setq lsp-ui-doc-show-with-cursor nil)
    (setq lsp-ui-doc-show-with-cursor t)))

(defun dp-lsp-ui-doc-toggle-show-with-mouse ()
  "Toggle the value of `lsp-ui-doc-show-with-mouse'"
  (interactive)
  (if lsp-ui-doc-show-with-mouse
      (setq lsp-ui-doc-show-with-mouse nil)
    (setq lsp-ui-doc-show-with-mouse t)))


;; shell -----------------------------------------------------------------------

;; Turn off automatic company completion in shell mode buffers (note that you
;; can still use `C-SPC' to manually invoke a completion). In sh-mode company
;; tries to find a completion by looking up all of the available shell commands
;; on $PATH which sometimes locks up Emacs for 5 or 10 seconds at a time (and is
;; hardly ever very useful)
(add-hook 'sh-mode-hook (lambda () (setq-local company-idle-delay nil)))


;; chatgpt-shell ---------------------------------------------------------------

;; (setq slack-prompt
;;       "You are ChatGPT, and your responses will be pasted directly into Slack. Slack supports a limited set of Markdown-like formatting. Format your responses accordingly using the following rules:

;; - Use `*bold*` for bold text.
;; - Use `_italic_` for italic text.
;; - Use `` `inline code` `` for inline code.
;; - Use triple backticks for multi-line code blocks **without specifying a language**. Don't include any extra newlines of whitespace after the code.
;; - Use `>` for blockquotes.
;; - Use `-` or `•` for bullet points.
;; - Use newlines to separate paragraphs (avoid excessive whitespace).
;; - Avoid using full Markdown features that Slack does not support (e.g., headings `#`, tables, or full hyperlinks `[text](url)`, as they won’t render properly).
;; - Avoid lists that have embedded multi-line code blocks as they won’t render properly.

;; ;; Ensure that your responses are well-structured, easy to read, and formatted for direct use in Slack.")

;; (use-package! chatgpt-shell
;;   :config
;;   (setq chatgpt-shell-openai-key
;;         (lambda ()
;;           (auth-source-pick-first-password :host "api.openai.com")))
;;   (map! :leader
;;         :desc "ChatGPT Shell" "l" #'chatgpt-shell)
;;   (map! :map chatgpt-shell-mode-map
;;         "M-RET" #'chatgpt-shell-submit
;;         "RET" nil) ; Default is that RET sends a command to the LLM
;;   ;; (push (cons "Slack markdown" slack-prompt) chatgpt-shell-system-prompts)
;;   )

;; (after! chatgpt-shell
;;   (map! :map chatgpt-shell-mode-map
;;         :localleader
;;         :desc "Send message"        "s" #'chatgpt-shell-send-message
;;         :desc "Clear buffer"        "c" #'chatgpt-shell-clear-buffer
;;         :desc "Show history"        "h" #'chatgpt-shell-show-history
;;         :desc "Regenerate response" "r" #'chatgpt-shell-regenerate-response))


;; gptel -----------------------------------------------------------------------

;; ;; NOTE: uses the entry in ~/.authinfo for credentials
;; (use-package! gptel
;;   :config
;;   (map! :leader
;;         :desc "Send LLM query" "k" #'gptel-menu)
;;   (map! :map chatgpt-shell-mode-map
;;         "M-RET" #'gptel-send)
;;   (setq gptel-default-mode #'org-mode))

;; Functions for cleaning up gptel responses in shell modes
(defun dp-gptel-fix-shell-newlines (start end)
  "Remove unwanted newlines before gptel responses in shell modes."
  (when (derived-mode-p 'shell-mode 'eshell-mode 'term-mode 'vterm-mode)
    (save-excursion
      ;; Look backwards from start to find and remove newlines
      (goto-char start)
      (while (and (> (point) (point-min))
                  (looking-back "\n" (1- (point))))
        (delete-char -1)
        (setq start (1- start))
        (setq end (1- end))))))

(defun dp-gptel-shell-mode-setup ()
  "Configure gptel for shell modes without extra newlines."
  (when (derived-mode-p 'shell-mode 'eshell-mode 'term-mode 'vterm-mode)
    ;; Set buffer-local variable to override default "\n\n" separator
    (setq-local gptel-response-separator "")))

(after! gptel

  ;; Add keybindings to `gptel-mode-map'
  (map! :map gptel-mode-map
        "M-<return>" #'gptel-send
        :leader
        "k" #'dp-gptel-clear-history)

  ;; Register the backend and its models. See the `gptel' org-roam documentation
  ;; for an explanation for how these model names were obtained
  (gptel-make-anthropic "Claude"
    :stream t
    :key #'gptel-api-key
    :models '(claude-opus-4-20250514
              claude-sonnet-4-20250514
              claude-3-7-sonnet-20250219
              claude-3-5-sonnet-20241022
              claude-3-5-haiku-20241022
              claude-3-5-sonnet-20240620
              claude-3-haiku-20240307
              claude-3-opus-20240229))

  ;; ;; Pick the default model
  ;; ;; For some reason I get a warning when starting with sonnet
  ;; (setq gptel-model 'claude-sonnet-4-20250514)
  (setq gptel-model 'gpt-4o)

  ;; Set org-mode as the default rather than markdown mode
  (setq gptel-default-mode #'org-mode)

  ;; Hook runs after gptel inserts a response, allowing us to clean up unwanted formatting
  (add-hook 'gptel-post-response-functions #'dp-gptel-fix-shell-newlines)

  ;; These hooks ensure gptel configuration is applied when shell buffers are created
  ;; Each shell mode needs its own hook since they're separate major modes
  (add-hook 'shell-mode-hook #'dp-gptel-shell-mode-setup)
  (add-hook 'eshell-mode-hook #'dp-gptel-shell-mode-setup)
  (add-hook 'term-mode-hook #'dp-gptel-shell-mode-setup)
  (add-hook 'vterm-mode-hook #'dp-gptel-shell-mode-setup))

(use-package! gptel-prompts
  :after gptel
  :config
  (setq gptel-prompts-directory "~/.doom.d/gptel-prompt-entries")
  (gptel-prompts-update)
  (gptel-prompts-add-update-watchers))

(defun dp-gptel-clear-history ()
  "Erase the current `gptel-mode' conversation and re‑initialise the buffer.

After running, the buffer is identical to what you get from
`M-x gptel` (or `C-u M-x gptel`) but it keeps the same buffer
name, major mode and backend/model settings."
  (interactive)
  (unless gptel-mode
    (user-error "This command only works in a gptel chat buffer"))
  (let ((inhibit-read-only t))
    ;; 1. Remove every gptel overlay and its text properties
    (remove-overlays (point-min) (point-max) 'gptel t)
    (set-text-properties (point-min) (point-max) nil)

    ;; 2. Forget the saved bounds so the next save doesn’t resurrect them
    (setq-local gptel--bounds nil)

    ;; 3. Wipe the visible text
    (erase-buffer)

    ;; 4. Insert the standard first‑prompt prefix
    (insert (gptel-prompt-prefix-string))
    (goto-char (point-max)))

  (message "gptel history cleared – start typing a new prompt."))


;; uncollected functions -------------------------------------------------------

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

(load! "lisp/occur-non-ascii/occur-non-ascii.el")

(defun fill-by-sentence ()
  "Fills the current paragraph, but starts each sentence on a new line."
  (interactive)
  (save-excursion
    ;; Select the entire paragraph
    (mark-paragraph)
    ;; Move to the start of the paragraph
    (goto-char (region-beginning))
    ;; Record the location of the end of the paragraph
    (let ((end-of-paragraph (copy-marker (region-end))))
      ;; Loop over each sentence in the paragraph
      (forward-sentence)
      ;;
      (while (and (< (point) end-of-paragraph) (not (equal (following-char) ?\n)))
        ;; Check that we don't have an instance of one of the following known
        ;; false positive sentence endings
        (unless (let* ((prev-line-str (buffer-substring (save-excursion (beginning-of-line) (point))
                                                       (point)))
                      (next-line-str (buffer-substring (point)
                                                       (save-excursion (end-of-line) (point)))))
                  (or (string-match-p "e\\.g\\.$" prev-line-str)
                      (string-match-p "i\\.e\\.$" prev-line-str)
                      (string-match-p "c\\.f\\.$" prev-line-str)
                      (and (string-match-p "etc\\.$" prev-line-str)
                           (string-match-p "[[:blank:]]*[^[:upper:]]" next-line-str))))
          (delete-region (point) (progn (skip-chars-forward " \t") (point)))
          (insert "\n"))
        (forward-sentence))))
  (pop-mark))

;; from https://gist.github.com/kristianhellquist/3082383?permalink_comment_id=2373734#gistcomment-2373734
(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))
