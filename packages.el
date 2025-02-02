;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! ivy :built-in 'prefer)  ;; org-mru-clock seems to rely on this
;; (unpin! ess)
;; (package! ess
;;   :recipe (:local-repo "ess" :branch "master" :host github :repo "dapritchard/ESS"))
;; (package! org-gcal
;;   :recipe (:host github :repo "kidd/org-gcal.el"))
(package! evil-smartparens)
(package! org-mru-clock)
;; (package! org-clock-csv)
(package! deadgrep)
(package! adoc-mode)
;; FIXME we don't need both of these, right?
(package! transpose-frame
 :recipe (:host github :repo "emacsorphanage/transpose-frame"
          :files ("transpose-frame.el")))
(package! shell-maker
  :recipe (:host github :repo "xenodium/shell-maker" :files ("shell-maker*.el")))
(package! chatgpt-shell
;; to resolve errors like
;;     Error (org-roam): Failed to process /Users/david.pritchard/Dev/org-roam/20230811173135-git_comand_for_making_one_branch_look_like_another.org with error Wrong type argument: integer-or-marker-p, nil, skipping...
;;
;; See
;;     https://www.reddit.com/r/emacs/comments/15jyzz7/strange_orgroam_bug_with_links_and_more/
;;     https://github.com/org-roam/org-roam/issues/2361#issuecomment-1650957932
(package! org :pin "ca873f7") ;;
  :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el")))
(package! anki-editor
  :recipe (:host github :repo "louietan/anki-editor"))
(package! org-anki
  :recipe (:host github :repo "eyeinsky/org-anki"))
