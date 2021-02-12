;;; lisp/utils/notify-macos.el -*- lexical-binding: t; -*-

;; The idea for this is taken from https://emacs.stackexchange.com/a/29713/15552
;; Also see https://developer.apple.com/library/archive/documentation/LanguagesUtilities/Conceptual/MacAutomationScriptingGuide/DisplayNotifications.html
(defun notify-macos (msg &optional title subtitle sound)
  "Send a notification on a macOS system.
Take a string MSG and optional strings TITLE, SUBTITLE, and SOUND
as inputs, and send a system notification using AppleScript. If
title is not provided, then the name of the Emacs application is
used for the title."
  ;; See the following link for the AppleScript display notification command.
  ;;  https://developer.apple.com/library/archive/documentation/LanguagesUtilities/Conceptual/MacAutomationScriptingGuide/DisplayNotifications.html
  (let* ((applescript-msg
          (concat (notify-macos--create-element-str "display notification" msg)
                  (notify-macos--create-element-str "with title" title)
                  (notify-macos--create-element-str "subtitle" subtitle)
                  (notify-macos--create-element-str "sound name" sound))))
    (ns-do-applescript applescript-msg)))

(defun notify-macos--create-element-str (command value)
  "Concatenate inputs and wrap 2nd term in escaped quotes."
  (if value
      (concat command
              " \""
              (notify-macos--escape-special-chars value)
              "\" ")
    ""))

(defun notify-macos--escape-special-chars (str)
  "Create a string with escaped AppleScript special characters."
  ;; the special characters that need to be escaped in AppleScript are \ and "
  (replace-regexp-in-string "\\([\\\"]\\)" "\\\\\\1" str))
