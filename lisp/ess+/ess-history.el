;;; lisp/ess+/ess-history.el -*- lexical-binding: t; -*-

(require 'comint)

;; these are the two functions (that I am aware of) which ultimately send inputs
;; to the inferior process
(advice-add #'ess-eval-linewise :after #'ess-history-for-eval-linewise)
(advice-add #'ess-send-string :after #'ess-history-for-send-string)

(defun ess-history-for-send-string (process string &optional visibly _message _type)
  "Add an entry to the comint history associated with a process.
This function is intended to be used as advice for
`ess-send-string'. When VISIBLY is t then the work is handed off
to `ess-eval-linewise' so we will allow other handlers to deal
with that case and this function becomes a no-op."
  (unless (eq visibly t)
    (ess-history--add-to-input-history (process-buffer process) string)))


(defun ess-history-for-eval-linewise
    (text &optional _invisibly _eob _even-empty _wait-last-prompt _sleep-sec _wait-sec)
  "Add an entry to the comint history associated with a process.
This function is intended to be used as advice for
`ess-eval-linewise'."
  (let*
      ((sprocess (ess-get-process ess-current-process-name))
       (sbuffer (process-buffer sprocess)))
    (ess-history--add-to-input-history sbuffer text)))


(defun ess-history--add-to-input-history (comint-buffer str)
  "Add an entry to a comint buffer's history.
Take a buffer associated with a comint buffer COMINT-BUFFER and a
string STRING as inputs, and add a trimmed and propertized
version of STRING to the buffer's history ring."
  (let*
      ((extracted-str (ess-history--extract-from-essr str))
       (trimmed-str (ess-history--strip-lead-trail-newline extracted-str))
       (propertized-str (propertize trimmed-str
                                    'font-lock-face
                                    'comint-highlight-input)))
    (with-current-buffer comint-buffer
      (comint-add-to-input-history propertized-str))))


(defun ess-history--strip-lead-trail-newline (str)
  "Strip any leading and trailing newlines from a string."
  (unless (stringp str)
    (error "The input to STR must be a string."))
  (thread-last str
   (replace-regexp-in-string "\\`\n*" "")
   (replace-regexp-in-string "\n*\\'" "")))


(defun ess-history--extract-from-essr (str)
  (if (string-match-p "\\`\\.ess\\." str)
      (thread-last str
        (replace-regexp-in-string "\\`[^\"]*\"" "")
        (replace-regexp-in-string "\"[^\"]*\\'" ""))
    str))

(provide 'ess-history)