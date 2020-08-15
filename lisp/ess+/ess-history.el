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
       (propertized-str (ess-history-add-r-text-properties trimmed-str)))
    (with-current-buffer comint-buffer
      ;; when expressions are entered directly into the R prompt then they are
      ;; already added to the history, but we do more work to propertize the
      ;; text so let's use our version (and fortunately we are happy to delete
      ;; consecutive duplicates in the other situation that this can arise)
      (when (and comint-input-ring
                 (string= extracted-str (ring-ref comint-input-ring 0)))
        (ring-remove comint-input-ring 0))
      (comint-add-to-input-history propertized-str))))


(defun ess-history--strip-lead-trail-newline (str)
  "Strip any leading and trailing newlines from a string."
  (unless (stringp str)
    (error "The input to STR must be a string."))
  (thread-last str
   (replace-regexp-in-string "\\`\n*" "")
   (replace-regexp-in-string "\n*\\'" "")))


(defun ess-history--extract-from-essr (str)
  "Extract an expression wrapped in an ESSR function call.
Some ESS commands send R expressions to the interpreter in a form
like .ess.eval(\"expr\", ...). This command attempts to extract
that expression, if necessary."
  (if (string-match-p "\\`\\.ess\\." str)
      (thread-last str
        (replace-regexp-in-string "\\`[^\"]*\"" "")  ; everything up to the first " (inclusive)
        (replace-regexp-in-string "\"[^\"]*\\'" "")  ; everything after the last " (inclusive)
        (replace-regexp-in-string "\\\"" "\"")       ; escaped " becomes unescaped
        (replace-regexp-in-string "\\\\" "\\"))      ; escaped \ becomes unescaped
    str))


(defun ess-history-add-r-text-properties (str)
  "Add ess-r-mode text properties to STR."
  (with-temp-buffer
    (ess-r-mode)
    (insert str)
    (buffer-string)))


(provide 'ess-history)
