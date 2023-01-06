;;; lisp/org-effort-helpers/org-effort.el -*- lexical-binding: t; -*-

(defvar dp-colon-substitutes "[:.-]")

(defun dp-org-set-effort (effort effort-lower effort-upper)
  "In the current entry, set 'Effort', 'effort_lower', and 'effort_upper' properties"
  (interactive (list nil nil nil))
  (let ((effort (read-string "Effort estimate: ")))

    (let ((effort-lower (read-string "Effort lower bound: ")))

      (let ((effort-upper (read-string "Effort upper bound: ")))

        (org-entry-put nil "Effort" effort)
        (org-entry-put nil "effort_lower" effort-lower)
        (org-entry-put nil "effort_upper" effort-upper)
      ))))

(defun dp-normalize-effort (effort-str)
  "Normalize the effort string EFFORT-STR and return a list of hours and minutes.

This function takes a string of the form HH:MM or HHh MMm, where HH is the
number of hours and MM is the number of minutes, and returns a list of the
form ((\"hours\" . HH) (\"minutes\" . MM)). The input string is not modified.

Returns: a list of hours and minutes as (hours . HH) and (minutes . MM) cons cells."
  (let* ((re-digits "\\([[:digit:]]+\\)")
         (re-two-digits "\\([[:digit:]][[:digit:]]\\)")
         (re-effort (concat "^"
                            re-digits
                            dp-colon-substitutes
                            re-two-digits
                            "$")))
    (if (string-match re-effort effort-str)
        (let* ((hours-chr (replace-regexp-in-string re-effort "\\1" effort-str))
               (hours (string-to-number hours-chr))
               (minutes-chr (replace-regexp-in-string re-effort "\\2" effort-str))
               (minutes (string-to-number minutes-chr)))
          (if (< minutes 60)
              (list (cons 'hours hours)
                    (cons 'minutes minutes))
            (user-error "Minutes must be less than 60")))
      (user-error "Invalid effort entry: %s" effort-str))))
