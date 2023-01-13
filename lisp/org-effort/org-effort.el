;;; lisp/org-effort-helpers/org-effort.el -*- lexical-binding: t; -*-

(defvar dp-colon-substitutes "[:.-]")

(defun dp-org-set-effort (effort effort-lower effort-upper)
  "In the current entry, set 'Effort', 'effort_lower', and 'effort_upper' properties"
  (interactive (list nil nil nil))
  ;; Read the effort point estimate
  (let* ((effort (dp-read-effort "Effort estimate: "))
         (effort-mins (dp-effort-to-mins effort)))
    ;; Read the effort lower bound and assert that it is no greater than the
    ;; point estimate
    (let* ((effort-lower (dp-read-effort "Effort lower bound: "))
           (effort-lower-mins (dp-effort-to-mins effort-lower)))
      (when (> effort-lower-mins effort-mins)
        (user-error "Effort lower bound is greater than effort point estimate (%s is greater than %s)"
                    effort-lower
                    effort))
      ;; Read the effort upper bound and assert that it is no less than the point
      ;; estimate
      (let* ((effort-upper (dp-read-effort "Effort upper bound: "))
             (effort-upper-mins (dp-effort-to-mins effort-upper)))
        (when (> effort-mins effort-upper-mins)
          (user-error "Effort point estimate is greater than effort upper bound (%s is greater than %s)"
                      effort
                      effort-upper))
        ;; Add the properties to the current entry
        (org-entry-put nil "Effort" (dp-effort-to-str effort))
        (org-entry-put nil "effort_lower" (dp-effort-to-str effort-lower))
        (org-entry-put nil "effort_upper" (dp-effort-to-str effort-upper))))))

(defun dp-read-effort (msg)
  "Prompt the user for input with the message `msg' and transform the output using `dp-create-effort'"
  (dp-create-effort (read-string msg)))

(defun dp-create-effort (effort-str)
  "Normalize the effort string EFFORT-STR and return a list of hours and minutes.

This function takes a string of the form HH:MM, where HH is the
number of hours and MM is the number of minutes, and returns a
list of the form ((\"hours\" . HH) (\"minutes\" . MM)). The input
string is not modified.

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
              (list (cons 'effort-digits
                          (list (cons 'hours hours)
                                (cons 'minutes minutes)))
                    (cons 'effort-strings
                          (list (cons 'hours hours-chr)
                                (cons 'minutes minutes-chr))))
            (user-error "Minutes must be less than 60 in entry %s" effort-str)))
      (user-error "Invalid effort entry format: %s" effort-str))))

(defun dp-effort-to-mins (effort)
  "Convert a pair of hours and minutes to the number of minutes they represent."
  (let* ((effort-digits (cdr (assoc 'effort-digits effort)))
         (hours (cdr (assoc 'hours effort-digits)))
         (minutes (cdr (assoc 'minutes effort-digits))))
    (+ (* 60 hours) minutes)))

(defun dp-effort-to-str (effort)
  "Convert a pair of hours and minutes to a string"
  (let* ((effort-strings (cdr (assoc 'effort-strings effort)))
         (hours (cdr (assoc 'hours effort-strings)))
         (minutes (cdr (assoc 'minutes effort-strings))))
    (format "%s:%s" hours minutes)))
