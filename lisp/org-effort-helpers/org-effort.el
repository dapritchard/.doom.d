;;; lisp/org-effort-helpers/org-effort.el -*- lexical-binding: t; -*-

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
