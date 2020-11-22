;; -*- lexical-binding: t; -*-

(defun dp-strip-lead-trail-newline (str)
  "Strip any leading and trailing newlines from a string."
  (unless (stringp str)
    (error "The input to STR must be a string."))
  (thread-last str
   (replace-regexp-in-string "\\`\n*" "")
   (replace-regexp-in-string "\n*\\'" "")))


;; TODO: comint-input-filter-functions propertizes the text, maybe we should do
;; the same?
(defun dp-ess-add-to-history (text &optional invisibly eob even-empty wait-last-prompt sleep-sec wait-sec)
  (message "Is this on?")
  (comint-add-to-input-history text))


;; comint.el notes

;; `process-send-string' seems to be one of the primitives for interacting with
;; a process?
;;
;;
;; `with-ess-process-buffer' allows you to execute in the process buffer
;;
;; `comint-send-input' is the command that is used when RET is entered in the
;; process buffer. This ultimately funcalls (I think) `comint-input-sender',
;; which for `inferior-ess-mode' is bound to `inferior-ess-input-sender'. This
;; in turn calls either `ess-eval-linewise' or `ess-send-string' depending on
;; the value of `comint-process-echoes'. Each of these function ultimately send
;; the text to the process using `process-send-string'.
;;

;; `comint-input-ring' is a ring data-type that stores the input ring

;; see `comint-read-input-ring' for an example of how to read the command ring
;; into a temporary buffer

;; TODO: might also want to consider using `with-ess-process-buffer'. What are
;; the pros and cons?
(defun dp-ess-find-most-recent-assignment-v1 ()
  (let*
      ((*proc* (ess-get-process))
       (inf-process-buffer (process-buffer *proc*))
       (inf-input-ring (buffer-local-value 'comint-input-ring inf-process-buffer))
       (assignment-var-str (dp-ess-find-most-recent-assignment-str inf-input-ring)))
    (ess-eval-linewise assignment-var-str)))


(defun dp-ess-find-most-recent-assignment ()
  (with-ess-process-buffer nil
    (let*
        ((assignment-var-str (dp-ess-find-most-recent-assignment-str comint-input-ring)))
      (ess-eval-linewise assignment-var-str))))




(defun dp-ess-find-most-recent-assignment-str (input-ring)
  (let*
      ((history-buf (get-buffer-create "*Temp Reverse Input History*"))
       (input-ring-len (ring-length input-ring))
       (index 0)
       (assignment-str))
    (while (and (not assignment-str) (< index input-ring-len))
      (setq assignment-str (extract-assignment-target (ring-ref input-ring index)))
      (setq index (1+ index)))
    assignment-str))


(defun extract-assignment-target (str)
  (let*
      ((nocomment-str (strip-comments str))
       (extraction-fcn-list '(extract-assignment-target-leftassign))
       (assignment-target-str))
    (cl-loop for f in extraction-fcn-list
             until assignment-target-str
             do (setq assignment-target-str (funcall f nocomment-str)))
    assignment-target-str))


(defun extract-assignment-target-leftassign (nocomment-str)
  (let*
      ((before-leftassign-str))
    (when (string-match-p "<-" nocomment-str)
      (setq before-leftassign-str (replace-regexp-in-string "^\\(.*?\\)<-.*" "\\1" nocomment-str))
      (string-trim before-leftassign-str))))


(defun my-nil-fcn (str)
  nil)


;; (defun check-if-assignment-str (str)
;;   (let*
;;       (nocomment-str (replace-regexp-in-string "#.*$" "" str))
;;     (string-match-p "<-" nocomment-str)))


;; surely this function exists already!  Let's find it.
(defun strip-comments (str)
  (replace-regexp-in-string "#.*$" "" str))


(defun dp-ess-history-ring-to-buffer-reverse (input-ring)
  (unless (ring-p input-ring)
    (error "The input to 'input-ring' is not a ring."))
  (let*
      ((history-buf (get-buffer-create "*Temp Reverse Input History*"))
       (input-ring-len (ring-length input-ring))
       (index 0))
    (with-current-buffer history-buf
      (erase-buffer)
      (while (< index input-ring-len)
        (insert (ring-ref input-ring index) "\n")
        (setq index (1+ index))))))


(defun dp-ess-history-ring-to-buffer (input-ring)
  (unless (ring-p input-ring)
    (error "The input to 'input-ring' is not a ring."))
  (let*
      ((history-buf (get-buffer-create "*Temp Input History*"))
       (index (ring-length input-ring)))
    (with-current-buffer history-buf
      (erase-buffer)
      (while (> index 0)
        (setq index (1- index))
        (insert (ring-ref input-ring index) "\n\n")))))


(defun dp-ess-create-history-buffer ()
  "Just for debugging right now."
  (dp-ess-history-ring-to-buffer comint-input-ring))
