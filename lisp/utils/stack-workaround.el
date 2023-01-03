;;; lisp/utils/stack-workaround.el -*- lexical-binding: t; -*-

(defun run-stack-workaround ()
  "Mimics `run-haskell' but using a workaround to get stack to work.

Temporary workaround to get around a stack segfault issue.
- https://github.com/haskell/haskell-mode/issues/1794
- https://github.com/commercialhaskell/stack/issues/5607
- https://github.com/commercialhaskell/stack/issues/5607#issuecomment-1220789279"
  (interactive)
  (let ((proc (inferior-haskell-process--stack-workaround)))
    (pop-to-buffer-same-window (process-buffer proc))))

(defun inferior-haskell-process--stack-workaround ()
  "Restart if not present."
  (cond ((and (buffer-live-p inferior-haskell-buffer)
              (comint-check-proc inferior-haskell-buffer))
         (get-buffer-process inferior-haskell-buffer))
        (t (inferior-haskell-start-process--stack-workaround)
           (inferior-haskell-process))))

(defun inferior-haskell-start-process--stack-workaround ()
  "Mimics `inferior-haskell-start-process' but using a workaround to get Stack to work."
  (let* ((command (haskell-program-name-with-args))
         (args-string (mapconcat #'shell-quote-argument command " ")))
    (when inferior-haskell-root-dir
      (setq default-directory inferior-haskell-root-dir))
    (setq inferior-haskell-buffer
          (make-comint "haskell" "bash" nil "-c" args-string))
    (with-current-buffer inferior-haskell-buffer
      (inferior-haskell-mode)
      (run-hooks 'inferior-haskell-hook))))
