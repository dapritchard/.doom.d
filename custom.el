(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (haskell-process-args-cabal-repl "--ghc-option=-ferror-spans" "fact-models")
     (haskell-process-args-cabal-repl "--ghc-option=-ferror-spans" "fact-models-test")
     (haskell-process-args-cabal-repl "hasklepias-core")
     (haskell-process-args-stack-ghci "validating-ciphers")
     (haskell-process-args-stack-ghci ":validating-ciphers")
     (haskell-process-args-stack-ghci ":ciphers-test")
     (haskell-process-args-stack-ghci ":hangman-app")
     (haskell-process-args-stack-ghci ":test-hangman-testing")
     (haskell-process-args-stack-ghci ":hangman-testing-exe")
     (haskell-process-args-stack-ghci ":test-make-a-gen-random-generator")
     (haskell-process-args-stack-ghci ":test-idempotence")
     (haskell-process-args-stack-ghci ":test-failure")
     (haskell-process-args-cabal-repl "--ghc-option=-ferror-spans" "hasklepias-main")
     (haskell-process-type . stack-ghci)
     (haskell-process-args-stack-ghci ":test-using-quickcheck")
     (haskell-process-type quote stack-ghci)
     (haskell-process-args-cabal-repl "--ghc-option=-ferror-spans" "fact-models-internal")
     (haskell-process-args-cabal-repl "fact-models:lib:fact-models")
     (haskell-process-args-cabal-repl "hasklepias-main")
     (TeX-master . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
