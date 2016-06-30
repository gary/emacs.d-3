(rbenv-use-global)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(defconst ruby--prettify-symbols-alist
  '(("lambda"  . ?λ) ("->" . ?λ)))

(setq enh-ruby-program rbenv-ruby-shim)
(setq-local prettify-symbols-alist ruby--prettify-symbols-alist)

(add-hook 'enh-ruby-mode-hook #'rspec-mode)
(add-hook 'enh-ruby-mode-hook #'rubocop-mode)
(add-hook 'enh-ruby-mode-hook #'yard-mode)

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(provide 'my-ruby)
