(rbenv-use-global)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(setq enh-ruby-program rbenv-ruby-shim)

(add-hook 'enh-ruby-mode-hook #'rspec-mode)
(add-hook 'enh-ruby-mode-hook #'rubocop-mode)
(add-hook 'enh-ruby-mode-hook #'yard-mode)

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(provide 'my-ruby)
