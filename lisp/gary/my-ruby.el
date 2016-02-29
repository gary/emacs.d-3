(rbenv-use-global)

(setq enh-ruby-program rbenv-ruby-shim)

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(provide 'my-ruby)
