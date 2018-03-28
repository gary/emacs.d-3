(mapc (lambda (auto-mode)
        (add-to-list 'auto-mode-alist auto-mode))
      '(("Cask$"             . lisp-interaction-mode)
        ("hosts$"            . conf-mode)
        ("\\.zsh$"           . shell-script-mode)))

(provide 'my-mode-mappings)
