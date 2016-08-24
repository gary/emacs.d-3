(with-eval-after-load #'flycheck
  (flycheck-pos-tip-mode))

(add-hook 'flycheck-mode-hook #'visual-line-mode)

(provide 'my-flycheck)
