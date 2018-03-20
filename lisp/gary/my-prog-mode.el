(defun turn-off-electric-indent-mode ()
  (electric-indent-mode -1))

(defun turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun turn-on-save-place-mode ()
  (require 'saveplace)
  (setq-default save-place t))

(add-hook 'prog-mode-hook #'column-number-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'turn-off-electric-indent-mode)
(add-hook 'prog-mode-hook #'turn-on-hl-line-mode)
(add-hook 'prog-mode-hook #'turn-on-prettify-symbols-mode)
(add-hook 'prog-mode-hook #'turn-on-save-place-mode)
(add-hook 'prog-mode-hook #'turn-on-visual-line-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'yagist-global-minor-mode)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(define-key prog-mode-map (kbd "C-<backspace>") #'delete-pair)

(provide 'my-prog-mode)
