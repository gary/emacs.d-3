(use-package solarized-theme
  :init
  (setq solarized-distinct-fringe-background t
        solarized-high-contrast-mode-line t)
  :config
  (defun init-frame ()
    (set-frame-size (selected-frame) 1155 1325 t)
    (set-frame-position (selected-frame) 1370 0)
    (set-font-for-host))
  (add-hook 'emacs-startup-hook 'init-frame)
  (load-theme 'solarized-dark t)
  (add-hook 'before-make-frame-hook 'turn-off-tool-bar)
  (when window-system
    (setq frame-title-format '(buffer-file-name "%f" ("%b")))
    (tooltip-mode -1)
    (mouse-wheel-mode t))

  (fringe-mode 10)
  (global-hi-lock-mode t)
  (scroll-bar-mode -1)
  (set-cursor-color "deeppink")
  (set-default 'indicate-empty-lines t)
  (set-default 'truncate-lines t)
  (setq-default indicate-buffer-boundaries 'left)
  (show-paren-mode 1)

  (setq inhibit-startup-message t
        initial-scratch-message nil
        split-height-threshold nil ; prefer vertically split window layout
        ))

(use-package whitespace
  :delight
  :config
  (setq whitespace-style '(face
                           tabs
                           spaces
                           trailing
                           lines
                           space-before-tab
                           newline
                           indentation
                           empty
                           space-after-tab
                           tab-mark)))

(provide 'my-appearance)
