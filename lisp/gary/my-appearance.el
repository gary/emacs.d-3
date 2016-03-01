(setq solarized-distinct-fringe-background t
      solarized-high-contrast-mode-line t)
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

(setq display-time-24-hr-format t
      inhibit-startup-message t
      initial-scratch-message nil
      split-height-threshold nil ; prefer vertically split window layout
      visible-bell nil
      whitespace-style '(face
                         tabs
                         spaces
                         trailing
                         lines
                         space-before-tab
                         newline
                         indentation
                         empty
                         space-after-tab
                         tab-mark))

(provide 'my-appearance)
