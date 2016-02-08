(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

(put 'narrow-to-region 'disabled nil)   ; narrow enabled
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)      ; change case enabled
(put 'eval-expression 'disabled nil)    ; allow eval commands

(set-default 'imenu-auto-rescan t)
(set-default 'indent-tabs-mode nil)

(setq diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain ; unified controls
      ffap-require-prefix t
      minibuffer-max-depth nil
      mouse-yank-at-point t
      shift-select-mode nil
      uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'reverse
      uniquify-ignore-buffers-re "^\\*"
      uniquify-separator "|"
      yank-pop-change-selection t
      )

(delete-selection-mode t) ; replace selection by typing
(icomplete-mode 1)        ; incremental minibuffer completion

(provide 'my-misc)
