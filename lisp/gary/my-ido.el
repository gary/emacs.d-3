(ido-mode t)

(flx-ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode)

(setq ido-auto-merge-work-directories-length nil
      ido-confirm-unique-completion t
      ido-create-new-buffer 'always
      ido-default-buffer-method 'other-window
      ido-enable-flex-matching t
      ido-enable-prefix nil
      ido-max-prospects 10
      ido-use-faces nil
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t)

(provide 'my-ido)
