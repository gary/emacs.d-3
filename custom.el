(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(kubernetes libgit lsp-mode forge ox-jira dockerfile-mode edit-server groovy-mode json-reformat json-snatcher json-mode kotlin-mode ssh-agency ripgrep powershell coffee-mode hydra harvest ivy dumb-jump xkcd yard-mode yasnippet yaml-mode yagist wgrep-ag web-mode switch-window smex solarized-theme rubocop rspec-mode rbenv projectile paredit multi-term magithub markdown-mode kaesar js2-mode ido-completing-read+ gitignore-mode gitconfig-mode flx-ido flycheck-pos-tip flycheck feature-mode exec-path-from-shell enh-ruby-mode dictionary browse-kill-ring auto-package-update aggressive-indent ag use-package-ensure-system-package delight))
 '(projectile-completion-system 'ivy)
 '(term-bind-key-alist
   '(("C-a" . term-bol)
     ("C-c" . term-interrupt-subjob)
     ("C-p" . term-send-up)
     ("C-n" . term-send-down)
     ("C-s" . isearch-forward)
     ("C-r" . term-send-reverse-search-history)
     ("C-m" . term-send-raw)
     ("C-k" . term-send-kill-whole-line)
     ("C-y" . term-paste)
     ("C-_" . term-send-raw)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-K" . term-send-kill-line)
     ("M-p" . previous-line)
     ("M-n" . next-line)
     ("M-y" . yank-pop)
     ("M-." . term-send-raw-meta)) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "DeepPink1" :foreground "DeepPink4" :inverse-video t)))))

(provide 'my-custom)
