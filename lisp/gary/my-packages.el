;;; my-packages.el --- Summary

;;; Commentary:

;;; Code:

(use-package aggressive-indent
  :defer t
  :delight
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package enh-ruby-mode
  :interpreter "ruby"
  :mode (("\\(\.?\\)Brewfile" . enh-ruby-mode)
         ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package flycheck
  :init
  (add-hook 'flycheck-mode-hook #'toggle-truncate-lines)
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode 1))

(use-package gitconfig-mode :defer t)

(use-package gitignore-mode :defer t)

(use-package ido
  :bind ("C-x M-f" . ido-find-file-other-window)
  :config
  (define-key (current-global-map) [remap find-file-other-window]
    'ido-find-file-other-window)

  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-auto-merge-work-directories-length nil
        ido-confirm-unique-completion t
        ido-create-new-buffer 'always
        ido-default-buffer-method 'selected-window
        ido-enable-flex-matching t
        ido-enable-prefix nil
        ido-max-prospects 10
        ido-save-directory-list-file (f-join var-directory "ido.last")
        ido-use-faces nil
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode))

(use-package inf-ruby
  :disabled) ; bindings are inflexible, load/unload as needed

(use-package kaesar)

(use-package magit
  :bind (:map mode-specific-map
              ("g" . magit-status)))

(use-package magithub
  :disabled
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/src"
        magithub-dir (f-join var-directory "magithub")))

(use-package paredit
  :defer t
  :delight
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :init
  (setq projectile-known-projects-file (f-join var-directory "projectile-bookmarks.eld"))
  :config
  (projectile-mode)
  (setq projectile-cache-file (f-join var-directory "projectile.cache")
        projectile-enable-caching t))

(use-package rbenv
  :init
  (add-hook 'enh-ruby-mode-hook #'rbenv-use-corresponding)
  :config
  (global-rbenv-mode))

(use-package rspec-mode
  :after yasnippet
  :delight
  :hook enh-ruby-mode
  :config
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))
  (ad-activate 'rspec-compile)

  (rspec-install-snippets))

(use-package rubocop
  :delight
  :init
  (add-hook 'enh-ruby-mode-hook #'rubocop-mode))

(use-package solarized-theme
  :init
  (setq solarized-distinct-fringe-background t
        solarized-high-contrast-mode-line t)
  :config
  (load-theme 'solarized-dark t))

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

(use-package with-editor
  :init
  (define-key (current-global-map) [remap async-shell-command]
    'with-editor-async-shell-command)
  (define-key (current-global-map) [remap shell-command]
    'with-editor-shell-command))

(use-package yagist
  :requires kaesar
  :init
  (add-hook 'prog-mode-hook #'yagist-global-minor-mode)
  (bind-keys :prefix-map my-yagist-prefix-map
             :prefix "C-c C-g"
             ("B" . yagist-buffer-private)
             ("G" . yagist-region-or-buffer-private)
             ("R" . yagist-region-private)
             ("b" . yagist-buffer)
             ("g" . yagist-region-or-buffer)
             ("l" . yagist-list)
             ("r" . yagist-region))
  (setq yagist-encrypt-risky-config t
        yagist-view-gist t
        yagist-working-directory "~/src/gists"))

(use-package yasnippet
  :delight yas-minor-mode
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (yas-reload-all))

(use-package yard-mode
  :delight
  :hook enh-ruby-mode)

(provide 'my-packages)
;;; my-packages.el ends here
