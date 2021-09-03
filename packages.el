;;; my-packages.el --- Summary

;;; Commentary:

;; Bootstraps all the packages I use, configuring most of them to be
;; lazily loaded.

;;; Code:

(use-package ag
  :bind (:map mode-specific-map
              ("s a" . ag-regexp))
  :ensure-system-package ag)

(use-package aggressive-indent
  :defer t
  :delight
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package auto-package-update
  :disabled
  :init
  (setq auto-package-update-last-update-day-filename (concat var-directory "last-package-update-day"))
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package autorevert
  :delight auto-revert-mode)

(use-package bookmark
  :config
  (setq bookmark-default-file (concat var-directory "emacs.bmk")))

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

(use-package coffee-mode)

(use-package dictionary
  :init
  (bind-keys :prefix-map my-dictionary-prefix-map
             :prefix "C-c C-w"
             ("l" . dictionary-lookup-definition)
             ("m" . dictionary-match-words)
             ("s" . dictionary-search)))

(use-package diff
  :config
  (setq diff-switches "-u"))

(use-package dockerfile-mode
  :config
  (put 'dockerfile-image-name 'safe-local-varible #'stringp))

(use-package dumb-jump
  :requires ivy
  :init
  (bind-keys :prefix-map my-dumb-jump-prefix-map
             :prefix "M-g"
             ("o" . dumb-jump-go-other-window)
             ("j" . dumb-jump-go)
             ("i" . dumb-jump-go-prompt)
             ("x" . dumb-jump-go-prefer-external)
             ("z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  :ensure-system-package ag)

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init (if after-init-time
            (edit-server-start)
          (add-hook 'after-init-hook
                    #'(lambda() (edit-server-start))))
  :config (setq edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t)
                  (window-system . x))))

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

(use-package feature-mode :defer t)

(use-package ffap
  :config
  (setq ffap-require-prefix t))

(use-package flycheck
  :init
  (add-hook 'flycheck-mode-hook #'toggle-truncate-lines)
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(use-package flyspell
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :delight)

(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode 1))

(use-package forge
  :if (memq window-system '(mac ns x))
  :after magit
  :init
  (setq forge-database-file (concat var-directory "forge-database.sqlite")))

(use-package gitconfig-mode :defer t)

(use-package gitignore-mode :defer t)

(use-package groovy-mode)

(use-package harvest
  :disabled
  :requires (org hydra ivy)
  :init
  (add-hook 'org-clock-in-hook #'harvest)
  (add-hook 'org-clock-out-hook #'harvest-clock-out))

(use-package hcl-mode)

(use-package hydra
  :disabled)

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
        ido-save-directory-list-file (concat var-directory "ido.last")
        ido-use-faces nil
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode))

(use-package inf-ruby
  :disabled) ; bindings are inflexible, load/unload as needed

(use-package ispell
  :ensure-system-package aspell
  :init
  (defun endless/ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (if (setq bef (thing-at-point 'word))
                   ;; Word was corrected or used quit.
                   (if (ispell-word nil 'quiet)
                       nil ; End the loop.
                     ;; Also end if we reach `bob'.
                     (not (bobp)))
                 ;; If there's no word at point, keep looking
                 ;; until `bob'.
                 (not (bobp)))
          (backward-word))
        (setq aft (thing-at-point 'word)))
      (if (and aft bef (not (equal aft bef)))
          (let ((aft (downcase aft))
                (bef (downcase bef)))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob")))
        (user-error "No typo at or before point"))))
  :bind (("M-x" . endless/ispell-word-then-abbrev)
         ("M-^" . ispell-comments-and-strings))
  :config
  (setq ispell-personal-dictionary (concat var-directory "ispell_english")))

(use-package ivy)  ; TODO: evaluate in full later

(use-package json-mode)

(use-package json-reformat)

(use-package json-snatcher)

(use-package js2-mode
  :ensure-system-package node
  :interpreter "node"
  :mode "\\.js\\'")

(use-package kaesar)

(use-package kotlin-mode)

(use-package lsp-mode
  :defer t
  :init
  (setq lsp-session-file (concat var-directory "lsp-session-v1")))

(use-package markdown-mode
  :mode "\\.mdwn\\'" "\\.mdtxt\\'" "\\.mkd\\'" "\\.mkdn\\'"
  :ensure-system-package markdown)

(use-package magit)

(use-package midnight
  :config
  (add-to-list 'clean-buffer-list-kill-never-buffer-names "*msg*")
  (add-to-list 'clean-buffer-list-kill-never-regexps "^\\*shell-")
  (append clean-buffer-list-kill-buffer-names '("*Shell Command Output*"
                                                "*Completions*"
                                                "*Occur*"
                                                "*Bookmark List*"
                                                "*Ediff Registry*"
                                                "*ag search"
                                                "*markdown-output*"))
  (add-to-list 'clean-buffer-list-kill-regexps "\\.el\\'")
  (add-to-list 'clean-buffer-list-kill-regexps "\\.js\\'")
  (add-to-list 'clean-buffer-list-kill-regexps "\\.rb\\'")
  (midnight-delay-set 'midnight-delay 4400))

(use-package multi-term
  :bind (:map mode-specific-map
              ("t" . get-term)
              ("T" . multi-term-dedicated-toggle))
  :config
  (defun last-term-buffer (l)
    "Return most recently used term buffer."
    (when l
      (if (eq 'term-mode (with-current-buffer (car l) major-mode))
          (car l) (last-term-buffer (cdr l)))))

  (defun get-term ()
    "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
    (interactive)
    (let ((b (last-term-buffer (buffer-list))))
      (if (or (not b) (eq 'term-mode major-mode))
          (multi-term)
        (switch-to-buffer b))))
  :custom
  term-bind-key-alist '(("C-a" . term-bol)
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
                        ("M-." . term-send-raw-meta))
  term-unbind-key-list '("C-z" "C-x" "C-c" "C-h" "C-y" "M-y" "<ESC>"))

(use-package org
  :init
  :config
  (setq org-directory "~/var/org/"
        org-agenda-files (file-expand-wildcards (concat org-directory "**/*.org"))))

(use-package ox-jira)

(use-package paredit
  :defer t
  :delight
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package powershell
  :if (memq window-system '(pc w32)))

(use-package projectile
  :requires ivy
  :delight '(:eval (concat " " (projectile-project-name)))
  :init
  (setq projectile-known-projects-file (concat var-directory "projectile-bookmarks.eld"))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-cache-file (concat var-directory "projectile.cache")
        projectile-completion-system 'ivy
        projectile-enable-caching t))

(use-package recentf
  :config
  (setq recentf-save-file (concat var-directory "recentf")))

(use-package rbenv
  :init
  (add-hook 'enh-ruby-mode-hook #'rbenv-use-corresponding)
  :config
  (global-rbenv-mode))

(use-package ripgrep
  :bind (:map mode-specific-map
              ("s r" . ripgrep-regexp))
  :ensure-system-package rg)

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

(use-package rustic
  :requires lsp-mode)

(use-package saveplace
  :config
  (setq save-place-file (concat var-directory "places")))

(use-package solarized-theme
  :init
  (setq solarized-distinct-fringe-background t
        solarized-high-contrast-mode-line t)
  :config
  (load-theme 'solarized-dark t))

(use-package smex
  :bind (("C-x C-m" . smex)
         :map mode-specific-map
         ("C-m" . smex-major-mode-commands))
  :config
  (setq smex-save-file (concat var-directory "smex-items")))

(use-package ssh-agency
  :if (memq window-system '(pc w32))
  :config
  (setenv "SSH_ASKPASS" "git-gui--askpass")
  :after magit)

(use-package switch-window
  :defer t
  :init
  (define-key (current-global-map) [remap other-window] 'switch-window))

(use-package tramp
  :config
  (setq tramp-persistency-file-name  (concat var-directory "tramp")))

(use-package transient
  :requires magit
  :config
  (setq transient-history-file (concat var-directory "transient/history.el")
        transient-values-file (concat var-directory "transient/values.el")
        transient-levels-file (concat var-directory "transient/levels.el")))

(use-package web-mode
  :config
  (setq web-mode-engines-alist '(("erb" . "\\.erb\\'")))
  :mode "\\html?\\'" "\\.erb\\'")

(use-package wgrep-ag
  :hook (ag-mode-hook . wgrep-ag-setup))

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

(use-package yaml-mode :defer t)

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

(use-package xkcd
  :config
  (setq xkcd-cache-dir    (concat var-directory "xkcd/")
        xkcd-cache-latest (concat xkcd-cache-dir "latest")))

(provide 'my-packages)
;;; packages.el ends here
