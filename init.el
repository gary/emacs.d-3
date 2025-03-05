;;; init.el --- Initialization for my emacs config

;; Copyright Gary Iams 2016-2018

;; Author: Gary Iams <ge.iams@gmail.com>
;; Created: 4 Feb 2016
;; URL: https://github.com/gary/emacs.d-3/blob/master/init.el

;;; Commentary:

;; Initializes environment and sets up core configuration, bindings,
;; and appearance before loading the rest of the packages I use.

;;; Code:
(defconst emacs-start-time (current-time))

(require 'package)
(require 'xdg)

(setq package-install-upgrade-built-in t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(eval-and-compile
  (define-inline emacs-path (path)
    (expand-file-name path user-emacs-directory)))

(load-theme 'leuven-dark)

(defconst data-directory
  (concat (xdg-data-home) "/emacs")
  "Directory for all portable Emacs data files that persist between restarts.")

(defconst var-directory
  (emacs-path "var/"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package delight                           :defer t)
(use-package use-package-ensure-system-package :defer t)

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(add-hook 'prog-mode-hook #'column-number-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'turn-on-hl-line-mode)
(add-hook 'prog-mode-hook #'turn-on-prettify-symbols-mode)
(add-hook 'prog-mode-hook #'turn-on-save-place-mode)
(add-hook 'prog-mode-hook #'turn-on-visual-line-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)

(delight '((abbrev-mode nil "abbrev")
           (visual-line-mode nil "simple")))

(bind-keys ("<C-SPC>"    . hippie-expand)
           ("<C-return>" . set-mark-command)
           ("<C-tab>"    . bury-buffer)
           ("C-M-z"      . zap-to-char)
           ("C-w"        . backward-kill-word)
           ("C-x \\"     . align-regexp)
           ("M-`"        . other-frame)
           ("M-z"        . zap-up-to-char)
           :map ctl-x-4-map
           ("t" 'toggle-window-split)
           :map mode-specific-map ; \C-c
           ("<tab>" . indent-relative)
           ("k"     . kill-region)
           ("C-u"   . browse-url-at-point)
           ("^"     . top-level)
           ("j"     . delete-indentation)
           ("r"     . revert-buffer)
           :map prog-mode-map
           ("<C-backspace>" . delete-pair))
(find-function-setup-keys)

;; remappings
(define-key (current-global-map) [remap isearch-backward-regexp]
  'isearch-backward)
(define-key (current-global-map) [remap isearch-forward-regexp]
  'isearch-forward)
(define-key (current-global-map) [remap isearch-backward]
  'isearch-backward-regexp)
(define-key (current-global-map) [remap isearch-forward]
  'isearch-forward-regexp)
(define-key (current-global-map) [remap list-buffers] 'ibuffer)
(define-key (current-global-map) [remap open-line] 'vi-open-next-line)
(define-key (current-global-map) [remap query-replace] 'query-replace-regexp)
(define-key (current-global-map) [remap query-replace-regexp]
  'query-replace)
(define-key 'help-command "a" 'apropos)

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; appearance
(add-to-list 'default-frame-alist '(cursor-color . "DeepPink1"))
(add-to-list 'default-frame-alist '(width . 173))
(add-to-list 'default-frame-alist '(height . 81))
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(when window-system
  (tooltip-mode -1)
  (mouse-wheel-mode t))

(fringe-mode 10)
(global-hi-lock-mode t)
(scroll-bar-mode -1)
(set-default 'indicate-empty-lines t)
(set-default 'truncate-lines t)
(setq-default indicate-buffer-boundaries 'left)
(show-paren-mode 1)

(setq inhibit-startup-message t
      initial-scratch-message nil
      split-height-threshold nil) ; force vertical split

;; character encoding
(prefer-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'UTF-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq coding-system-for-write 'utf-8)

;; faces
(set-cursor-color "DeepPink1")

;; file management
(setq abbrev-file-name (concat var-directory "abbrev_defs")
      auto-save-file-name-transforms `((".*" ,data-directory t))
      auto-save-list-file-prefix data-directory
      backup-by-copying-when-linked t
      backup-directory-alist `((".*" . ,data-directory))
      custom-file "custom.el"
      delete-old-versions t
      dired-clean-confirm-killing-deleted-buffers nil
      emacs-lock-default-locking-mode 'kill
      kept-new-versions 16
      kept-old-versions 2
      nsm-settings-file (concat var-directory "network-security.data")
      save-place-file (concat var-directory "places")
      version-control t)

;; miscellaneous
(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))
(put 'narrow-to-region 'disabled nil) ; narrow enabled
(put 'narrow-to-page   'disabled nil)
(put 'upcase-region    'disabled nil) ; change case enabled
(put 'eval-expression  'disabled nil) ; allow eval commands
(set-default 'indent-tabs-mode nil)
(setq-default abbrev-mode t)
(setq async-shell-command-buffer 'rename-buffer
      browse-url-browser-function 'browse-url-default-browser
      mac-command-modifier 'meta ; command is alt in osx
      mac-option-modifier 'alt
      mouse-yank-at-point t
      ring-bell-function 'ignore
      save-abbrevs 'silently
      shift-select-mode nil
      uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'reverse
      uniquify-ignore-buffers-re "^\\*"
      uniquify-separator "|"
      yank-pop-change-selection t)
(delete-selection-mode t) ; replace selection by typing
(icomplete-mode 1)        ; incremental minibuffer completion

(defun init-display ()
  (if (not (daemonp))
      (progn
        (add-hook 'emacs-startup-hook #'turn-off-menu-bar)
        (add-hook 'emacs-startup-hook #'turn-off-tool-bar)
        (add-hook 'emacs-startup-hook '(lambda () (set-font-for-host (selected-frame))))
        (add-hook 'emacs-startup-hook '(lambda () (init-layout (selected-frame))))))
  (add-hook 'before-make-frame-hook #'turn-off-menu-bar)
  (add-hook 'before-make-frame-hook #'turn-off-tool-bar)
  (add-hook 'after-make-frame-functions #'init-layout)
  (add-hook 'after-make-frame-functions #'set-font-for-host))

(defun init-layout (frame)
  (with-selected-frame frame
    (set-frame-position frame 2500 0)))

(if (>= emacs-major-version 29)
    ; (defadvice line-number-mode) ; -> display-line-numbers-mode & add nag
    (defun linum-mode ()
      (interactive)
      (message "display-line-numbers-mode")))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun set-font-for-host (frame)
  (with-selected-frame frame
    (if (featurep 'ns)
        (progn
          (setq ns-antialias-text t
                ns-input-font "Inconsolata"
                ns-input-fontsize 16
                ns-pop-up-frames nil)
          (ns-respond-to-change-font))
      (set-face-attribute 'default nil :family "Inconsolata" :height 120))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun turn-off-menu-bar ()
  (if (functionp 'menu-bar-mode) (menu-bar-mode -1)))

(defun turn-off-tool-bar ()
  (if (functionp 'tool-bar-mode) (tool-bar-mode -1)))

(defun turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun turn-on-save-place-mode ()
  (save-place-mode t))

(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (if (looking-at "^")
      (open-line arg)
    (end-of-line)
    (open-line arg)
    (next-line 1)
    (indent-according-to-mode)))

(defun zap-up-to-char (arg char)
  "Kill up to and excluding ARG'th occurrence of CHAR.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "*p\ncZap up to char: ")
  (kill-region (point)
               (progn
                 (search-forward
                  (char-to-string char) nil nil arg)
                 (progn (goto-char
                         (if (> arg 0) (1- (point)) (1+ (point))))
                        (point)))))

(load (emacs-path "packages"))

(when (and (require 'treesit nil t)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (require 'init-treesitter))

(init-display)
;;; init.el ends here
