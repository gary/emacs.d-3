(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(eval-when-compile
  (require 'use-package))

(use-package delight :defer t)
(use-package f       :demand t)

(defconst my-custom-file
  (f-join user-emacs-directory "lisp" user-login-name "my-custom.el"))

(use-package emacs
  :init
  (add-hook 'before-make-frame-hook 'turn-off-tool-bar)
  (add-hook 'emacs-startup-hook 'init-frame)
  :config
  ;; appearance
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
        split-height-threshold nil) ; force vertical split

  (defun init-frame ()
    (set-frame-size (selected-frame) 1155 1325 t)
    (set-frame-position (selected-frame) 1370 0)
    (set-font-for-host))

  (defun set-font-for-host ()
    (setq ns-antialias-text t
          ns-input-font "Inconsolata"
          ns-input-fontsize 14
          ns-pop-up-frames nil)
    (ns-respond-to-change-font))

  (defun turn-off-tool-bar ()
    (if (functionp 'tool-bar-mode) (tool-bar-mode -1))))
