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

(use-package delight                           :defer t)
(use-package f                                 :demand t)
(use-package use-package-ensure-system-package :defer t)

(defconst my-custom-file
  (f-join user-emacs-directory "lisp" user-login-name "my-custom.el"))

(defconst tmp-directory
  (f-join temporary-file-directory "org.gnu.emacs"))

(defconst var-directory
  (f-join user-emacs-directory "var"))

(use-package emacs
  :init
  (add-hook 'before-make-frame-hook 'turn-off-tool-bar)
  (add-hook 'emacs-startup-hook 'init-frame)
  (add-hook 'prog-mode-hook #'electric-pair-mode)
  (add-hook 'prog-mode-hook #'turn-on-hl-line-mode)
  (add-hook 'prog-mode-hook #'turn-on-prettify-symbols-mode)
  (add-hook 'prog-mode-hook #'turn-on-save-place-mode)
  (add-hook 'prog-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'prog-mode-hook #'whitespace-mode)
  (bind-keys ((kbd "<f5>") . increase-opacity)
             ((kbd "<f6>") . decrease-opacity))
  :bind (("<C-SPC>"    . hippie-expand)
         ("<C-return>" . set-mark-command)
         ("<C-tab>"    . bury-buffer)
         ("C-+"        . text-scale-decrease)
         ("C-="        . text-scale-increase)
         ("C-M-z"      . zap-to-char)
         ("C-w"        . backward-kill-word)
         ("C-x \\"     . align-regexp)
         ("M-`"        . other-frame)
         ("M-z"        . zap-up-to-char)
         :map mode-specific-map ; \C-c
         ("<tab>" . indent-relative)
         ("k"     . kill-region)
         ("C-u"   . browse-url-at-point)
         ("^"     . top-level)
         ("j"     . delete-indentation)
         ("r"     . revert-buffer)
         :map prog-mode-map
         ("<C-backspace>" . delete-pair))
  :delight
  (abbrev-mode)
  (visual-line-mode)
  :config
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

  ;; file management
  (setq abbrev-file-name (f-join var-directory "abbrev_defs")
        auto-save-file-name-transforms `((".*" ,tmp-directory t))
        auto-save-list-file-prefix tmp-directory
        backup-by-copying-when-linked t
        backup-directory-alist `((".*" . ,tmp-directory))
        custom-file my-custom-file
        delete-old-versions t
        kept-new-versions 16
        kept-old-versions 2
        version-control t)
  (load custom-file)

  (defun decrease-opacity ()
    (interactive)
    (opacity-modify))

  (defun increase-opacity ()
    (interactive)
    (opacity-modify t))

  (defun init-frame ()
    (set-frame-size (selected-frame) 1155 1325 t)
    (set-frame-position (selected-frame) 1370 0)
    (set-font-for-host))

  (defun opacity-modify (&optional dec)
    "Modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
    (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
           (oldalpha (if alpha-or-nil alpha-or-nil 100))
           (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
      (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
        (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

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

  (defun set-font-for-host ()
    (setq ns-antialias-text t
          ns-input-font "Inconsolata"
          ns-input-fontsize 14
          ns-pop-up-frames nil)
    (ns-respond-to-change-font))

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
                          (point))))))
