(defun set-font-for-host ()
  (setq ns-antialias-text t
        ns-input-font "Inconsolata"
        ns-input-fontsize 14
        ns-pop-up-frames nil)
  (ns-respond-to-change-font))

(exec-path-from-shell-initialize)

(global-set-key (kbd "M-`") 'other-frame)

(setq browse-url-browser-function 'browse-url-default-macosx-browser
      mac-command-modifier        'meta ; command is alt in osx
      mac-option-modifier         'alt
      ring-bell-function          'ignore)

(set-font-for-host)

(require 'my-aliases)
(require 'my-appearance)
(require 'my-custom)
(require 'my-file-management)
(require 'my-functions)
(require 'my-global-bindings)
(require 'my-ido)
(require 'my-key-chords)
(require 'my-lisp)
(require 'my-misc)
(require 'my-mode-mappings)
(require 'my-prog-mode)
(require 'my-projectile)
(require 'my-registers)
(require 'my-ruby)
(require 'my-term)
(require 'my-text-mode)
(require 'my-utf8)
(require 'my-yasnippet)
