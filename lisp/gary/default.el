(defun set-font-for-host ()
  (setq ns-antialias-text t
        ns-input-font "Inconsolata"
        ns-input-fontsize 14
        ns-pop-up-frames nil)
  (ns-respond-to-change-font))

;; command is alt in osx
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'alt))

(exec-path-from-shell-initialize)

(global-set-key (kbd "M-`") 'other-frame)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(set-font-for-host)

(require 'my-aliases)
(require 'my-appearance)
(require 'my-custom)
(require 'my-file-management)
(require 'my-functions)
(require 'my-global-bindings)
(require 'my-ido)
(require 'my-key-chords)
(require 'my-misc)
(require 'my-mode-mappings)
(require 'my-ruby)
