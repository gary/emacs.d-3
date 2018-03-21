;;; default.el --- Summary

;;; Commentary:

;;; Code:

(global-set-key (kbd "M-`") 'other-frame)

(setq browse-url-browser-function 'browse-url-default-macosx-browser
      mac-command-modifier        'meta ; command is alt in osx
      mac-option-modifier         'alt
      ring-bell-function          'ignore
      use-package-always-ensure   t)

(require 'my-aliases)
;; (require 'my-custom)
(require 'my-file-management)
;; (require 'my-functions)
;; (require 'my-global-bindings)
;; (require 'my-misc)
;; (require 'my-minibuffer)
;; (require 'my-mode-mappings)
(load-library "my-packages")
;; (require 'my-prog-mode)
;; (require 'my-projectile)
;; (require 'my-registers)
;; (require 'my-ruby)
;; (require 'my-term)
;; (require 'my-text-mode)
;; (require 'my-utf8)
;; (require 'my-web-mode)
;; (require 'my-yagist)
;; (require 'my-yasnippet)

(provide 'default)
;;; default.el ends here
