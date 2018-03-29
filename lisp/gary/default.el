;;; default.el --- Summary

;;; Commentary:

;;; Code:

(setq browse-url-browser-function 'browse-url-default-macosx-browser
      mac-command-modifier        'meta ; command is alt in osx
      mac-option-modifier         'alt
      ring-bell-function          'ignore
      use-package-always-ensure   t)

(require 'my-aliases)
;; (require 'my-custom)
(require 'my-file-management)
;; (require 'my-misc)
;; (require 'my-minibuffer)
;; (require 'my-mode-mappings)
(load-library "my-packages")
;; (require 'my-registers)
;; (require 'my-utf8)

(provide 'default)
;;; default.el ends here
