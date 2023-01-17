;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(startup-redirect-eln-cache (concat user-emacs-directory "var/" "eln-cache"))

(provide 'early-init)

;;; early-init.el ends here
