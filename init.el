(package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(load-library "pallet")
(pallet-mode t)

(defconst my-custom-file
      (f-join user-emacs-directory "lisp" user-login-name "my-custom.el"))

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
