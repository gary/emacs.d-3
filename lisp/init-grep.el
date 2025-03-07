;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ripgrep
  :bind (:map mode-specific-map
              ("s r" . ripgrep-regexp))
  :ensure-system-package rg)

(provide 'init-grep)
;;; init-grep.el ends here
