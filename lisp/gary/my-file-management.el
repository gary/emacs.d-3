(setq backup-by-copying-when-linked t
      delete-old-versions t
      kept-new-versions 16
      kept-old-versions 2
      version-control t)

(let ((emacs-tmp-dir (format "%s%s" temporary-file-directory "org.gnu.emacs")))
  (setq abbrev-file-name (f-join var-directory "abbrev_defs")
        auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
        auto-save-list-file-prefix   emacs-tmp-dir
        backup-directory-alist       `((".*" . ,emacs-tmp-dir))
        bookmark-default-file        (f-join var-directory "emacs.bmk")
        custom-file                  my-custom-file
        emacs-download-dir           (f-join var-directory "downloads/")
        recentf-save-file            (f-join var-directory "recentf")
        save-place-file              (f-join var-directory "places")
        slime-repl-history-file      (f-join var-directory "slime-history.eld")
        tramp-persistency-file-name  (f-join var-directory "tramp")
        ))

(provide 'my-file-management)
