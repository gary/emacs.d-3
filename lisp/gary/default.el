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
(set-font-for-host)
