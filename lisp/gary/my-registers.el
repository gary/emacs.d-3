;; Use C-x r j followed by the letter of the register to jump to it.

(let ((my-config-dir (f-dirname (f-this-file))))
  (dolist (r `((?b (file . ,(f-join my-config-dir "my-global-bindings.el")))
               (?e (file . ,(f-join my-config-dir "default.el")))
               (?d (file . ,(f-canonical "~/.dotfiles/.zshrc")))
               (?m (file . ,(f-join my-config-dir "my-misc.el")))
               (?r (file . ,(f-join my-config-dir "my-registers.el")))
               (?z (file . ,(f-canonical "~/.oh-my-custom/common-aliases.zsh"))))
    (set-register (car r) (cadr r)))))

(provide 'my-registers)
