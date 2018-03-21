;; Third party functionality

(define-key ctl-x-map "\C-i" #'imenu-anywhere)

(global-set-key (kbd "C-c a") #'ag)
(global-set-key (kbd "C-c l")   'dictionary-lookup-definition)
(global-set-key (kbd "M-x") #'endless/ispell-word-then-abbrev)
(global-set-key (kbd "M-^") #'endless/ispell-word-then-abbrev)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-g b r") #'yagist-buffer-private)
(global-set-key (kbd "C-c C-g b") #'yagist-buffer)
(global-set-key (kbd "C-c C-g l") #'yagist-list)
(global-set-key (kbd "C-c C-g r p") #'yagist-region-private)
(global-set-key (kbd "C-c C-g r") #'yagist-region)
(global-set-key (kbd "C-c C-g g p") #'yagist-region-or-buffer-private)
(global-set-key (kbd "C-c C-g g") #'yagist-region-or-buffer)

;; Builtin functionality

(define-key 'help-command "a" 'apropos)

(define-key (current-global-map) [remap isearch-backward-regexp]
  'isearch-backward)
(define-key (current-global-map) [remap isearch-forward-regexp]
  'isearch-forward)
(define-key (current-global-map) [remap isearch-backward]
  'isearch-backward-regexp)
(define-key (current-global-map) [remap isearch-forward]
  'isearch-forward-regexp)
(define-key (current-global-map) [remap list-buffers] 'ibuffer)
(define-key (current-global-map) [remap open-line] 'vi-open-next-line)
(define-key (current-global-map) [remap query-replace] 'query-replace-regexp)
(define-key (current-global-map) [remap query-replace-regexp]
  'query-replace)
(define-key (current-global-map) [remap other-window] 'switch-window)

(define-key ctl-x-map "\\" 'align-regexp)
(define-key ctl-x-map "\C-k" 'kill-region)

(global-set-key   (kbd "C-w")           'backward-kill-word)
(global-set-key   (kbd "M-y")           'browse-kill-ring)
(global-set-key   (kbd "C-c C-u")       'browse-url-at-point)
(global-set-key   (kbd "<C-tab>")       'bury-buffer)
(global-set-key   (kbd "C-c j")         'delete-indentation)
(global-set-key   (kbd "<C-SPC>")       'hippie-expand)
(global-set-key   (kbd "C-c <tab>")     'indent-relative)
(global-set-key   (kbd "C-<f10>")       'menu-bar-mode)
(global-set-key   (kbd "C-c r")         'revert-buffer)
(global-set-key   (kbd "<C-return>")    'set-mark-command)
(global-set-key   (kbd "C-c t")         'get-term)
(global-set-key   (kbd "C-c T")         'multi-term-dedicated-toggle)
(global-set-key   (kbd "C-=")           'text-scale-decrease)
(global-set-key   (kbd "C-+")           'text-scale-increase)
(global-set-key   (kbd "C-c ^")         'top-level)
(global-set-key   (kbd "C-M-z")         'zap-to-char)
(global-set-key   (kbd "M-z")           'zap-up-to-char)
(global-set-key   (kbd "<f7>")          '(lambda ()
                                           (interactive)
                                           (modify-frame-parameters nil `((alpha . 100)))))
(global-set-key   (kbd "<f5>")          '(lambda () ; decrease
                                           (interactive)
                                           (opacity-modify t)))
(global-set-key   (kbd "<f6>")          '(lambda () ; increase
                                           (interactive)
                                           (opacity-modify)))

(global-unset-key (kbd "C-x C-d")) ; too close to dired
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

(provide 'my-global-bindings)
