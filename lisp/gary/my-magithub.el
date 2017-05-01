(require 'magithub)

(setq magithub-feature-autoinject t)
(setq magithub-features '((pull-request-merge . t) (pull-request-checkout t)))

(provide 'my-magithub)
