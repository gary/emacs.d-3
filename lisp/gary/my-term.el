(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
        (car l) (last-term-buffer (cdr l)))))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
        (multi-term)
      (switch-to-buffer b))))

(customize-set-variable 'term-bind-key-alist
                        '(("C-a" . term-bol)
                          ("C-c" . term-interrupt-subjob)
                          ("C-p" . term-send-up)
                          ("C-n" . term-send-down)
                          ("C-s" . isearch-forward)
                          ("C-r" . term-send-reverse-search-history)
                          ("C-m" . term-send-raw)
                          ("C-k" . term-send-kill-whole-line)
                          ("C-y" . term-paste)
                          ("C-_" . term-send-raw)
                          ("M-f" . term-send-forward-word)
                          ("M-b" . term-send-backward-word)
                          ("M-K" . term-send-kill-line)
                          ("M-p" . previous-line)
                          ("M-n" . next-line)
                          ("M-y" . yank-pop)
                          ("M-." . term-send-raw-meta)))

(customize-set-variable 'term-unbind-keylist
                        '("C-z" "C-x" "C-c" "C-h" "C-y" "M-y" "<ESC>"))

(provide 'my-term)
