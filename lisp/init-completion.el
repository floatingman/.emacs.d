(use-package company
  :defer 2
  :bind ("<C-tab>" . company-complete)
  :config
  (add-hook 'after-init-hook 'global-company-mode))




(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :init (progn
          (yas-global-mode 1)
          (yas-reload-all)))

;;use abbrev to correct misspellings
(use-package abbrev
  :disabled t
  :diminish abbrev-mode
  :init (add-hook 'prog-mode-hook #'abbrev-mode)
  :defer 30
  :config
  (progn
    (define-key ctl-x-map "\M-a" 'my/ispell-word-then-abbrev)

    (defun my/ispell-word-then-abbrev (p)
      "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
      (interactive "P")
      (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
        (call-interactively 'ispell-word)
        (setq aft (downcase (or (thing-at-point 'word) "")))
        (unless (string= aft bef)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob"))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft))))

    (setq save-abbrevs t)
    (setq-default abbrev-mode t)))

;; excludes very large buffers from dabbrev
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

(provide 'init-completion)
