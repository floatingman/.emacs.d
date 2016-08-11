(use-package deft
  :ensure t
  :bind ("C-x t" . deft)
  :config
  (setq deft-extension "org"
        deft-directory "~/personal/org/deft/"
        deft-text-mode 'org-mode))

(provide 'init-deft)
