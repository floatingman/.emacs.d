(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package paredit-everywhere
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode))


(provide 'init-coding)
