(use-package company
  :init
  (setq company-tooltip-align-annotations t)
  (add-hook 'prog-mode-hook 'company-mode)
  )

(provide 'init-completion)
