

(use-package rainbow-delimiters :ensure t)

(column-number-mode 1)

;;Projects
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node-modules"))
  :config
  (projectile-global-mode))
(use-package helm-projectile
   :defer t :ensure t
   :ensure helm-projectile)

(provide 'init-coding-helpers)
