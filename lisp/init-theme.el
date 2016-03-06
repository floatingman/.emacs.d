(use-package color-theme :ensure t)
(use-package lush-theme  :ensure t)
(use-package color-theme-solarized :ensure t)

(use-package powerline
  :ensure t
  :config
  (progn (powerline-default-theme)
         (setq powerline-default-separator 'slant)))


(defun my/setup-color-theme()
  (interactive)
  (lush-theme))

(eval-after-load 'color-theme
  '(when window-system
    (my/setup-color-theme)))

(provide 'init-theme)
