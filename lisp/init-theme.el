(use-package color-theme :ensure t)
(use-package lush-theme  :ensure t)
(use-package color-theme-solarized :ensure t)

(defun my/setup-color-theme()
  (interactive)
  (lush-theme))

(eval-after-load 'color-theme
  '(when window-system
    (my/setup-color-theme)))

(provide 'init-theme)
