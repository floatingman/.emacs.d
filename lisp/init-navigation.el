(use-package ido
  :demand t
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-virtual-buffers t))

(use-package recentf
  :demand t
  :config
  (recentf-mode 1)
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (setq recentf-max-menu-items 40))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  :ensure t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))

(use-package projectile
  :ensure t
  :init
  (setq-default projectile-mode-line '(:eval (if (file-remote-p default-directory)
						 " Pr"
					       (format " Pr[%s]" (projectile-project-name)))))
  :config
  (projectile-global-mode))


;; Enable move pint from window to window using Shift and the arrow keys
(windmove-default-keybindings)


(provide 'init-navigation)
