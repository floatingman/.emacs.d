
(provide 'init-ivy)

(use-package ivy
  :ensure t
  :demand t
  :diminish (ivy-mode . "")
  :bind
  (("C-M-z" . ivy-resume)
   ("C-x C-r" . ivy-switch-buffer))

  :init
  (ivy-mode 1)
  :config
  (bind-key "C-s" 'swiper)
    (setq-default ivy-use-virtual-buffers t
              ivy-virtual-abbreviate 'fullpath
              ivy-count-format "%d/%d "
              projectile-completion-system 'ivy
              ivy-initial-inputs-alist
              '((man . "^")
                (woman . "^")))
    (use-package counsel-ag)
    (use-package counsel-projectile
      :ensure t
      :init
      (counsel-projectile-on)))

(use-package ivy-historian
  :ensure t
  :config
  (add-hook 'after-init-hook (lambda ()
                               (ivy-historian-mode t))))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :config
  (setq-default counsel-mode-override-describe-bindings t)
  (add-hook 'after-init-hook 'counsel-mode))

(when (executable-find "ag")
  (defun sanityinc/counsel-ag-project (inital-input)
    "Search using `counsel-ag' from the project root for INITIAL-INPUT."
    (interactive (list (thing-at-point 'symbol)))
    (counsel-ag inital-input (condition-case err
                                 (projectile-project-root)
                               (error default-directory))))
  (global-set-key (kbd "M-?")
                   'sanityinc/counsel-ag-project))

(use-package swiper
  :ensure t
  :config
  (defun sanityinc/swiper-at-point (sym)
    "Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))
  (define-key ivy-mode-map (kbd "M-s /")
    'sanityinc/swiper-at-point))
