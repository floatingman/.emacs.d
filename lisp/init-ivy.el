(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-initial-inputs-alist
                '((man . "^")
                  (woman . "^")))
  ;; IDO-style directory navigation
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (dolist (k '("C-j" "C-RET"))
    (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))

  (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
  (defun sanityinv/enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (use-package flx
      :ensure t)
    (setq-default ivy-re-builders-alist
                  '((t . ivy--regex-fuzzy))))
  (add-hook 'after-init-hook
            (lambda ()
              (when (bound-and-true-p ido-ubiquitous-mode)
                (ido-ubiquitous-mode -1))
              (when (bound-and-true-p ido-mode)
                (ido-mode -1))
              (ivy-mode 1))))

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


(provide 'init-ivy)
