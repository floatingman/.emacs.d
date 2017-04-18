(use-package deft
  :ensure t
  :bind ("C-x t" . deft)
  :config
  (setq deft-extension '("org" "txt" "tex" "text" "md")
        deft-directory "~/personal/org/deft/"
        deft-recursive t
        deft-use-filter-string-for-filename t 
        deft-text-mode 'org-mode))

(provide 'init-deft)
