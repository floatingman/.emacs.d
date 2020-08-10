(use-package dired
  :config
  (use-package diff-hl
    :ensure t
    :config
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode
     ))
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (add-hook 'dired-mode-hook (lambda ()
			      (guide-key/add-local-guide-key-sequence "%"))))


(provide 'init-dired)
