(use-package diff-hl
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'diff-hl-mode)
  (after-load 'diff-hl
    (define-key diff-hl-mode-map
      (kbd "<left-fringe> <mouse-1>")
      'diff-hl-diff-goto-hunk)))

(use-package browse-at-remote
  :ensure t)

(provide 'init-vc)
