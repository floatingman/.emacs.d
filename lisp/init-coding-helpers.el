(setq-default tab-width 2)

(global-set-key (kbd "RET") 'newline-and-indent)

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

(defun my/recursive-find-file (file &optional directory)
  "Find the first FILE in DIRECTORY or its parents."
  (setq directory (or directory (file-name-directory (buffer-file-name)) (pwd)))
  (if (file-exists-p (expand-file-name file directory))
      (expand-file-name file directory)
    (unless (string= directory "/")
      (my/recursive-find-file file (expand-file-name ".." directory)))))

(defun my/find-tags ()
  "Set the TAGS file."
  (set (make-variable-buffer-local 'tags-table-list) nil)
  (set (make-variable-buffer-local 'tags-file-name)
       (my/recursive-find-file "TAGS")))

(eval-after-load 'drupal-mode
  '(progn
     (add-hook 'drupal-mode-hook 'my/find-tags)))

(provide 'init-coding-helpers)
