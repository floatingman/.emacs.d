(use-package ruby-refactor
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)))

(use-package ruby-mode
  :ensure t
  :defer t
  :mode ("Rakefile\\'"
         "\\.rake\\'"
         "\\.rxml\\'"
         "\\.rjs\\'"
         ".irbrc\\'"
         "\\.builder\\'"
         "\\.ru\\'"
         "\\.gemspec\\'"
         "Gemfile\\'"
         "Kirkfile\\'"
         "Capfile\\'"
         "Guardfile\\'"))

(use-package ruby-hash-syntax
  :ensure t
  :defer t)

(use-package yari
  :ensure t
  :defer t)

(use-package ruby-compilation
  :ensure t
  :defer t)

(use-package inf-ruby
  :ensure t
  :defer t)

(use-package robe
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)))

(use-package rinari
  :ensure t
  :defer t
  :config
  (progn
    (setq ruby-insert-encoding-magic-comment nil)
    (global-rinari-mode)))

(use-package rhtml-mode
  :ensure t
  :defer t)

(provide 'init-ruby)
