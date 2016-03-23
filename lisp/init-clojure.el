(defun my/clojure-things-hook ()
  "Set up clojure-y things"
  (eldoc-mode 1)
  (subword-mode t)
  (paredit-mode 1)
  (global-set-key (kbd "C-c t") 'clojure-jump-between-tests-and-code))

(use-package clojure-mode
  :init
  (add-hook #'clojure-mode-hook #'my/clojure-things-hook))

(defun my/setup-cider ()
  (interactive)
  setq cider-history-file "~/.nrepl-history"
  cider-hide-special-buffers t
  cider-repl-history-size 10000
  cider-prefer-local-resources t
  cider-popup-stacktraces-in-repl t
  (paredit-mode 1)
  (eldoc-mode 1))

(use-package cider
  :defer 30
  :init
  (add-hook #'cider-mode-hook #'my/setup-cider)
  (add-hook #'cider-repl-mode-hook #'my/setup-cider)
  (add-hook #'cider-mode-hook #'my/clojure-things-hook)
  (add-hook #'cider-repl-mode-hook #'my/clojure-things-hook)
  (use-package ac-cider
    :init
    (add-hook #'cider-mode-hook #'ac-flyspell-workaround)
    (add-hook #'cider-mode-hook #'ac-cider-setup)
    (add-hook #'cider-repl-mode-hook #'ac-cider-setup)))

(provide 'init-clojure)
