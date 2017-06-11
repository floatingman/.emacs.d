(defun my/clojure-things-hook ()
  "Set up clojure-y things"
  (eldoc-mode 1)
  (subword-mode t)
  (paredit-mode 1)
  (global-set-key (kbd "C-c t") 'clojure-jump-between-tests-and-code)
  (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
  (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
  (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
  (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns))

(use-package clojure-mode
  :ensure t
  :init
  (add-hook #'clojure-mode-hook #'my/clojure-things-hook))

(defun my/setup-cider ()
  (interactive)
  (setq cider-history-file "~/.nrepl-history"
        cider-hide-special-buffers t
        cider-repl-history-size 10000
        cider-prefer-local-resources t
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-pop-to-buffer-on-connect t)
  (paredit-mode 1)
  (eldoc-mode 1))

(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(use-package cider
  :ensure t
  :defer 30
  :init
  (add-hook #'cider-mode-hook #'my/setup-cider)
  (add-hook #'cider-repl-mode-hook #'my/setup-cider)
  (add-hook #'cider-mode-hook #'my/clojure-things-hook)
  (add-hook #'cider-repl-mode-hook #'my/clojure-things-hook)
  (use-package ac-cider
    :disabled t
    :ensure t
    :init
    (add-hook #'cider-mode-hook #'ac-flyspell-workaround)
    (add-hook #'cider-mode-hook #'ac-cider-setup)
    (add-hook #'cider-repl-mode-hook #'ac-cider-setup)))

(provide 'init-clojure)
