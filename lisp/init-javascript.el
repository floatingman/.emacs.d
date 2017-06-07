(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode))
  :config
  (progn
    (add-hook 'js-mode-hook 'js2-minor-mode)
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
    (add-hook 'js2-mode-hook 'ac-js2-mode)
    (add-hook 'js2-mode-hook (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
    (setq-default
     js-indent-level 2
     js2-basic-offset 2)
    )
  )

(eval-after-load
    'flycheck
  (lambda ()
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    ;; Disable jshint
    (setq-default
     flycheck-disabled-checkers
     (append flycheck-disabled-checkers
             '(javascript-jshint)))))

(use-package js2-refactor
  :ensure t
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (js2-refactor-mode t)))
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (define-key js2-mode-map (kbd "C-k") #'js2r-kill))
	)
(use-package xref-js2
  :ensure t
  :config
  (progn
    (define-key js-mode-map (kbd "M-.") nil)
    )
  )

(use-package tern
  :ensure t
  :config
	(progn
		(add-hook 'js2-mode-hook (lambda () (tern-mode t)))))



;; kill the tern server if auto reload stops working
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(use-package nodejs-repl
  :ensure t
  :defer t
	)

(use-package indium
  :ensure t
  :config
  (progn
    (add-hook 'js-mode-hook #'indium-interaction-mode)))

;; enable flycheck for javascript
(add-hook 'js-mode-hook
					(lambda () (flycheck-mode t)))

(use-package json-mode
  :ensure t
  :defer t)                             
(use-package jade-mode
  :ensure t
  :defer t)

(use-package sws-mode
  :ensure t
  :defer t)

(provide 'init-javascript)
