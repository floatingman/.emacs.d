(use-package js2-mode
  :defer t
  :mode (("\\.js\\'" . js2-mode))
	:config
	(progn
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
    (setq-default
     js-indent-level 2
     js2-basic-offset 2
     ;; Supress js2 mode errors
     js2-mode-show-parse-errors nil
     js2-mode-show-strict-warnings)
    )
	)

(eval-after-load
    'flycheck
  (lambda ()
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    ;; Disable jshint
    (setq-default
     flycheck-disabled-checkers
     (append flycheck-disabled-checkers
             '(javascript-jshint)))))




(use-package js2-refactor
  :defer t
  :config
	(progn
    (add-hook 'js2-mode-hook (lambda () (js2-refactor-mode t)))
    (js2r-add-keybindings-with-prefix "C-c C-m"))
	)

(use-package tern
  :defer t
  :config
	(progn
		(add-hook 'js2-mode-hook (lambda () (tern-mode t)))))



;; kill the tern server if auto reload stops working
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(use-package nodejs-repl 
  :defer t
	)

(use-package skewer-mode
  :defer t
	:config
	(progn
		(require 'skewer-repl)
		(add-hook 'js2-mode-hook 'skewer-mode)
		(add-hook 'css-mode-hook 'skewer-css-mode)
		(add-hook 'html-mode-hook 'skewer-html-mode)))

(defun skewer-start ()
  (interactive)
  (let ((httpd-port 8023))
    (httpd-start)
    (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))

(defun skewer-demo ()
  (interactive)
  (let ((httpd-port 8024))
    (run-skewer)
    (skewer-repl)))

;; enable flycheck for javascript
(add-hook 'js-mode-hook
					(lambda () (flycheck-mode t)))

;;mozrepl needs to be installed in firefox
(use-package moz
  :config
  (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
  (add-hook 'javascript-mode-hook 'javascript-custom-setup)
  (defun javascript-custom-setup ()
    (moz-minor-mode 1))
	)




(use-package json-mode 
  :defer t)                             

(provide 'init-javascript)
