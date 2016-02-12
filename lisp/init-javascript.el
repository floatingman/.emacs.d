(use-package js3-mode
	:ensure t
	:defer t
	:config
	(progn
		(setq js3-auto-indent-p t
					js3-curly-indent-offset 0
					js3-enter-indents-newline t
					js3-expr-indent-offset 2
					js3-indent-on-enter-key t
					js3-lazy-commas t
					js3-lazy-dots t
					js3-lazy-operators t
					js3-paren-indent-offset 2
					js3-square-indent-offset 4)
		(linum-mode 1)))

(use-package js2-mode
	:ensure t
	:defer t
	:config
	(progn
		(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
		(setq js2-bounce-indent-p t)))


(use-package tern
	:ensure t
	:defer t)
(add-hook 'js3-mode-hook (lambda () (tern-mode t)))
(use-package company-tern
	:ensure t
	:defer t)

(use-package nodejs-repl
	:ensure t
	:defer t)

(use-package skewer-mode
	:ensure t
	:defer t
	:config
	(progn
		(add-hook 'js2-mode-hook 'skewer-mode)
		(add-hook 'css-mode-hook 'skewer-css-mode)
		(add-hook 'html-mode-hook 'skewer-html-mode)))

	(provide 'init-javascript)
