;; (use-package js3-mode
;; 	:ensure t
;; 	:defer t
;; 	:config
;; 	(progn
;; 		(setq js3-auto-indent-p t
;; 					js3-curly-indent-offset 0
;; 					js3-enter-indents-newline t
;; 					js3-expr-indent-offset 2
;; 					js3-indent-on-enter-key t
;; 					js3-lazy-commas t
;; 					js3-lazy-dots t
;; 					js3-lazy-operators t
;; 					js3-paren-indent-offset 2
;; 					js3-square-indent-offset 4)
;; 		(linum-mode 1)))

(use-package js2-mode
	:ensure t
	:mode "\\.json$\\'"
	:config
	(progn
		(add-hook 'js-mode-hook 'js2-minor-mode)
		(setq js2-highlight-level 3)))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(use-package js2-refactor
	:ensure t
	:mode "\\.js$\\'"
	)

(use-package tern
	:ensure t
	:config
	(progn
		(add-hook 'js-mode-hook (lambda () (tern-mode t)))))


(use-package ac-js2
	:ensure t
	:config
	(progn
		(add-hook 'js2-mode-hook 'ac-js2-mode)))

(use-package tern-auto-complete
	:ensure t
	:config
	(progn
		(require 'tern-auto-complete)
		(tern-ac-setup)))

;; kill the tern server if auto reload stops working
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(use-package nodejs-repl
	:ensure t
	)

(use-package skewer-mode
	:ensure t
	:config
	(progn
		(add-hook 'js2-mode-hook 'skewer-mode)
		(add-hook 'css-mode-hook 'skewer-css-mode)
		(add-hook 'html-mode-hook 'skewer-html-mode)))

;; use paredit for javascript
(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))
(add-hook 'js-mode-hook 'my-paredit-nonlisp)
	
(define-key js-mode-map "{" 'paredit-open-curly)
(define-key js-mode-map "}" 'paredit-close-curly-and-newline)


;; enable flycheck for javascript
(add-hook 'js-mode-hook
					(lambda () (flycheck-mode t)))

;;mozrepl needs to be installed in firefox
(use-package moz
	:ensure t
	)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

(provide 'init-javascript)
