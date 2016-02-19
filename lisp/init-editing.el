(use-package markdown-mode
	:ensure t
	:config
	(progn
		(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
		(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
		(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))))


;; this is for fun, a prose checker
;; you need to install proselint with pip
;; pip install proselint
(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
						(id (one-or-more (not (any " "))))
						(message) line-end))
  :modes (text-mode markdown-mode gfm-mode))

(add-to-list 'flycheck-checkers 'proselint)


(provide 'init-editing)
