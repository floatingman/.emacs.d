;;; init-editing --- Summary

;;; Commentary:

;;; Code:
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
  :modes (org-mode text-mode markdown-mode gfm-mode))

(add-to-list 'flycheck-checkers 'proselint)

;;make writing posts in org-mode look more better
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'markdown-mode-hook (lambda () (set-fill-column 80)))
(add-hook 'org-mode-hook (lambda () (set-fill-column 120)))

(use-package visual-fill-column
	:ensure t
	:config
	(progn
		(global-visual-fill-column-mode)))

(use-package fill-column-indicator
	:ensure t)

(provide 'init-editing)
;;; init-editing.el ends here
