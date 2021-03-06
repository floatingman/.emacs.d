;;; init-editing --- Summary

;;; Commentary:

;;; Code:
(delete-selection-mode)

(use-package markdown-mode
  :ensure t)

(use-package haml-mode
  :ensure t)

(use-package edit-server
  :ensure t
  :init (edit-server-start)
  )


(use-package iedit
  :ensure t)

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

;; Whitespace mode from the awesome site http://writequit.org/org/settings.html
(setq whitespace-line-column 140)

(setq whitespace-style '(tabs newline space-mark
                              tab-mark newline-mark
                              face lines-tail))
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
      ;; 32 SPACE, 183 MIDDLE DOT
      '((space-mark nil)
        ;; 10 LINE FEED
        ;; (newline-mark 10 [172 10])
        (newline-mark nil)
        ;; 9 TAB, MIDDLE DOT
        (tab-mark 9 [183 9] [92 9])))

(setq whitespace-global-modes '(not org-mode
                                    eshell-mode
                                    shell-mode
                                    web-mode
                                    log4j-mode
                                    "Web"
                                    dired-mode
                                    emacs-lisp-mode
                                    clojure-mode
                                    lisp-mode))

;; turn on whitespace mode globally
(global-whitespace-mode 1)
(diminish 'global-whitespace-mode "")
(set-default 'indicate-empty-lines t)
(setq show-trailing-whitespace t)

;; Latex support
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-parse-self t)
  ;; Here we make auctex aware of latexmk and xelatexmk We can use
  ;; these instead of calling pdflatex, bibtex, pdflatex, pdflatex (or
  ;; similar). I'll set latexmk as the default as there's really no
  ;; reason to use pdflatex
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list '("latexmk" "latexmk -synctex=1 -shell-escape -pdf %s" TeX-run-TeX nil t :help "Process file with latexmk")))
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list '("xelatexmk" "latexmk -synctex=1 -shell-escape -xelatex %s" TeX-run-TeX nil t :help "Process file with xelatexmk")))
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk"))))

(provide 'init-editing)
;;; init-editing.el ends here
