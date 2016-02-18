(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun my/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
	     (not (or (get-text-property (point) 'part-side)
		      (get-text-property (point) 'block-side))))
    t))
(use-package ac-html :ensure t)
(use-package ac-emmet :ensure t)
(use-package ac-html-angular :ensure t)
(use-package ac-html-bootstrap :ensure t)
(use-package ac-html-csswatcher :ensure t)

(use-package web-mode
  :ensure t
  :mode "\\.html\\'|\\.ejs\\'"
  :config
  (progn
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-ac-sources-alist
	  '(("css" . (ac-source-css-property ac-source-emmet-css-snippets))
	    ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets ac-source-words-in-buffer ac-source-abbrev))
			("php" . (ac-source-words-in-buffer
                  ac-source-words-in-same-mode-buffers
                  ac-source-dictionary))))))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook  'emmet-mode)
	(add-hook 'web-mode-hook 'emmet-mode)
  )

(use-package web-beautify
	:ensure t)

(provide 'init-web)
