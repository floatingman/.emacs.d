(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun my/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
	     (not (or (get-text-property (point) 'part-side)
		      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode
  :ensure t
  :mode "\\.html\\.ejs\\'"
  :config
  (progn
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-ac-sources-alist
	  '(("css" . (ac-source-css-property))
	    ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
	  )))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook  'emmet-mode)
  )
(provide 'init-web)
