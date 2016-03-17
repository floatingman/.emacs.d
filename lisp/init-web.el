(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing 1))

(defun my/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))


(use-package web-mode
  :ensure t
  :mode (("\\.html\\'"       . web-mode)
         ("\\.ejs\\'"        . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.jinja\\'"      . web-mode)
         ("\\.php\\'"        . web-mode))
  :defer t
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-auto-pairing 1)))

(use-package emmet-mode
  :ensure t
  :defer t
  :config
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  )

(use-package web-beautify
  :defer t
	:ensure t)

(use-package scss-mode
  :ensure t
  :defer t
  :mode (("\\.scss\\'" . scss-mode)))

(provide 'init-web)
