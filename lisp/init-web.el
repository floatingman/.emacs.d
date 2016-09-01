(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing 1))

(defun my/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))


(use-package web-mode
  :ensure t
  :mode (("\\.ejs\\'"        . web-mode)
         ("\\.html?\\'"      . web-mode)
         ("\\.erb\\'"        . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.jinja\\'"      . web-mode)
         ("\\.php\\'"        . web-mode)
         ("\\.js[x]?\\'"     . web-mode))
  :defer t
  :config
  (progn
    (setq web-mode-content-types-alist
	  '(("json" . "/some/path/.*\\.api\\'")
	    ("xml"  . "/other/path/.*\\.api\\'")
	    ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-auto-pairing 1)
    (defun my-web-mode-hook ()
      (setq web-mode-enable-auto-pairing nil))
    
    (add-hook 'web-mode-hook  'my-web-mode-hook)

    (defun sp-web-mode-is-code-context (id action context)
      (and (eq action 'insert)
           (not (or (get-text-property (point) 'part-side)
                    (get-text-property (point) 'block-side)))))

    
    ))


(use-package emmet-mode
  :ensure t
  :defer t
  :config
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  )

(use-package web-beautify
  :ensure t
  :defer t)

(use-package scss-mode
  :ensure t
  :defer t
  :mode (("\\.scss\\'" . scss-mode))
  :config
  (progn (setq scss-sass-command (expand-file-name "~/.rvm/gems/ruby-2.2.1/bin/sass"))))

(use-package lorem-ipsum
  :ensure t
  :init
  (global-set-key (kbd "C-c C-l s") 'lorem-ipsum-insert-sentences)
  (global-set-key (kbd "C-c C-l p") 'lorem-ipsum-insert-paragraphs)
  (global-set-key (kbd "C-c C-l l") 'lorem-ipsum-insert-list)
  )


(provide 'init-web)
