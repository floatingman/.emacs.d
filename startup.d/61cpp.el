(require 'cc-mode)
(setq gdb-many-windows t)
(semantic-mode 1)
(global-semantic-idle-completions-mode 1)
(add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq c-default-style "stroustrup")
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

