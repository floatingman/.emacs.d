(setq ahk-syntax-directory "~/.emacs.d/vendor/ahk-org-mode/syntax/")
(add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-org-mode))
(autoload 'ahk-org-mode "ahk-org-mode")
