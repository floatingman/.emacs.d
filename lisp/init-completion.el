
(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command))
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(provide 'init-completion)
