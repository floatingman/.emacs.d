(use-package elpy
  :defer t
  :ensure t
  :config
  (elpy-enable)
  )

(use-package jedi
  :defer t
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  )

(use-package company-jedi
  :ensure t)

(use-package virtualenvwrapper
  :ensure t
  :defer t
  :init
  (progn
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    (setq venv-location (or (getenv "WORKON_HOME")
                            "~/.venvs"))))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

(provide 'init-python)
