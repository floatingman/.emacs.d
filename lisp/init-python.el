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
  (add-hook 'python-mode-hook 'jedi:ac-setup))

(use-package virtualenvwrapper
  :defer t
  :init
  (progn
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    (setq venv-location (or (getenv "WORKON_HOME")
                            "~/.venvs"))))

(provide 'init-python)
