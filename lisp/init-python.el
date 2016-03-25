(use-package python
  :defer t
  :config
  (define-key python-mode-map (kbd "C-c C-z") 'run-python)
  (define-key python-mode-map (kbd "<backtab>") 'python-back-indent))

(use-package virtualenvwrapper
  :defer t
  :init
  (progn
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    (setq venv-location (or (getenv "WORKON_HOME")
                            "~/.venvs"))))

(provide 'init-python)
