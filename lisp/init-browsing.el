(use-package eww
  :defer t
  :config
  (use-package s
    :ensure t)
  (define-key eww-mode-map "o" 'eww)
  (define-key eww-mode-map "O" 'eww-browse-with-external-browser)
  (setq browse-url-browser-function
        '((".*google.*maps.*" . browse-url-generic)
          ("." . eww-browse-url)))
  (setq shr-external-browser 'browse-url-generic)
  (setq browse-url-generic-program (executable-find "firefox"))

  (use-package eww-lnum
    :config
    (bind-key "f" #'eww-lnum-follow eww-mode-map)
    (bind-key "F" #'eww-lnum-universal eww-mode-map)))

(global-set-key (kbd "C-x m") 'browse-url-at-point)

(provide 'init-browsing)
