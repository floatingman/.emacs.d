(require 'package)

(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '()))

(defvar packages-refreshed? nil)

;; Misc packages to try out and find a place for later.
(use-package docker
  :ensure t)

(use-package nginx-mode
  :ensure t)

(use-package command-log-mode
  :ensure t)
(use-package csharp-mode
  :ensure t)
(use-package prodigy
  :ensure t)
(use-package restclient
  :ensure t)

(use-package super-save
  :defer t
  :config
  (progn
    (dolist (f '(select-window
                 select-window-by-number
                 ace-select-window))
      (add-to-list 'super-save-triggers (symbol-name f)))
    (super-save-initialize)))


(provide 'init-packages)
