;; turn on emacs debugging when starting, turn it off later
(setq debug-on-error t)
(setq debug-on-quit t)

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)

;;Setup some variables for use in other config files

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-gui* (not (eq window-system nil)))
(defvar running-alternate-emacs nil)

(package-initialize)
;; Setup load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)
(use-package init-utils)
(use-package init-ui)
(use-package init-theme)
(use-package init-completion)
(use-package init-whitespace)
(use-package init-editing)
(use-package init-macos)
(use-package init-navigation)
(use-package init-snippets)
(use-package init-misc)
(use-package init-lisp)
(use-package init-coding)
(use-package init-git)
(use-package init-dired)
(use-package init-python)
(use-package init-grep)

;; turn off debugging after emacs starts
(setq debug-on-error nil)
(setq debug-on-quit nil)

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-day)))
 '(custom-safe-themes
   (quote
    ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(package-selected-packages
   (quote
    (company-anaconda multiple-cursors diff-hl dired-sort dired+ yasnippet use-package smex redshank rainbow-delimiters projectile paredit nlinum indent-guide ido-ubiquitous guide-key golden-ratio flycheck exec-path-from-shell erefactor company-quickhelp color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized auto-compile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
