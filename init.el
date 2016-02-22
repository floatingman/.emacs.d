;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

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

;; Setup user information
(setq user-full-name "Daniel Newman"
      user-mail-address "dwnewman78@gmail.com")

;; This sets up the load path so that we can override it
(package-initialize nil)


;; setup load path
(eval-and-compile
  (mapc #'(lambda (path)
            (add-to-list 'load-path
                         (expand-file-name path user-emacs-directory)))
        '("site-lisp" "override" "lisp" ))

  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept)))

;; Load the rest of the packages
(package-initialize nil)

(setq package-enable-at-startup nil)

;; package archives
(require 'init-packages)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)


;; load custom config

(use-package init-general)
(use-package init-org
  :load-path ("override/org-mode/contrib/lisp"
	      "override/org-mode/lisp"))
(use-package init-mswindows)
(use-package init-theme)
(use-package init-dired)
(use-package init-completion)
(use-package init-coding-helpers)
(use-package init-functions)
(use-package init-utils)
(use-package init-javascript)
(use-package init-web)
(use-package init-elisp)
(use-package init-git)
(use-package init-spelling)
(use-package init-php)
(use-package init-news)
(use-package init-editing)
(use-package init-chat)
(use-package init-keybinding)
;;; Post initialization
(server-start)
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

(add-hook 'after-init-hook #'global-flycheck-mode)

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

;;; init.el ends here

