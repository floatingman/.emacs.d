;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

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
;; Load a development version of CEDET instead of built-in one
(setq cedetpath "override/cedet/cedet-devel-load.el")
;; (when (file-exists-p (expand-file-name cedetpath user-emacs-directory))
;;   (load-file (expand-file-name cedetpath user-emacs-directory))
;;   ;; Use the full Java 1.5 grammar to parse Java files
;;   (autoload 'wisent-java-default-setup "semantic/wisent/java"
;;     "Hook run to setup Semantic in `java-mode'." nil nil))

;; load development version of org-mode
(setq orgpath "override/org-mode/lisp/org.el")
(when (file-exists-p (expand-file-name orgpath user-emacs-directory))
  (add-to-list 'load-path "~/.emacs.d/override/org-mode/lisp")
  (add-to-list 'load-path "~/.emacs.d/override/org-mode/contrib/lisp")
  (require 'org))

;;(defvar my/background 'light)
(defvar my/background 'dark)

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



;; package archives
(require 'init-packages)
(setq custom-file "~/personal/.emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)



;; load custom config
(use-package better-defaults
  :ensure t)
(use-package init-general)
(when (file-exists-p "~/personal/org/todo.org")
  (use-package init-org))
(use-package init-mswindows)
(use-package init-theme)
(use-package init-dired)
(use-package init-display)
(use-package init-completion)
(use-package init-coding-helpers)
(use-package init-functions)
(use-package init-utils)
(use-package init-javascript)
(use-package init-web)
(use-package init-elisp)
(use-package init-clojure)
(use-package init-shell)
(use-package init-java)
(use-package init-python)
(use-package init-helm)
(use-package init-git)
(use-package init-spelling)
(use-package init-php)
(use-package init-news)
(use-package init-editing)
(use-package init-chat)
;;(when *is-linux* (use-package init-mail))
(use-package init-keybinding)
(use-package init-reading)
(use-package init-flymake)
(use-package init-browsing)
(use-package init-deft)
(use-package init-hugo)
(use-package init-r)
(use-package init-groovy)
(use-package init-lisp)
(use-package init-ivy)
(use-package init-ruby)
;;; Post initialization
;; start a server only in text mode
(use-package server
  :config
  (progn
    (when (not (window-system))
      (if (server-running-p server-name)
          nil
        (server-start)))))

(when (window-system)
  (setenv "EMACS_GUI" "t"))


(add-hook 'after-init-hook #'global-flycheck-mode)

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

;;; init.el ends here

