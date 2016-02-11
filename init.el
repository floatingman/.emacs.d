;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)

(eval-and-compile
  (mapc #'(lambda (path)
            (add-to-list 'load-path
                         (expand-file-name path user-emacs-directory)))
        '("site-lisp" "override" "lisp" "site-lisp/use-package" ))

  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))

  (require 'cl)

  (defvar use-package-verbose t)
  ;;(defvar use-package-expand-minimally t)
  (require 'use-package))


(require 'package)
(when (< emacs-major-version 24)
  ;; Mainly for ruby-mode
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))

;; We include the org repository for completeness, but don't normally
;; use it.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Also use Melpa for most packages
(add-to-list 'package-archives `("melpa" . ,(if (< emacs-major-version 24)
                                                "http://melpa.org/packages/"
                                              "https://melpa.org/packages/")))

;; Load the rest of the packages
;;(package-initialize nil)
;(setq package-enable-at-startup nil)
;;(org-babel-load-file "~/.emacs.d/dnewman.org")
;; install use-package
(require 'bind-key)          
(require 'diminish nil t)


(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-gui* (not (eq window-system nil)))

;; Load some custom configs
(use-package init-org
  :load-path ("override/org-mode/contrib/lisp"
	      "override/org-mode/lisp")
  :defer 5)

;(require 'init-org) ;; load org-mode
(require 'init-mswindows)

;;; Post initialization

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
