;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;; Override the packages with the the git version of Org and other packages
(eval-and-compile
  (mapc #'(lambda (path)
            (add-to-list 'load-path
                         (expand-file-name path user-emacs-directory)))
        '("site-lisp" "override" "lisp" "site-lisp/use-package" "")))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-mode/contrib/lisp"))
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
(package-initialize nil)
;(setq package-enable-at-startup nil)
;;(org-babel-load-file "~/.emacs.d/dnewman.org")
;; install use-package
(require 'cl)
(defvar use-package-verbose t)
(require 'use-package)
(require 'diminish nil t)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-gui* (not (eq window-system nil)))

;; Load some custom configs 
(require 'init-org) ;; load org-mode
(require 'init-mswindows)

(provide 'init)
