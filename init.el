;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)

(eval-and-compile
  (defconst emacs-environment (getenv "NIX_MYENV_NAME"))

  (mapc #'(lambda (path)
            (add-to-list 'load-path
                         (expand-file-name path user-emacs-directory)))
        '("site-lisp" "override" "lisp" "lisp/use-package"))

  (defun nix-read-environment (name)
    (let ((script
           (nth 0 (split-string
                   (shell-command-to-string (concat "which load-env-" name))
                   "\n"))))
      (if (string= script "")
          (error "Could not find environment %s" name)
        (with-temp-buffer
          (insert-file-contents-literally script)
          (when (re-search-forward "^source \\(.+\\)$" nil t)
            (let ((script2 (match-string 1)))
              (with-temp-buffer
                (insert-file-contents-literally script2)
                (when (re-search-forward "^ outbuildings=\"\\(.+?\\)\""
                                         nil t)
                  (let ((inputs (split-string (match-string 1))))
                    inputs)))))))))
  
  (when (executable-find "nix-env")
    (mapc #'(lambda (path)
              (let ((share (expand-file-name "share/emacs/site-lisp" path)))
                (if (file-directory-p share))))
          (nix-read-environment emacs-environment)))
  
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))

  (require 'cl)

  (defvar use-package-verbose t)
  ;; (defvar use-package-expand-minimally t)
  (require 'use-package))

;;Setup some variables for use in other config files

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-gui* (not (eq window-system nil)))
(defvar running-alternate-emacs nil)
(defvar running-development-emacs nil)

;;(defvar my/background 'light)
(defvar my/background 'dark)

;; Setup user information
(setq user-full-name "Daniel Newman"
      user-mail-address "dwnewman78@gmail.com")

;; This sets up the load path so that we can override it
(package-initialize nil)

(require 'bind-key)
(require 'diminish nil t)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

;; package archives
;; (require 'init-packages)
(setq custom-file "~/personal/.emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(autoload 'org-cycle "org" nil t)
(autoload 'hippie-expand "hippie-exp" nil t)
(autoload 'indent-according-to-mode "indent" nil t)

;;; Enable disabled commands

(put 'downcase-region             'disabled nil)   ; Let downcasing work
(put 'erase-buffer                'disabled nil)
(put 'eval-expression             'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page              'disabled nil)   ; Let narrowing work
(put 'narrow-to-region            'disabled nil)   ; Let narrowing work
(put 'set-goal-column             'disabled nil)
(put 'upcase-region               'disabled nil)   ; Let upcasing work
(put 'company-coq-fold            'disabled nil)
(put 'TeX-narrow-to-group         'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)

;;; Configure libraries

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lib" user-emacs-directory)))

(use-package anaphora       :defer t :load-path "lib/anaphora")
(use-package button-lock    :defer t :load-path "lib/button-lock")
(use-package ctable         :defer t :load-path "lib/emacs-ctable")
(use-package dash           :defer t :load-path "lib/dash-el")
(use-package deferred       :defer t :load-path "lib/emacs-deferred")
(use-package epc            :defer t :load-path "lib/emacs-epc")
(use-package epl            :defer t :load-path "lib/epl")
(use-package f              :defer t :load-path "lib/f-el")
(use-package fame           :defer t :load-path "lib/fame")
(use-package fuzzy          :defer t :load-path "lib/fuzzy-el")
(use-package gh             :defer t :load-path "lib/gh-el")
(use-package ht             :defer t :load-path "lib/ht-el")
(use-package let-alist      :defer t :load-path "lib/let-alist")
(use-package logito         :defer t :load-path "lib/logito")
(use-package makey          :defer t :load-path "lib/makey")
(use-package marshal        :defer t :load-path "lib/marshal-el")
(use-package pcache         :defer t :load-path "lib/pcache")
(use-package pkg-info       :defer t :load-path "lib/pkg-info")
(use-package popup          :defer t :load-path "lib/popup-el")
(use-package popwin         :defer t :load-path "lib/popwin-el")
(use-package pos-tip        :defer t :load-path "lib/pos-tip")
(use-package request        :defer t :load-path "lib/emacs-request")
(use-package s              :defer t :load-path "lib/s-el")
(use-package tablist        :defer t :load-path "lib/tablist")
(use-package uuidgen        :defer t :load-path "lib/uuidgen-el")
(use-package web            :defer t :load-path "lib/emacs-web")
(use-package websocket      :defer t :load-path "lib/emacs-websocket")
(use-package web-server     :defer t :load-path "lib/emacs-web-server")
(use-package working        :defer t :load-path "lib/working")
(use-package xml-rpc        :defer t :load-path "lib/xml-rpc")

;; (use-package auto-compile
;;   :ensure t
;;   :config (auto-compile-on-load-mode))
;; (setq load-prefer-newer t)

;; load custom config
;;(use-package better-defaults
;;:ensure t)
(use-package init-general)
(use-package init-org
  :load-path ("override/org-mode/contrib/lisp"
              "override/org-mode/lisp")
  :commands my-org-startup
  :bind (("M-C" . jump-to-org-agenda)
         ("M-m" . org-smart-capture)
         ("M-M" . org-inline-note)
         ("C-c a" . org-agenda)
         ("C-c S" . org-store-link)
         ("C-c l" . org-insert-link)
         ("C-. n" . org-velocity-read)
         )
  :defer 30
  :config
  (when (and nil
             (not running-alternate-emacs)
             (not running-development-emacs))
    (run-with-idle-timer 300 t 'jump-to-org-agenda)
    (my-org-startup)))
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
(use-package init-cc)
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
(use-package init-diminish)
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
