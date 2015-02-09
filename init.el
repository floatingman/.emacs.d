;; This sets up the load path so that we can override it
(package-initialize nil)

;; Override the packages with the the git version of Org and other packages
(add-to-list 'load-path "~/elisp/org-mode/lisp")
(add-to-list 'load-path "~/elisp/org-mode/contrib/lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Initialize a few variables
(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-gui* (not (eq window-system nil)))

;; From old config
(require 'init-benchmarking) ;; Measure startup time
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

(require 'init-org) ;; load org-mode

;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)
(org-babel-load-file "~/.emacs.d/dnewman.org")



(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))


(provide 'init)
