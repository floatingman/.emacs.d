;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Override the packages with the the git version of Org and other packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)
(org-babel-load-file "~/.emacs.d/dnewman.org")

(require 'init-org) ;; load org-mode
(require 'init-mswindows)






(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))


(provide 'init)
