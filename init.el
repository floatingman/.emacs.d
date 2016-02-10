;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;; Override the packages with the the git version of Org and other packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/org-mode/contrib/lisp"))




;; Load the rest of the packages
(package-initialize nil)
(setq package-enable-at-startup nil)
;;(org-babel-load-file "~/.emacs.d/dnewman.org")

;; Load some custom configs 
(require 'init-org) ;; load org-mode
(require 'init-mswindows)





(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))


(provide 'init)
