;; This sets up the load path so that we can override it
(package-initialize nil)

(add-to-list 'load-path "/usr/share/emacs/site-lisp")
;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)
(org-babel-load-file "~/.emacs.d/dnewman.org")
