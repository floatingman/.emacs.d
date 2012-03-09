(pymacs-exec "import sys, os")
(pymacs-exec "sys.path.append(os.path.join(os.path.expanduser('~'),'.emacs.d','daniel-pymacs-extensions'))")

;; python libraries
;(setq pymacs-load-path (list))
;(add-subdirectories-to-load-path 'pymacs-load-path "~/.emacs.d/daniel-pymacs-extensions")

;; shortcut function to insert license headers
(pymacs-load "license")
