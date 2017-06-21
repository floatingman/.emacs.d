
;; Setup load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)
(require 'init-ui)
(require 'init-theme)
(require 'init-completion)
(require 'init-editing)
(require 'init-macos)
(require 'init-navigation)
;; Accept y or n instead of yes or no
(cl-flet ((always-yes (&rest _) t))
  (defun no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
              ((symbol-function 'yes-or-no-p) #'always-yes))
       (apply fun args))))
