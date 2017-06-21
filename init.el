
;; Setup load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)
(require 'init-ui)
(require 'init-theme)
(require 'init-completion)
(require 'init-editing)
(require 'init-macos)
(require 'init-navigation)
(require 'init-snippets)
(require 'init-misc)
