(add-to-list 'load-path "~/git/lintnode")
(require 'flymake-jslint)
(setq lintnode-location "~/git/lintnode")
(setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
(add-hook 'js-mode-hook
          (lambda ()
            (lintnode-hook)))

(require 'flymake-cursor)
