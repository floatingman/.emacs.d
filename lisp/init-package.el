(require 'package) ;; You might already have this line

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-refresh-contents)
)

;; Setup Use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(provide 'init-package)
