
(setq user-full-name "Daniel Newman"
      user-mail-address "dnewman@danlovesprogramming.com")

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-refresh-contents))

(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/artbollocks-mode")
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(use-package auto-compile
  :ensure t
  :init (auto-compile-on-load-mode))
