
(add-to-list 'package-archives `("melpa" . "https://melpa.org/packages/"))

;; Mainly for ruby-mode
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))


;; We include the org repository for completeness, but don't normally
;; use it.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-refresh-contents)
(provide 'init-packages)
