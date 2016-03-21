(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Mainly for ruby-mode
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))


;; We include the org repository for completeness, but don't normally
;; use it.
;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar my/install-packages
  '(
    ;; themeing
    rainbow-mode leuven-theme dakrone-theme color-identifiers-mode moe-theme nyan-mode color-theme-sanityinc-tomorrow powerline volatile-highlights apropospriate-theme material-theme visible-mark

                 ;; chat
                 erc-hl-nicks ercn alert twittering-mode

                 ;; dired
                 dired-details peep-dired dired+ popwin

                 ;; utils
                 engine-mode undo-tree

                 ;; lisp
                 erefactor paredit

                 ;; keybinding 
                 key-chord avy avy-zap smart-forward

                 ;; markup language
                 markdown-mode markdown-mode+ yaml-mode zencoding-mode adoc-mode

                 ;; editing
                 visual-fill-column fill-column-indicator

                 ;; autocomplete
                 fuzzy popup company yasnippet

                 ;; helm
                 helm helm-descbinds helm-swoop helm-projectile helm-ag helm-css-scss helm-gtags helm-ls-git
                 helm-flycheck helm-flyspell helm-dash

                 ;; news
                 elfeed
                 
                 
                 ))

(defvar packages-refreshed? nil)

(dolist (pack my/install-packages)
  (unless (package-installed-p pack)
    (unless packages-refreshed?
      (package-refresh-contents)
      (setq packages-refreshed? t))
    (package-install pack)))

(setq use-package-verbose nil)

;; (package-refresh-contents)
(provide 'init-packages)
