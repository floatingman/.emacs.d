(require 'package)

(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '()))

;; (defvar my/install-packages
;;   '(

;;     ;; package management
;;     use-package auto-compile

;;      ;; themeing
;;      rainbow-mode leuven-theme dakrone-theme color-identifiers-mode
;;      nyan-mode color-theme-sanityinc-tomorrow apropospriate-theme
;;      material-theme smart-mode-line beacon aurora-theme moe-theme
;;      spaceline solarized-theme tao-theme

;;      ;; misc
;;      diminish gist async sx exec-path-from-shell bbdb symon scpaste anzu bookmark+

;;      ;; logs
;;      log4j-mode logstash-conf

;;      ;; chat
;;      erc-hl-nicks ercn alert twittering-mode

;;      ;; the all-seeing eye
;;      sauron

;;      ;; code-helpers
;;      projectile smartparens smart-tab ggtags smartscan eyebrowse

;;      ;; java
;;      malabar-mode groovy-mode emacs-eclim java-imports

;;      ;; python
;;      hy-mode virtualenvwrapper jedi elpy

;;      ;; flycheck
;;      flycheck flycheck-tip flycheck-haskell flycheck-pos-tip

;;      ;; dired
;;      peep-dired dired+ popwin

;;      ;; utils
;;      engine-mode undo-tree

;;      ;; lisp
;;      paredit elisp-slime-nav

;;      ;; keybinding 
;;      guide-key avy avy-zap smart-forward

;;      ;; markup language
;;      markdown-mode markdown-mode+ yaml-mode zencoding-mode adoc-mode

;;      ;; editing
;;      visual-fill-column fill-column-indicator

;;      ;; autocomplete
;;      fuzzy popup company yasnippet auto-complete company-quickhelp

;;      ;; helm
;;      helm helm-descbinds helm-swoop helm-projectile helm-ag helm-css-scss helm-gtags helm-ls-git
;;      helm-flycheck helm-flyspell helm-flx

;;      ;; highlighting
;;      idle-highlight-mode

;;      ;; javascript
;;      tern json-mode js2-mode js2-refactor nodejs-repl skewer-mode jsx-mode

;;      ;; news
;;      elfeed

;;      ;; window mgmt
;;      golden-ratio switch-window windmove

;;      ;;general
;;      keyfreq

;;      ;; eshell
;;      eshell-prompt-extras

;;      ;; clojure
;;      clojure-mode clojure-mode-extra-font-locking cider paredit paren-face ac-cider

;;      ;; git
;;      magit magit-gh-pulls git-gutter+ with-editor git-timemachine git-gutter

;;      ;; web
;;      web-mode emmet-mode web-beautify scss-mode

;;      ;; org
;;      htmlize gnuplot-mode gnuplot org-alert org-present org-bullets deft org-pomodoro

;;      ;; eww
;;      eww-lnum

;;      ))

(defvar packages-refreshed? nil)

;; (dolist (pack my/install-packages)
;;   (unless (package-installed-p pack)
;;     (unless packages-refreshed?
;;       (package-refresh-contents)
;;       (setq packages-refreshed? t))
;;     (unwind-protect
;;         (condition-case ex
;;             (package-install pack)
;;           ('error (message "Failed to install package [%s], caught exception: [%s]"
;;                            pack ex)))
;;       (message "Installed %s" pack))))

;; Load use-package, used for loading packages everywhere else
(require 'use-package)
;; Set to t to debug package loading or nil to disable
(setq use-package-verbose nil)

;; Misc packages to try out and find a place for later.
(use-package docker
  :ensure t)

(use-package nginx-mode
  :ensure t)

(use-package command-log-mode
  :ensure t)
(use-package csharp-mode
  :ensure t)
(use-package prodigy
  :ensure t)
(use-package restclient
  :ensure t)

(use-package super-save
  :defer t
  :config
  (progn
    (dolist (f '(select-window
                 select-window-by-number
                 ace-select-window))
      (add-to-list 'super-save-triggers (symbol-name f)))
    (super-save-initialize)))


(provide 'init-packages)
