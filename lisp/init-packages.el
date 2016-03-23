(require 'package)

(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((cider . "melpa-stable")
          (ac-cider . "melpa-stable")
          (clojure-mode . "melpa-stable")
          (clojure-mode-extra-font-locking . "melpa-stable")
          (company-cider . "melpa-stable")
          (malabar-mode . "melpa-stable"))))

(defvar my/install-packages
  '(
    ;; package management
    use-package auto-compile
                
                ;; themeing
                rainbow-mode leuven-theme dakrone-theme color-identifiers-mode
                nyan-mode color-theme-sanityinc-tomorrow apropospriate-theme
                material-theme smart-mode-line beacon aurora-theme moe-theme
                spaceline solarized-theme tao-theme

                ;; misc
                diminish gist async sx exec-path-from-shell bbdb symon scpaste anzu
                
                ;; chat
                erc-hl-nicks ercn alert twittering-mode

                ;; code-helpers
                projectile highlight-indentation smartparens flycheck aggressive-indent smart-tab
                
                ;; dired
                peep-dired dired+ popwin

                ;; utils
                engine-mode undo-tree

                ;; lisp
                erefactor paredit

                ;; keybinding 
                hydra guide-key key-chord avy avy-zap smart-forward

                ;; markup language
                markdown-mode markdown-mode+ yaml-mode zencoding-mode adoc-mode

                ;; editing
                visual-fill-column fill-column-indicator

                ;; autocomplete
                fuzzy popup company yasnippet auto-complete company-quickhelp

                ;; helm
                helm helm-descbinds helm-swoop helm-projectile helm-ag helm-css-scss helm-gtags helm-ls-git
                helm-flycheck helm-flyspell helm-flx

                ;; highlighting
                idle-highlight-mode
                
                ;; news
                elfeed

                ;; window mgmt
                golden-ratio switch-window windmove

                ;;general
                keyfreq

                ;; eshell
                eshell-prompt-extras

                ;; clojure
                clojure-mode clojure-mode-extra-font-locking cider paredit paren-face ac-cider

                ;; git
                magit magit-gh-pulls git-messenger git-gutter+ with-editor git-timemachine

                ;; web
                web-mode emmet-mode web-beautify scss-mode

                ;; org
                htmlize gnuplot-mode gnuplot org-alert org-present org-bullets deft org-pomodoro

                ;; eww
                eww-lnum
                
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
