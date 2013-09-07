;;; init.el --- dnewman's configuration file

(add-to-list 'load-path user-emacs-directory)
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* t)
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-gui* (not (eq window-system nil)))

;;-----------------------------------------------------
;; initial libraries to load
;;-----------------------------------------------------
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;-----------------------------------------------------
;; Load configs for specific features and modes
;;-----------------------------------------------------

(require-package 'wgrep)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)

(require-package 'base16-theme)
(require-package 'browse-kill-ring)
(require-package 'clojure-mode)
(require-package 'dropdown-list)
(require-package 'emms)
(require-package 'google-maps)
(require-package 'highlight-cl)
(require-package 'iy-go-to-char)
(require-package 'jedi)
(require-package 'keyfreq)
(require-package 'lorem-ipsum)
(require-package 'powershell)
(require-package 'powershell-mode)
(require-package 'pylint)
(require-package 'smartrep)
(require-package 'smart-operator)
(require-package 'smart-tab)
(require-package 'tfs)
(require-package 'thesaurus)
(require-package 'yaml-mode)
(require-package 'yasnippet)


(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-os-keys)
(require 'init-gui-frames)
(require 'init-maxframe)
(require 'init-dired)
(require 'init-isearch)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)

(require 'init-recentf)
(require 'init-ido)
(require 'init-hippie-expand)
(require 'init-auto-complete)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
(require 'init-mmm)
;; (require 'init-growl)

(require 'init-editing-utils)

(require 'init-darcs)
(require 'init-git)

(require 'init-markdown)
(require 'init-csv)
(require 'init-erlang)
(require 'init-javascript)
(require 'init-php)
(require 'init-org)
(require 'init-nxml)
(require 'init-css)
(require 'init-haml)
(require 'init-python-mode)
(require 'init-haskell)
(require 'init-ruby-mode)
(require 'init-rails)
(require 'init-sql)

(require 'init-paredit)
(require 'init-lisp)
(require 'init-slime)
(require 'init-clojure)
(require 'init-common-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-marmalade)
(require 'init-misc)
(require 'init-mswindows)
(require 'init-skewer)
(require 'init-html)
(require 'init-key-bindings)
;; Extra packages which don't require any configuration,

(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)
(require-package 'dsvn)
(require-package 'regex-tool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow access from emacsclient ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'server)
(unless (server-running-p)
  (server-start))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables configured via the interactive 'customize' interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow users to provide an optional "init-local" containing personal settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'init-local nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locales (setting them earlier in this file doesn't work in X) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'init-locales)

(message "init completed in %.2fms"
         (sanityinc/time-subtract-millis (current-time) before-init-time))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
