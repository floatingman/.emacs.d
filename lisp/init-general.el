;;Backups
(setq delete-auto-save-files t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq temporary-file-directory "~/.emacs.d/tmp/")
;;History
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(setq-default save-place t)



;; setup path
(use-package exec-path-from-shell
  :defer t
  :init
  (progn
    (setq exec-path-from-shell-variables '("JAVA_HOME"
                                           "PATH"
                                           "WORKON_HOME"
                                           "MANPATH"))
    (exec-path-from-shell-initialize)))

;;Help
(use-package guide-key
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
    (guide-key-mode 1)))

;; Moving between windows
(use-package windmove
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   ))

;; Split windows by golden ratio
(use-package golden-ratio
  :diminish golden-ratio-mode
  :defer t
	:config
	(defun my/helm-alive-p ()
    (if (boundp 'helm-alive-p)
        (symbol-value 'helm-alive-p)))
  (add-to-list 'golden-ratio-exclude-modes #'messages-buffer-mode)
  (add-to-list 'golden-ratio-exclude-modes #'fundamental-mode)
  ;; Inhibit helm
  (add-to-list 'golden-ratio-inhibit-functions #'my/helm-alive-p))

(use-package switch-window
  :bind (("C-x o" . switch-window)))

;;Recent files found at awesome site http://writequit.org/org/settings.html
(use-package recentf
  :defer 10
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :init
  (setq recentf-max-saved-items 300
        recentf-exclude '("/auto-install/" ".recentf" "/repos/" "/elpa/"
                          "\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"
                          ".gz"
                          "~$" "/tmp/" "/ssh:" "/sudo:" "/scp:")
        recentf-auto-cleanup 600)
  (when (not noninteractive) (recentf-mode 1))
  


  )

;; reload buffers when file changes on disk
(global-auto-revert-mode t)
;; be quiet about reverting files
(setq auto-revert-verbose nil)

;; start emacs maximized
;; found here http://thestandardoutput.com/posts/how-to-properly-maximize-the-active-emacs-frame-on-startup-on-windows/
(defun w32-maximize-frame ()
  "Maximize the current frame (windows only)"
  (interactive)
  (w32-send-sys-command 61488))

(if (eq system-type 'windows-nt)
    (progn
      (add-hook 'window-setup-hook 'w32-maximize-frame t))
  (set-frame-parameter nil 'fullscreen 'maximized))


;; set default columns to 80 and tabs to 2
(setq-default fill-column 80)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)

;;make unique buffer names when creating duplicate buffers
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))  ;; buffernames that are foo<1>, foo<2> are hard to read. This makes them foo|dir  foo|otherdir

;;setup good pasteing real good
(setq kill-ring-max 100) 
(setq x-select-enable-clipboard t) 
(setq select-active-regions t) 
(setq save-interprogram-paste-before-kill 1) 
(setq yank-pop-change-selection t)

;; ignore case when completeing file names
(setq read-file-name-completion-ignore-case t)

;;display time and load on modeline
(setq
 ;; don't display info about mail
 display-time-mail-function (lambda () nil)
 ;; update every 15 seconds instead of 60 seconds
 display-time-interval 15)
(display-time-mode 1)

;; find out what commands I use most frequently.
(use-package keyfreq
  :config
  (progn
    (setq keyfreq-excluded-commands
          '(self-insert-command
            abort-recursive-edit
            forward-char
            backward-char
            previous-line
            next-line))
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))

;; syntax higlighting in all buffers
(global-font-lock-mode t)

;; dial back emacs garbage collection
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100 mb
;; Allow font-lock-mode to do background parsing
(setq jit-lock-stealth-time 1
      ;; jit-lock-stealth-load 200
      jit-lock-chunk-size 1000
      jit-lock-defer-time 0.05)

;; fix an issue with line-numbers breaking
(setq line-number-display-limit-width 10000)

;; make gnutil safer
(setq gnutls-min-prime-bits 4096)

;; fix scrolling
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

;; increase keystroke completion 
(setq echo-keystrokes 0.4)

;; don't warn about opening files unless they're over 25mb
(setq large-file-warning-threshold (* 25 1024 1024))


;; when I select a region and start typing
(delete-selection-mode 1)

;; disable marks when changing focus
(transient-mark-mode 1)

;; don't show empty lines
(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries nil)

;; turn off all the modes
(when (functionp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (functionp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'nil))
(when (functionp 'tooltip-mode)
  (tooltip-mode -1))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(require 'mouse-copy)
(global-set-key [C-down-mouse-1] 'mouse-drag-secondary-pasting)
(global-set-key [C-S-down-mouse-1] 'mouse-drag-secondary-moving)

;; no beeping no startup messages
(setq ring-bell-function (lambda ()))
(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode)

;; ignore case when completing filenames
(setq read-file-name-completion-ignore-case t)

;; y or n to answer questions that trouble me so
(defalias 'yes-or-no-p 'y-or-n-p)

;; confirm quiting
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; make moving around long lines easy
(setq line-move-visual t)

;; hide that mouse
(setq make-pointer-invisible t)

;; fix weird color issues
(setq system-uses-terminfo nil)

;; resolve symlinks
(setq-default find-file-visit-truename t)

;; require a new line at the end of file
(setq require-final-newline t)

;; use regex for searching cause it's fun
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

;; end sentences with single space
(setq sentence-end-double-space nil)

;; split windows awesomer
(setq split-height-threshold nil)
(setq split-width-threshold 180)

;; rescan for changes in imenu
(set-default 'imenu-auto-rescan t)

;; make random randomer
(random t)

;; use unified diffs
(setq diff-switches "-u")

;; make them symbols pretty
(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '("lambda" . ?λ) prettify-symbols-alist)))
  (add-hook 'clojure-mode-hook
            (lambda ()
              (push '("fn" . ?ƒ) prettify-symbols-alist)))
  (global-prettify-symbols-mode +1))

;; prefer newer files over elc
(setq load-prefer-newer t)

;; use winner move to undo window configurations with C-c LEFT
(use-package winner
  :init (winner-mode 1))

;; quit emacs superfast when the fuzz shows up
(defun my/quit-emacs-unconditionally ()
  (interactive)
  (my-quit-emacs '(4)))

(define-key special-event-map (kbd "<sigusr1>") #'my/quit-emacs-unconditionally)

;; Change the clipboard settings to better integrate into Linux:
(setq x-select-enable-clipboard t)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; save whatever is in the clipboard before replacing it with the emacs text
(setq save-interprogram-paste-before-kill t)

(use-package saveplace
  :defer t
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

;; shows the number of search hits in the modeline
(use-package anzu
  :defer t
  :bind ("M-%" . anzu-query-replace-regexp)
  :config
  (progn
    (use-package thingatpt)
    (setq anzu-mode-lighter "")
    (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

(add-hook 'prog-mode-hook #'anzu-mode)
(add-hook 'org-mode-hook #'anzu-mode)


(use-package helm-flx
  :init (helm-flx-mode +1))

(use-package idle-highlight-mode
  :init
  (add-hook 'java-mode-hook #'idle-highlight-mode)
  (add-hook 'emacs-lisp-mode-hook #'idle-highlight-mode)
  (add-hook 'clojure-lisp-mode-hook #'idle-highlight-mode))

(use-package beacon
  :diminish beacon-mode
  :init (beacon-mode 1)
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode))

(use-package eyebrowse
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c C-b"))
  (progn
    (defun my/create-eyebrowse-setup ()
      (interactive)
      "Create a default window config, if none is present"
      (when (not (eyebrowse--window-config-present-p 2))
        ;; there's probably a better way to do this, creating two workspaces
        (eyebrowse-switch-to-window-config-2)
        (eyebrowse-switch-to-window-config-1)))
    (setq eyebrowse-wrap-around t
          eyebrowse-new-workspace t)
    (eyebrowse-mode 1)
    (global-set-key (kbd "C-'") 'eyebrowse-next-window-config)
    
    (add-hook 'after-init-hook #'my/create-eyebrowse-setup)))

(use-package smartscan
     :init (add-hook #'prog-mode-hook #'smartscan-mode)
     :config
     (bind-key "M-'" #'other-window smartscan-map))

(provide 'init-general)
