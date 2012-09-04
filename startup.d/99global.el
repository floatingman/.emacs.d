;; (global-font-lock-mode 1)
;; (winner-mode 1)
;; (savehist-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
;; (transient-mark-mode 1)
;; (line-number-mode 1)
;; (column-number-mode 1)
(global-auto-revert-mode t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1))

(mouse-wheel-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global t
      delete-by-moving-to-trash t
      shift-select-mode nil
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat starter-kit-dir "oddmuse")
      xterm-mouse-mode t
      save-place-file (concat starter-kit-dir "places"))
(auto-compression-mode t)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq make-backup-files t)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq backup-directory-alist '(("." . "~/emacs-meta/backups")))
;; (setq auto-save-default nil)

;; (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
;; (setq warning-suppress-types nil)



(defadvice flymake-start-syntax-check-process
  (after my-flymake-start-syntax-check-process
         (cmd args dir) activate compile)
  ;; set flag to allow exit without query on any active flymake
  ;; processes
  (set-process-query-on-exit-flag ad-return-value nil))

;; (add-hook 'find-file-hook (lambda ()
;; (when (and buffer-file-name
;; (not (file-writable-p buffer-file-name))
;; (y-or-n-p (format "File %s is read-only. Open it as root?"
;; buffer-file-name)))
;; (sudo))))

;; Pairing parentheses

;; All languages:
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)


;; Individual language pairings

;; Just python
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map "'" 'skeleton-pair-insert-maybe)))

;; just ruby
(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map "'" 'skeleton-pair-insert-maybe)
            (define-key ruby-mode-map "|" 'skeleton-pair-insert-maybe)))

;; just html
(add-hook 'html-mode-hook
          (lambda ()
            (define-key html-mode-map "<" 'skeleton-pair-insert-maybe)))

(set-face-attribute 'default nil :height 105)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq max-specpdl-size 99999999)

(setq enable-recursive-minibuffers t)
(follow-mode t)
(setq redisplay-dont-pause t)

;;Customizations from Ryan McGreary
(setq-default fill-column 72)
(setq auto-fill-mode 1)
;Line by line scrolling
(setq scroll-step 1)

;Show newlines at end of file
(define-fringe-bitmap 'empty-line [0 0 #x3c #x3c #x3c #x3c 0 0])
(set-default 'indicate-empty-lines nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makes loading org files take really long, so disabled for now
;; When turning on flyspell-mode, automatically check the entire buffer.
;; Why this isn't the default baffles me.
;(defadvice flyspell-mode (after advice-flyspell-check-buffer-on-start activate)
;  (flyspell-buffer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs title
;;(setq frame-title-format "emacs [%b %*%+ %f]"
;;      icon-title-format "emacs [%b]")

;; gui settings (misc)
;; block cursors are extraordinary ugly
(bar-cursor-mode t)

; delete seleted text when typing
(delete-selection-mode 1)

;; Line numbering
(setq linum-format "%4d")

(setq user-full-name "Daniel Newman"
      user-mail-address "dwnewman78@gmail.com")

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))

(setq diff-switches "-u")

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

(recentf-mode 1)

(show-paren-mode 1)


;; ido mode
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point t
        ido-max-prospects 10))
