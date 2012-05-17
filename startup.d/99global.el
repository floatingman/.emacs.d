(global-font-lock-mode 1)
(windmove-default-keybindings)
(winner-mode 1)
(savehist-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(ido-mode 1)
(transient-mark-mode 1)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(setq visible-bell t)
(setq ido-enable-flex-matching t)
(column-number-mode 1)
(global-auto-revert-mode t)
(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (or (buffer-file-name) (buffer-name)))))
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq make-backup-files nil)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq backup-directory-alist '(("." . "~/emacs-meta/backups")))
(setq auto-save-default nil)

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(setq warning-suppress-types nil)



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
(global-set-key "<" 'skeleton-pair-insert-maybe)

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
(setq frame-title-format "emacs [%b %*%+ %f]"
      icon-title-format "emacs [%b]")

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
