;;Backups

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
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

;; Window configuration
;; (when window-system
;;  (tooltip-mode -1)
;;  (tool-bar-mode -1)
;;  (menu-bar-mode 1)
;;  (scroll-bar-mode -1))


;;Help
(use-package guide-key
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
    (guide-key-mode 1)))

;; Moving between windows
(use-package windmove
  :ensure t
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   ))

;; Split windows by golden ratio
(use-package golden-ratio
	:ensure t
	:diminish golden-ratio-mode
	:config
	(progn
		(golden-ratio-mode 1)))

(use-package switch-window
  :ensure t
  :bind (("C-x o" . switch-window)))

;;Recent files found at awesome site http://writequit.org/org/settings.html
(use-package recentf
  :defer t
  :init
  (progn
    (setq recentf-max-saved-items 300
          recentf-exclude '("/auto-install/" ".recentf" "/repos/" "/elpa/"
                            "\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"
                            ".gz"
                            "~$" "/tmp/" "/ssh:" "/sudo:" "/scp:")
          recentf-auto-cleanup 600)
    (when (not noninteractive) (recentf-mode 1))

    (defun recentf-save-list ()
      "Save the recent list.
Load the list from the file specified by `recentf-save-file',
merge the changes of your current session, and save it back to
the file."
      (interactive)
      (let ((instance-list (cl-copy-list recentf-list)))
        (recentf-load-list)
        (recentf-merge-with-default-list instance-list)
        (recentf-write-list-to-file)))
    (defun recentf-merge-with-default-list (other-list)
      "Add all items from`other-list' to `recentf-list'."
      (dolist (oitem other-list)
        ;; add-to-list already checks for equal'ity
        (add-to-list 'recentf-list oitem)))

    (defun recentf-write-list-to-file ()
      "Write the recent files list to file.
Uses `recentf-list' as the list and `recentf-save-file' as the
file to write to."
      (condition-case error
          (with-temp-buffer
            (erase-buffer)
            (set-buffer-file-coding-system recentf-save-file-coding-system)
            (insert (format recentf-save-file-header (current-time-string)))
            (recentf-dump-variable 'recentf-list recentf-max-saved-items)
            (recentf-dump-variable 'recentf-filter-changer-current)
            (insert "\n \n;;; Local Variables:\n"
                    (format ";;; coding: %s\n" recentf-save-file-coding-system)
                    ";;; End:\n")
            (write-file (expand-file-name recentf-save-file))
            (when recentf-save-file-modes
              (set-file-modes recentf-save-file recentf-save-file-modes))
            nil)
        (error
         (warn "recentf mode: %s" (error-message-string error)))))))

;; reload buffers when file changes on disk
(global-auto-revert-mode t)

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
  :ensure t
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

(defface visible-mark-active ;; put this before (use-package visible-mark)
  '((((type tty) (class mono)))
    (t (:background "magenta"))) "")

(use-package visible-mark
  :init (global-visible-mark-mode 1)
  :config
  (progn
    (setq visible-mark-max 2)
    (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))))

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
(when (functionp 'mouse-wheel-mode)
  (mouse-wheel-mode -1))
(when (functionp 'tooltip-mode)
  (tooltip-mode -1))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

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


(provide 'init-general)
