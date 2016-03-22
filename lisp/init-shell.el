(setenv "PAGER" "cat")

(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 ;; '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () "")) ; what to run when i press enter on a
                                        ; line above the current prompt
 )

(defun my/shell-kill-buffer-sentinel (process event)
  (when (memq (process-status process) '(exit signal))
    (kill-buffer)))

(defun my/kill-process-buffer-on-exit ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'my/shell-kill-buffer-sentinel))

(dolist (hook '(ielm-mode-hook term-exec-hook comint-exec-hook))
  (add-hook hook 'my/kill-process-buffer-on-exit))

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))

(defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
        (progn (fset 'message 'ignore) ad-do-it)
      (fset 'message old-message))))

(add-hook 'shell-mode-hook 'set-scroll-conservatively)
;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
;; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;eshell
(defun my/setup-eshell ()
  (interactive)
  ;; turn off semantic-mode in eshell buffers
  (semantic-mode -1)
  ;; turn off hl-line-mode
  (hl-line-mode -1)
  (define-key eshell-mode-map (kbd "M-l")
    'helm-eshell-history))

(use-package eshell
  :config
  (progn
    (defalias 'emacs 'find-file)
    (defalias 'ec 'find-file)
    (setenv "PAGER" "cat")
    (use-package esh-opt
      :config
      (progn
        (use-package em-cmpl)
        (use-package em-prompt)
        (use-package em-term)

        (setq eshell-cmpl-cycle-completions nil
              ;; auto truncate after 12k lines
              eshell-buffer-maximum-lines 12000
              ;; history size
              eshell-history-size 350
              ;; buffer shorthand -> echo foo > #'buffer
              eshell-buffer-shorthand t
              ;; my prompt is easy enough to see
              eshell-highlight-prompt nil
              ;; treat 'echo' like shell echo
              eshell-plain-echo-behavior t)

        ;; Visual commands
        (setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                                       "ncftp" "pine" "tin" "trn" "elm" "vim"
                                       "nmtui" "alsamixer" "htop" "el" "elinks"
                                       ))
        (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))

        (defun my/truncate-eshell-buffers ()
          "Truncates all eshell buffers"
          (interactive)
          (save-current-buffer
            (dolist (buffer (buffer-list t))
              (set-buffer buffer)
              (when (eq major-mode 'eshell-mode)
                (eshell-truncate-buffer)))))

        ;; After being idle for 5 seconds, truncate all the eshell-buffers if
        ;; needed. If this needs to be canceled, you can run `(cancel-timer
        ;; my/eshell-truncate-timer)'
        (setq my/eshell-truncate-timer
              (run-with-idle-timer 5 t #'my/truncate-eshell-buffers))

        (when (not (functionp 'eshell/rgrep))
          (defun eshell/rgrep (&rest args)
            "Use Emacs grep facility instead of calling external grep."
            (eshell-grep "rgrep" args t)))

        (defun eshell/cds ()
          "Change directory to the project's root."
          (eshell/cd (locate-dominating-file default-directory ".git")))

        (defun eshell/l (&rest args) "Same as `ls -lh'"
               (apply #'eshell/ls "-lh" args))
        (defun eshell/ll (&rest args) "Same as `ls -lh'"
               (apply #'eshell/ls "-lh" args))
        (defun eshell/la (&rest args) "Same as `ls -alh'"
               (apply #'eshell/ls "-alh" args))

        (defun eshell/clear ()
          "Clear the eshell buffer"
          (interactive)
          (let ((eshell-buffer-maximum-lines 0))
            (eshell-truncate-buffer)))))
    
    (add-hook 'eshell-mode-hook #'my/setup-eshell)

    ;; See eshell-prompt-function below
    (setq eshell-prompt-regexp "^[^#$\n]* [#$] ")

    ;; So the history vars are defined
    (require 'em-hist)
    (if (boundp 'eshell-save-history-on-exit)
        ;; Don't ask, just save
        (setq eshell-save-history-on-exit t))
    (if (boundp 'eshell-ask-to-save-history)
        ;; For older(?) version
        (setq eshell-ask-to-save-history 'always))

    ;; See: https://github.com/kaihaosw/eshell-prompt-extras
    (use-package eshell-prompt-extras
      :init
      (progn
        (setq eshell-highlight-prompt nil
              ;; epe-git-dirty-char "Ϟ"
              epe-git-dirty-char "*"
              eshell-prompt-function 'epe-theme-dakrone)))

    (defun eshell/magit ()
      "Function to open magit-status for the current directory"
      (interactive)
      (magit-status default-directory)
      nil)))

(defun my/create-or-switch-to-delta-buffer ()
  "Switch to the *eshell delta* buffer, or create it"
  (interactive)
  (if (get-buffer "*eshell-delta*")
      (switch-to-buffer "*eshell-delta*")
    (let ((eshell-buffer-name "*eshell-delta*"))
      (eshell))))

(global-set-key (kbd "C-x d") 'my/create-or-switch-to-delta-buffer)

(defun my/create-or-switch-to-eshell-1 ()
  "Switch to the *eshell* buffer, or create it"
  (interactive)
  (if (get-buffer "*eshell*")
      (switch-to-buffer "*eshell*")
    (let ((eshell-buffer-name "*eshell*"))
      (eshell))))

(defun my/create-or-switch-to-eshell-2 ()
  "Switch to the *eshell*<2> buffer, or create it"
  (interactive)
  (if (get-buffer "*eshell*<2>")
      (switch-to-buffer "*eshell*<2>")
    (let ((eshell-buffer-name "*eshell*<2>"))
      (eshell))))

(defun my/create-or-switch-to-eshell-3 ()
  "Switch to the *eshell*<3> buffer, or create it"
  (interactive)
  (if (get-buffer "*eshell*<3>")
      (switch-to-buffer "*eshell*<3>")
    (let ((eshell-buffer-name "*eshell*<3>"))
      (eshell))))

(global-set-key (kbd "H-1") 'my/create-or-switch-to-eshell-1)
(global-set-key (kbd "H-2") 'my/create-or-switch-to-eshell-2)
(global-set-key (kbd "H-3") 'my/create-or-switch-to-eshell-3)
(global-set-key (kbd "s-1") 'my/create-or-switch-to-eshell-1)
(global-set-key (kbd "s-2") 'my/create-or-switch-to-eshell-2)
(global-set-key (kbd "s-3") 'my/create-or-switch-to-eshell-3)
(global-set-key (kbd "M-1") 'my/create-or-switch-to-eshell-1)
(global-set-key (kbd "M-2") 'my/create-or-switch-to-eshell-2)
(global-set-key (kbd "M-3") 'my/create-or-switch-to-eshell-3)

(use-package tramp
  :defer t
  :config
  (progn
    (with-eval-after-load 'tramp-cache
      (setq tramp-persistency-file-name "~/.emacs.d/etc/tramp"))
    (setq tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
          ;;tramp-adb-program "/Users/hinmanm/android-sdk-macosx/platform-tools/adb"
          ;; use the settings in ~/.ssh/config instead of Tramp's
          tramp-use-ssh-controlmaster-options nil
          backup-enable-predicate
          (lambda (name)
            (and (normal-backup-enable-predicate name)
                 (not (let ((method (file-remote-p name 'method)))
                        (when (stringp method)
                          (member method '("su" "sudo"))))))))

    (use-package tramp-sh
      :config
      (progn
        (add-to-list 'tramp-remote-path "/usr/local/sbin")
        (add-to-list 'tramp-remote-path "/opt/java/current/bin")
        (add-to-list 'tramp-remote-path "~/bin")))))

(provide 'init-shell)
