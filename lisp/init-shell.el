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
