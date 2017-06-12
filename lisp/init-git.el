(use-package magit
  :load-path ("site-lisp/magit/lisp"
              "site-lisp/with-editor")
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix))
  :preface
  (defun magit-monitor (&optional no-display)
    "Start git-monitor in the current directory."
    (interactive)
    (when (string-match "\\*magit: \\(.+\\)" (buffer-name))
      (let ((name (format "*git-monitor: %s*"
                          (match-string 1 (buffer-name)))))

        (or (get-buffer name)
            (let ((buf (get-buffer-create name)))
              (ignore-errors
                (start-process "*git-monitor*" buf "git-monitor"
                               "-d" (expand-file-name default-directory)))
              buf)))))

  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'magit-status)))

  (defun lusty-magit-status (dir &optional switch-function)
    (interactive (list (if current-prefix-arg
                           (lusty-read-directory)
                         (or (magit-get-top-dir)
                             (lusty-read-directory)))))
    (magit-status-internal dir switch-function))

  (defun eshell/git (&rest args)
    (cond
     ((or (null args)
          (and (string= (car args) "status") (null (cdr args))))
      (magit-status-internal default-directory))
     ((and (string= (car args) "log") (null (cdr args)))
      (magit-log "HEAD"))
     (t (throw 'eshell-replace-command
               (eshell-parse-command
                "*git"
                (eshell-stringify-list (eshell-flatten-list args)))))))
    
  :init
  (add-hook 'magit-mode-hook 'hl-line-mode)

  (use-package with-editor
    ;; Magit makes use of this mode
    :demand t
    :commands (with-editor-async-shell-command
               with-editor-shell-command)
    :load-path "site-lisp/with-editor")

  (use-package git-commit)
  
  :config
  (setenv "GIT_PAGER" "")

  (unbind-key "M-h" magit-mode-map)
  (unbind-key "M-s" magit-mode-map)
  (unbind-key "M-m" magit-mode-map)
  (unbind-key "M-w" magit-mode-map)
  (unbind-key "<C-return>" magit-file-section-map)

  (diminish 'magit-wip-after-save-local-mode)
  (diminish 'magit-wip-after-apply-mode)
  (diminish 'magit-wip-before-change-mode)

  (bind-key "U" #'magit-unstage-all magit-mode-map)

  (add-hook 'magit-log-edit-mode-hook
            #'(lambda ()
                (set-fill-column 72)
                (flyspell-mode 1)))

  (add-hook 'magit-status-mode-hook #'(lambda () (magit-monitor t)))

  (remove-hook 'server-switch-hook 'magit-commit-diff))

(use-package git-gutter
  :ensure t
  :disabled t
  :defer t
  :bind (("C-x =" . git-gutter:popup-hunk)
         ("C-c P" . git-gutter:previous-hunk)
         ("C-c N" . git-gutter:next-hunk)
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-c G" . git-gutter:popup-hunk))
  :diminish ""
  :init
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (add-hook 'org-mode-hook 'git-gutter-mode))

(use-package git-gutter+
  :ensure t
  :defer t
  :disabled t
  :bind (("C-x n" . git-gutter+-next-hunk)
         ("C-x p" . git-gutter+-previous-hunk)
         ("C-x v =" . git-gutter+-show-hunk)
         ("C-x r"  . git-gutter+-revert-hunks)
         ("C-x t" . git-gutter+-stage-hunks)
         ("C-x U" . git-gutter+-unstage-whole-buffer)
         ("C-x =" . git-gutter+-popup-hunk))
  :config 
  (setq git-gutter+-modified-sign "  ") ;; two space
  (setq git-gutter+-added-sign "++")    ;; multiple character is OK
  (setq git-gutter+-deleted-sign "--")
  (set-face-background 'git-gutter+-modified "purple") ;; background color
  (set-face-foreground 'git-gutter+-added "green")
  (set-face-foreground 'git-gutter+-deleted "red")
  :diminish ""
  :init
  (add-hook 'prog-mode-hook 'git-gutter+-mode)
  (add-hook 'org-mode-hook 'git-gutter+-mode))

(provide 'init-git)
