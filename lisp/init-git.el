(use-package magit
  :ensure t
  :bind (("M-g M-g" . magit-status)
         ("C-x g" . magit-status))
  :init (add-hook 'magit-mode-hook 'hl-line-mode)
  :config
  (setenv "GIT_PAGER" "")
  (if (file-exists-p  "/usr/local/bin/emacsclient")
      (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")
    (setq magit-emacsclient-executable (executable-find "emacsclient")))
  (defun my/magit-browse ()
    "Browse to the project's github URL, if available"
    (interactive)
    (let ((url (with-temp-buffer
                 (unless (zerop (call-process-shell-command
                                 "git remote -v" nil t))
                   (error "Failed: 'git remote -v'"))
                 (goto-char (point-min))
                 (when (re-search-forward
                        "github\\.com[:/]\\(.+?\\)\\.git" nil t)
                   (format "https://github.com/%s" (match-string 1))))))
      (unless url
        (error "Can't find repository URL"))
      (browse-url url)))

  (define-key magit-mode-map (kbd "C-c C-b") #'my/magit-browse)
  ;; Magit has its own binding, so re-bind them
  (bind-key "M-1" #'my/create-or-switch-to-eshell-1 magit-mode-map)
  (bind-key "M-2" #'my/create-or-switch-to-eshell-2 magit-mode-map)
  (bind-key "M-3" #'my/create-or-switch-to-eshell-3 magit-mode-map)
  (bind-key "M-4" #'my/create-or-switch-to-eshell-4 magit-mode-map))

(use-package magit-gh-pulls
  :ensure t)

(use-package git-gutter
  :ensure t
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
