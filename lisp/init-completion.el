(use-package company
  :defer 2
  :bind ("<C-tab>" . company-complete)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package helm
  :diminish helm-mode
  :config
  (progn
		(setq helm-ff-transformer-show-only-basename nil
          helm-adaptive-history-file             "~/.emacs.d/helm-history"
          helm-yank-symbol-first                 t
          helm-move-to-line-cycle-in-source      t
          helm-buffers-fuzzy-matching            t
          helm-ff-auto-update-initial-value      t)

    (autoload 'helm-descbinds      "helm-descbinds" t)
    (autoload 'helm-eshell-history "helm-eshell"    t)
    (autoload 'helm-esh-pcomplete  "helm-eshell"    t)
    
    (require 'helm-config)
    (helm-mode t)
    (helm-adaptive-mode t)
    (setq helm-locate-command
          (case system-type
            ('gnu/linux "locate -i -r %s")
            ('berkeley-unix "locate -i %s")
            ('windows-nt "es %s")
            ('darwin "mdfind -name %s %s")
            (t "locate %s")))
    

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (define-key org-mode-map (kbd "C-x c o h") #'helm-org-headlines)
    
    )
  :bind (
         ("C-x b"     . helm-mini)
         ("C-x C-b"   . helm-buffers-list)
         ("C-x c g"   . helm-do-grep)
         ("C-h a"     . helm-apropos)
         ("C-h i"     . helm-info-emacs)
         ("M-y"       . helm-show-kill-ring)
         ("M-x"       . helm-M-x)
         ("C-x C-f"   . helm-find-files)
         ("C-x c o"   . helm-occur)
         ("M-s o"     . helm-swoop)
         ("C-x c y"   . helm-yas-complete)
         ("C-x c Y"   . helm-yas-create-snippet-on-region)
         ("C-x c SPC" . helm-all-mark-rings)
         ("C-x C-r"   . helm-recentf)
         ("M-s /"     . helm-multi-swoop)
         ("C-x c!"    . helm-calcul-expression)
         ("C-x c:"    . helm-eval-expression-with-eldoc)
         ("M-o"       . helm-previous-source)
         ))


(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-descbinds
  :init
  (progn
    (helm-descbinds-mode 1))
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package helm-swoop
  :bind
  (("C-S-s" . helm-swoop)
   ("M-i" . helm-swoop)
   ("M-s s" . helm-swoop)
   ("M-s M-s" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   )
  :config
  (progn
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))

(use-package helm-projectile
	:config
	(progn
		
		(helm-projectile-on)

		(define-key projectile-mode-map (kbd "C-c p /")
			#'(lambda ()
					(interactive)
					(helm-ag (projectile-project-root))))
		)
	)

(use-package helm-ag
	:bind(
				("M-s s" . helm-ag)))

(use-package helm-css-scss
  :config
  (progn
    (setq helm-css-scss-insert-close-comment-depth 2)
    (setq helm-css-scss-split-with-multiple-windows nil)
    (setq helm-css-scss-split-direction 'split-window-vertically)
    (dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
      (add-hook
       $hook (lambda ()
               (local-set-key (kbd "s-i") 'helm-css-scss)
               (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))
    (define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
    (define-key helm-css-scss-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)
    ))



(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :init (progn
          (yas-global-mode 1)
          (yas-reload-all)))


(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


(add-hook 'text-mode-hook 'abbrev-mode)
(diminish 'abbrev-mode " A")

(setq tab-always-indent 'complete)

(provide 'init-completion)
