;; (use-package auto-complete
;;   :ensure t
;;   :diminish (auto-complete-mode . "AC")
;;   :init
;;   (use-package pos-tip
;;     :ensure t)
;;   (ac-config-default)
;;   :config
;;   (ac-set-trigger-key "TAB")
;; 	(ac-set-trigger-key "<tab>")
;;   (dolist (ac-mode '(text-mode org-mode))
;;     (add-to-list 'ac-modes ac-mode))
;;   (dolist (ac-mode-hook '(text-mode-hook org-mode-hook prog-mode-hook))
;;     (add-hook ac-mode-hook
;; 	      (lambda ()
;; 		(setq ac-fuzzy-enable t)
;; 		(add-to-list 'ac-sources 'ac-source-files-in-current-dir)
;; 		(add-to-list 'ac-sources 'ac-source-filename)))))

;; (defun ac-pcomplete ()
;;   ;; eshell uses `insert-and-inherit' to insert a \t if no completion
;;   ;; can be found, but this must not happen as auto-complete source
;;   (flet ((insert-and-inherit (&rest args)))
;;     ;; this code is stolen from `pcomplete' in pcomplete.el
;;     (let* (tramp-mode ;; do not automatically complete remote stuff
;;            (pcomplete-stub)
;;            (pcomplete-show-list t) ;; inhibit patterns like * being deleted
;;            pcomplete-seen pcomplete-norm-func
;;            pcomplete-args pcomplete-last pcomplete-index
;;            (pcomplete-autolist pcomplete-autolist)
;;            (pcomplete-suffix-list pcomplete-suffix-list)
;;            (candidates (pcomplete-completions))
;;            (beg (pcomplete-begin))
;;            ;; note, buffer text and completion argument may be
;;            ;; different because the buffer text may bet transformed
;;            ;; before being completed (e.g. variables like $HOME may be
;;            ;; expanded)
;;            (buftext (buffer-substring beg (point)))
;;            (arg (nth pcomplete-index pcomplete-args)))
;;       ;; we auto-complete only if the stub is non-empty and matches
;;       ;; the end of the buffer text
;;       (when (and (not (zerop (length pcomplete-stub)))
;;                  (or (string= pcomplete-stub ; Emacs 23
;;                               (substring buftext
;;                                          (max 0
;;                                               (- (length buftext)
;;                                                  (length pcomplete-stub)))))
;;                      (string= pcomplete-stub ; Emacs 24
;;                               (substring arg
;;                                          (max 0
;;                                               (- (length arg)
;;                                                  (length pcomplete-stub)))))))
;;         ;; Collect all possible completions for the stub. Note that
;;         ;; `candidates` may be a function, that's why we use
;;         ;; `all-completions`.
;;         (let* ((cnds (all-completions pcomplete-stub candidates))
;;                (bnds (completion-boundaries pcomplete-stub
;;                                             candidates
;;                                             nil
;;                                             ""))
;;                (skip (- (length pcomplete-stub) (car bnds))))
;;           ;; We replace the stub at the beginning of each candidate by
;;           ;; the real buffer content.
;;           (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
;;                   cnds))))))

;; (defvar ac-source-pcomplete
;;   '((candidates . ac-pcomplete)))

;; ;autocomplete backends
;; (use-package ac-html :ensure t)
;; (use-package ac-emmet :ensure t)
;; (use-package ac-html-angular :ensure t)
;; (use-package ac-html-bootstrap :ensure t)
;; (use-package ac-html-csswatcher :ensure t)
;; (use-package ac-js2
;; 	:ensure t
;;   :defer t
;; 	:config 
;; 	(progn
;; 		(add-hook 'js2-mode-hook 'ac-js2-mode)))

;; (use-package tern-auto-complete
;; 	:ensure t
;; 	:config
;; 	(progn
;; 		(require 'tern-auto-complete)
;; 		(tern-ac-setup)))

(use-package company
  :ensure t
  :defer 2
  :bind ("<C-tab>" . company-complete)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (progn
		(setq helm-ff-transformer-show-only-basename nil
          helm-adaptive-history-file             "~/.emacs.d/helm-history"
          helm-yank-symbol-first                 t
          helm-move-to-line-cycle-in-source      t
          helm-buffers-fuzzy-matching            t
          helm-ff-auto-update-initial-value      t)

    (add-hook 'eshell-mode-hook
							#'(lambda ()
									(define-key eshell-mode-map (kbd "TAB") #'helm-esh-pcomplete)
									(define-key eshell-mode-map (kbd "C-c C-l") #'helm-eshell-history)))
    
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
  :ensure t
  :init
  (progn
    (helm-descbinds-mode 1))
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package helm-swoop
 :ensure t
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
	:ensure t
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
	:ensure t
	:bind(
				("M-s s" . helm-ag)))

(use-package helm-css-scss
  :ensure t
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
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
  (progn
		(setq yas-installed-snippets-dir "~/.emacs.d/site-lisp/yasnippet-snippets")
		(yas-global-mode 1)))


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
