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

(use-package helm-descbinds
  :init
  (progn
    (helm-descbinds-mode 1))
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package helm-swoop
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-)
   ("C-c M-i" . helm-multi-swoop))
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t
        ;; If this value is t, split window inside the current window
        helm-swoop-split-with-multiple-windows t
        ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
        helm-swoop-split-direction 'split-window-vertically
        ;; If nil, you can slightly boost invoke speed in exchange for text color
        helm-swoop-speed-or-color nil))

(use-package helm-projectile
	:init
  (use-package grep) ;; required for helm-ag to work properly
  (setq projectile-completion-system 'helm)
  ;; no fuzziness for projectile-helm
  (setq helm-projectile-fuzzy-match nil)
  (helm-projectile-on))

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

(use-package helm-c-yasnippet
  :ensure nil
  :disabled t
  :bind
  (("M-=" . helm-yas-complete)))

(provide 'init-helm)
