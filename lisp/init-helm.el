(use-package helm-config
  :if (not running-alternate-emacs)
  :demand t
  :load-path "site-lisp/helm" 
  :bind (("C-c h" . helm-command-prefix)
         ("C-h a" . helm-apropos)
         ("C-h e a" . my-helm-apropos)
         ("C-x f" . helm-multi-files)
         ("M-s b" . helm-occur)
         ("M-s n" . my-helm-find)
         )
  :preface
  (defun my-helm-find ()
    (interactive)
    (helm-find nil))
  
  :config
  (use-package helm-commnads)
  (use-package helm-files)
  (use-package helm-buffers)
  (use-package helm-multi-match)
  (helm-autoresize-mode 1)
  (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
  (bind-key "C-i" #'helm-execute-persistent-action helm-map)
  (bind-key "C-z" #'helm-select-action helm-map)
  (bind-key "A-v" #'helm-previous-page helm-map)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t)))

(use-package helm-swoop
  :load-path "site-lisp/helm-swoop"
  :bind
  (("M-s o" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-)
   ("M-s /" . helm-multi-swoop))
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

(use-package helm-descbinds
  :load-path "site-lisp/helm-descbinds"
  :bind ("C-h b" . helm-descbinds)
  :init
  (fset 'describe-bindings 'helm-descbinds)
  :config
  (require 'helm-config))

(use-package helm-ag
  :load-path "site-lisp/helm-ag")

(provide 'init-helm)
