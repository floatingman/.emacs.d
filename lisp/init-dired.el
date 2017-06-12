(defun my/dired-mode-hook ()
  (toggle-truncate-lines 1))

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (progn
    (use-package dired-x
      :init (setq-default dired-omit-files-p t)
      :config
      (add-to-list 'dired-omit-extensions ".DS_Store"))
    (customize-set-variable 'diredp-hide-details-initially-flag nil)
    (use-package dired+
      :ensure t)
    (use-package dired-aux
      :init (use-package dired-async))
    (put 'dired-find-alternate-file 'disabled nil)
    (setq ls-lisp-dirs-first t
          dired-recursive-copies 'always
          dired-recursive-deletes 'always
          dired-dwim-target t
          ;; -F marks links with @
          dired-ls-F-marks-symlinks t
          delete-by-moving-to-trash t
          ;; Auto refresh dired
          global-auto-revert-non-file-buffers t
          wdired-allow-to-change-permissions t)
    (when *is-a-mac* (setq trash-directory "~/.Trash"))          
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "C-M-u") 'dired-up-directory)
    (define-key dired-mode-map (kbd "M-o") #'my/dired-open)
    (define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
    (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
    (define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)
    (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
    (define-key dired-mode-map (vector 'remap 'smart-down) 'dired-jump-to-bottom)
    (define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)
    (add-hook 'dired-mode-hook #'my/dired-mode-hook)))


;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

;;(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
;;(define-key dired-mode-map (kbd "k") 'dired-do-delete)

;; M-up is nicer in dired if it moves to the fourth line - the first file
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 3))


;; M-down is nicer in dired if it moves to the last file
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))


;; peep-dired seems cool
(use-package peep-dired
  :ensure t
  :defer t ; don't acccess `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; Delete with C-x C-k to match file buffers and magit



(provide 'init-dired)
