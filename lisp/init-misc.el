(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq create-lockfiles nil)

(use-package guide-key
  :ensure t
  :config
  (add-to-list
   'guide-key/guide-key-sequence "C-c p"))

(provide 'init-misc)
