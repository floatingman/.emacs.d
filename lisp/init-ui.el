;; Turn on the burn
(global-font-lock-mode t)

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1))

(use-package nlinum
  :ensure t
  :preface
  (defun goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line number input"
    (interactive)
    (unwind-protect
	(progn
	  (nlinum-mode 1)
	  (let ((num (read-number "Goto line: ")))
	    (goto-char (point-min))
	    (forward-line (1- num))))
      (nlinum-mode -1)))
  :init
  (bind-key "C-c g" #'goto-line)
  (global-set-key [remap goto-line] 'goto-line-with-feedback))

;;Stop the startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-face-attribute 'default nil :height 140)
(setq-default line-spacing 0)

(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(setq-default cursor-type 'bar)
(set-cursor-color "#cccccc")
(setq ring-bell-function 'ignore)


(provide 'init-ui)
