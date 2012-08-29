(require 'paredit)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)

(defun turn-on-paredit ()
  (paredit-mode +1))

(defface starter-kit-paren-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim parentheses."
  :group 'starter-kit-faces)

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'run-starter-kit-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'starter-kit-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)
(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

(defun starter-kit-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\|)" . 'starter-kit-paren-face)))

;; Scheme

(add-hook 'scheme-mode-hook 'run-starter-kit-coding-hook)
(font-lock-add-keywords 'scheme-mode
			'(("(\\|)" . 'starter-kit-paren-face)))

;; Common Lisp
(add-hook 'lisp-mode-hook 'run-starter-kit-coding-hook)
(add-hook 'lisp-mode-hook 'turn-on-paredit)
(font-lock-add-keywords 'lisp-mode
			'(("(\\|)" . 'starter-kit-paren-face)))
