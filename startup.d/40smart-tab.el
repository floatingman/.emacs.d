(require 'smart-tab)

(setq smart-tab-completion-functions-alist
      '((emacs-lisp-mode . lisp-complete-symbol)
        (text-mode . dabbrev-completion)))

(setq smart-tab-disabled-major-modes
      '(org-mode term-mode inferior-python-mode rcirc-mode))

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol
        ))

(define-key read-expression-map [(tab)] 'hippie-expand)

(defun hippie-unexpand ()
  (interactive)
  (hippie-expand 0))

(define-key read-expression-map [(shift tab)] 'hippie-unexpand)


(setq smart-tab-using-hippie-expand t)

(global-smart-tab-mode nil)
