(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-mode t)
  (make-variable-buffer-local 'whole-line-or-region-mode))

(use-package move-dup
  :ensure t
  :bind (([M-up] . md/move-lines-up)
	 ([M-down] . md/move-lines-down)
	 ([M-S-up] . md/move-lines-up)
	 ([M-S-down] . md/move-lines-down)
	 ("C-c d" . md/duplicate-down)
	 ("C-c D" . md/duplicate-up)))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))





(provide 'init-utils)
