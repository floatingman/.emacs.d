(use-package js2-mode
  :ensure t
  :config
  (bind-key "C-c C-c" 'compile js2-mode-map)
  (add-hook 'js2-mode-hook 'jasminejs-mode))

(use-package coffee-mode
  :ensure t
  :config
  (bind-key "C-c C-c" 'compile coffee-mode-map))

(use-package jasminejs-mode
  :ensure t
  :config
  (add-hook 'jasminejs-mode-hook 'jasminejs-add-snippets-to-yas-snippet-dirs))

(add-to-list 'auto-mode-alist '("\\.js\\'\\|\\.json\\'" . js2-mode))

(defvar my/javascript-test-regexp (concat (regexp-quote "/** Testing **/") "\\(.*\n\\)*")
	"Regular expression matching testing-related code to remove.
See `my/copy-javascript-region-or-buffer'.")

(defun my/copy-javascript-region-or-buffer (beg end)
	"Copy the active region or the buffer, wrapping it in script tags.
Add a comment with the current filename and skip test-related
code. See `my/javascript-test-regexp' to change the way
test-related code is detected."
	(interactive "r")
	(unless (region-active-p)
		(setq beg (point-min) end (point-max)))
	(kill-new
	 (concat
		"<script type=\"text/javascript\">\n"
		(if (buffer-file-name) (concat "// " (file-name-nondirectory (buffer-file-name)) "\n") "")
		(replace-regexp-in-string
		 my/javascript-test-regexp
		 ""
		 (buffer-substring (point-min) (point-max))
		 nil)
		"\n</script>")))

(defvar my/debug-counter 1)
(defun my/insert-or-flush-debug (&optional reset beg end)
  (interactive "pr")
  (cond
   ((= reset 4)
    (save-excursion
      (flush-lines "console.log('DEBUG: [0-9]+" (point-min) (point-max))
      (setq my/debug-counter 1)))
   ((region-active-p)
    (save-excursion
      (goto-char end)
      (insert ");\n")
      (goto-char beg)
      (insert (format "console.log('DEBUG: %d', " my/debug-counter))
      (setq my/debug-counter (1+ my/debug-counter))
      (js2-indent-line)))
   (t
    ;; Wrap the region in the debug
    (insert (format "console.log('DEBUG: %d');\n" my/debug-counter))
    (setq my/debug-counter (1+ my/debug-counter))
    (backward-char 3)
    (js2-indent-line))))

(use-package js2-mode
  :ensure t
  :defer t
  :commands js2-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
    (setq-default js2-basic-offset 2)
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode)))
  :config
  (progn
    (js2-imenu-extras-setup)
    (bind-key "C-x C-e" 'js-send-last-sexp js2-mode-map)
    (bind-key "C-M-x" 'js-send-last-sexp-and-go js2-mode-map)
    (bind-key "C-c b" 'js-send-buffer js2-mode-map)
    (bind-key "C-c d" 'my/insert-or-flush-debug js2-mode-map)
    (bind-key "C-c C-b" 'js-send-buffer-and-go js2-mode-map)
    (bind-key "C-c w" 'my/copy-javascript-region-or-buffer js2-mode-map)))

(use-package coffee-mode
:ensure t
:defer t
:config (setq-default coffee-js-mode 'js2-mode coffee-tab-width 2))

(provide 'init-javascript)
