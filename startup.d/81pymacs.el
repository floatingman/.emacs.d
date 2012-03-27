(pymacs-exec "import sys, os")
(pymacs-exec "sys.path.append(os.path.join(os.path.expanduser('~'),'.emacs.d','daniel-pymacs-extensions'))")

;; python libraries
;(setq pymacs-load-path (list))
;(add-subdirectories-to-load-path 'pymacs-load-path "~/.emacs.d/daniel-pymacs-extensions")

;; shortcut function to insert license headers
(pymacs-load "license")

;;Shorten URLs with is.gd
(pymacs-exec "import shorten_url")
(defun shorten-url (start end)
  (interactive "r")
  (let ((region (buffer-substring start end)))
    (let ((rt (pymacs-eval (format "shorten_url.shorten_in_text('''%s''')" region))))
      (kill-region start end)
      (insert rt)
      )
  ))
