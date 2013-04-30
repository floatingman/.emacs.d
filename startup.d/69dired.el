;; Handle zip compression
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))))))


;; Use details
(add-hook 'dired-load-hook 'my-dired-load-hook)
(defun my-dired-load-hook ()
  (if (require 'dired-details+ nil t)
      (setq dired-details-propagate-flag nil)
    (progn (message "Notice: dired-details+ not present")
           ;; If we don't have dired-details+, at least try for
           ;; dired-details alone
           (if (require 'dired-details nil t)
               (dired-details-install)
             (message "Notice: dired-details not present"))))
  (setq-default diredful-init-file "~/.emacs-diredful.el")
  ;; We can function on systems without diredful, even though it's
  ;; nice to have
  (if (not (require 'diredful nil t))
      (message "Notice: diredful not present; not using extended highlighting")))
