(defun notmuch-file-to-group (file)
  "Calculate the Gnus group name from the given file name."
  (let ((group (file-name-directory (directory-file-name (file-name-directory file)))))
    (setq group (replace-regexp-in-string ".*/Maildir/" "nnimap+dnewman:" group))
    (setq group (replace-regexp-in-string "/$" "" group))
    (if (string-match ":$" group)
        (concat group "INBOX")
      (replace-regexp-in-string ":\\." ":" group))))

(defun notmuch-goto-message-in-gnus ()
  "Open a summary buffer containing the current notmuch
     article."
  (interactive)
  (unless (gnus-alive-p) (with-temp-buffer (gnus)))
  (let ((group (notmuch-file-to-group (notmuch-show-get-filename)))
        (message-id
         (replace-regexp-in-string "\"" ""
                                   (replace-regexp-in-string "^id:" ""
                                                             (notmuch-show-get-message-id)))))
    (if (and group message-id)
        (progn
          (gnus-summary-read-group group 1) ; have to show at least one old message
          (gnus-summary-refer-article message-id)) ; simpler than org-gnus method?
      (message "Couldn't get relevant infos for switching to Gnus."))))

(define-key notmuch-show-mode-map (kbd "C-c C-c") 'notmuch-goto-message-in-gnus)

(provide 'init-notmuchmail)
