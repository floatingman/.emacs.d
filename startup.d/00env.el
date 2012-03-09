(let ((bindirs (list
                (expand-file-name "~/.emacs.d/bin")
                (expand-file-name "~/bin"))))
  (dolist (dir bindirs)
    (setenv "PATH" (concat dir path-separator (getenv "PATH")))
    (add-to-list 'exec-path dir)))

(defun add-subdirectories-to-load-path (apath directory)
  "Add DIRECTORY and its immediate subdirectories to the list APATH."
  (add-to-list apath (file-truename directory))
  (let ((contents (directory-files directory t)))
    (dolist (filename contents)
      (let* ((basename (file-name-nondirectory filename))
             (is-hidden (string= (substring basename 0 1) ".")))
        (when (and (not is-hidden) (file-directory-p filename))
            (add-to-list apath (file-truename filename)))))))


;; python libraries
(setq pymacs-load-path (list))
(add-subdirectories-to-load-path 'pymacs-load-path "~/.emacs.d/daniel-pymacs-extensions")
