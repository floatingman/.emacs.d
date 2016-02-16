  (defun my/magit-commit-all ()
      "Publish the current file and commit all the current changes."
      (interactive)
			(magit-status default-directory)
      (magit-stage-all)
      (call-interactively 'magit-log-edit))

(use-package magit
	:ensure t
	:config
	(progn
		(setq magit-diff-options '("-b")) ; ignore whitespace
		(define-key magit-mode-map "#gg" 'endless/load-gh-pulls-mode)
		(defvar my/magit-limit-to-directory nil "Limit magit status to a specific directory.")
		(defun my/magit-status-in-directory (directory)
			"Displays magit status limited to DIRECTORY.
Uses the current `default-directory', or prompts for a directory
if called with a prefix argument. Sets `my/magit-limit-to-directory'
so that it's still active even after you stage a change. Very experimental."
			(interactive (list ( expand-file-name
													 (if current-prefix-arg
															 (read-directory-name "Directory: ")
														 default-directory))))
			(setq my/magit-limit-to-directory directory)
			(magit-status directory))
		(defadvice magit-insert-untracked-files (around sacha activate)
			(if my/magit-limit-to-directory
					(magit-with-section (section untracked 'untracked "Untracked files:" t)
															(let ((files (cl-mapcan
																						(lambda (f)
																							(when (eq (aref f 0) ??) (list f)))
																						(magit-git-lines
																						 "status" "--porcelain" "--" my/magit-limit-to-directory))))
																(if (not files)
																		(setq section nil)
																	(dolist (file files)
																		(setq file (magit-decode-git-path (substring file 3)))
																		(magit-with-section (section file file)
																												(insert "\t" file "\n")))
																	(insert "\n"))))
				ad-do-it))

		(defadvice magit-insert-staged-changes (around sacha activate)
          "Limit to `my/magit-limit-to-directory' if specified."
          (if my/magit-limit-to-directory
              (let ((no-commit (not (magit-git-success "log" "-1" "HEAD"))))
                (when (or no-commit (magit-anything-staged-p))
                  (let ((magit-current-diff-range (cons "HEAD" 'index))
                        (base (if no-commit
                                  (magit-git-string "mktree")
                                "HEAD"))
                        (magit-diff-options (append '("--cached") magit-diff-options)))
                    (magit-git-insert-section (staged "Staged changes:")
                        (apply-partially #'magit-wash-raw-diffs t)
                      "diff-index" "--cached" base "--" my/magit-limit-to-directory))))
            ad-do-it)))
      :bind (("C-x v d" . magit-status)
             ("C-x v C-d" . my/magit-status-in-directory)
             ("C-x v p" . magit-push)
             ("C-x v c" . my/magit-commit-all)))

;; From http://endlessparentheses.com/merging-github-pull-requests-from-emacs.html
(defun endless/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (magit-gh-pulls-mode 1)
  (magit-gh-pulls-reload))

(use-package magit-gh-pulls
  :ensure t )

(use-package git-messenger
  :ensure t
  :bind (("C-x v m" . git-messenger:popup-message)))

(use-package git-gutter+
  :ensure t
  :init (global-git-gutter+-mode)
  :config (progn
            (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
            (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
            (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
            (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)
						(setq git-gutter+-modified-sign "  ") ;; two space
						(setq git-gutter+-added-sign "++")    ;; multiple character is OK
						(setq git-gutter+-deleted-sign "--")
						
						(set-face-background 'git-gutter+-modified "purple") ;; background color
						(set-face-foreground 'git-gutter+-added "green")
						(set-face-foreground 'git-gutter+-deleted "red")
						)
  :diminish (git-gutter+-mode . "gg"))

(provide 'init-git)
