(use-package elfeed
	:ensure t
	:config
	(progn
		(setq-default elfeed-search-filter "@1-month-ago +unread ")
		;; Entries older than 1 month are marked as read
		(add-hook 'elfeed-new-entry-hook
							(elfeed-make-tagger :before "2 weeks ago"
																	:remove 'unread))
		
		;; need to explore importing opml files and explore elfeed-org
		(setq elfeed-db-directory "~/.emacs.d/.elfeed"
      elfeed-max-connections 5
      
			)
		)
	)


(provide 'init-news)
