(use-package erc
  :ensure t :defer t
  :config
  (setq erc-hide-list '("PART" "QUIT" "JOIN"))
  (setq erc-autojoin-channels-alist '(("freenode.net"
																			 "#org-mode"
																			 "#emacs"
																			 "#emacs-beginners"))
				erc-server "irc.freenode.net"
				erc-nick "floatingman")
  (defun erc-cmd-OPME ()
    "Request chanserv to op me."
    (erc-message "PRIVMSG"
                 (format "chanserv op %s %s"
                         (erc-default-target)
                         (erc-current-nick)) nil))
	
  (defun erc-cmd-DEOPME ()
    "Deop myself from current channel."
    (erc-cmd-DEOP (format "%s" (erc-current-nick)))))

	(erc :server "irc.freenode.net" :port 6667 :nick "floatingman")
	(erc :server "thenewmans.no-ip.org" :port 6667 :nick "floatingman")
	(provide 'init-chat)
