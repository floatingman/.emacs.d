(load-file "~/.emacs.d/.emacs-secrets.el")

(use-package erc
  :ensure t :defer t
  :config
  (setq erc-hide-list '())
	(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477" "VOICE" "DEVOICE"))
	(setq erc-autojoin-channels-alist '(("freenode.net"
																			 "#org-mode"
																			 "#emacs"
																			 "#emacs-beginners")
																			("thenewmans.no-ip.org"
																			 "#BeWell_MS"
																			 "#BeWell_DevOps"
																			 "#BeWell_General"
																			 "#BeWell_SQA"
																			 "#KornerShop_QA"
																			 "#QA_Perf_Testing"
																			 "#ProdRelease"
																			 "#Random"))
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


;;(erc :server "irc.freenode.net" :port 6667 :nick "floatingman")
(erc :server "thenewmans.no-ip.org" :port 6667 :nick "floatingman")

(defun bitlbee-identify () 
  "If we're on the bitlbee server, send the identify command to the #bitlbee channel."
  (when (and (= 6667 erc-session-port)
						 (string= "&bitlbee" (buffer-name)))
    (erc-send-command (format "PRIVMSG &bitlbee :identify %s" bitlbee-password))))
(add-hook 'erc-join-hook 'bitlbee-identify)

(provide 'init-chat)
