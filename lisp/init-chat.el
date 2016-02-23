;;; Code:
(defun start-irc ()
  (interactive)
  (when (file-exists-p "~/.ercpass")
    (load-file "~/.ercpass"))


	;; following Sacha Chua and http://writequit.org/org/settings.html#sec-1-13
	(use-package erc
		:ensure t 
		:config
		(progn
			(setq erc-fill-column 78
						erc-server-coding-system '(utf-8 . utf-8)
						erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
						erc-track-exclude-types (append '("324" "329" "332" "333"
																							"353" "477" "MODE")
																						erc-hide-list)
						erc-nick '("floatingman" "floatingman_" "floatingman__")
						erc-flood-protect nil
						erc-keywords '("floatingman" "floatingman_" "floatingman__" "daniel"
                           "daniel newman")
						erc-ignore-list '()
            erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                      "324" "329" "332" "333" "353" "477")
            erc-log-matches-types-alist
            '((keyword . "ERC keywords")
              (current-nick . "ERC messages for me"))
						erc-prompt-for-nickserv-password nil
            erc-server-reconnect-timeout 5
            erc-fill-function 'erc-fill-static
            erc-fill-static-center 18
						;; update ERC prompt with room name
            erc-prompt (lambda ()
                         (if (and (boundp 'erc-default-recipients)
                                  (erc-default-target))
                             (erc-propertize (concat (erc-default-target) ">")
                                             'read-only t 'rear-nonsticky t
                                             'front-nonsticky t)
                           (erc-propertize (concat "ERC>") 'read-only t
                                           'rear-nonsticky t
                                           'front-nonsticky t)))
						erc-autojoin-channels-alist '(("freenode.net"
																					 "#org-mode"
																					 "#emacs"
																					 "#emacs-beginners"))


						)

			

			(defun erc-cmd-OPME ()
				"Request chanserv to op me."
				(erc-message "PRIVMSG"
										 (format "chanserv op %s %s"
														 (erc-default-target)
														 (erc-current-nick)) nil))
			
			(defun erc-cmd-DEOPME ()
				"Deop myself from current channel."
				(erc-cmd-DEOP (format "%s" (erc-current-nick))))

			(defmacro de-erc-connect (command server port nick)
				"Create interactive command `command', for connecting to an IRC server. The
      command uses interactive mode if passed an argument."
				(fset command
							`(lambda (arg)
								 (interactive "p")
								 (if (not (= 1 arg))
										 (call-interactively 'erc)
									 (erc :server ,server :port ,port :nick ,nick)))))

			(defmacro asf-erc-bouncer-connect (command server port nick ssl pass)
				"Create interactive command `command', for connecting to an IRC server. The
   command uses interactive mode if passed an argument."
				(fset command
							`(lambda (arg)
								 (interactive "p")
								 (if (not (= 1 arg))
										 (call-interactively 'erc)
									 (let ((erc-connect-function ',(if ssl
																										 'erc-open-ssl-stream
																									 'open-network-stream)))
										 (erc :server ,server :port ,port :nick ,nick :password ,pass))))))
			
			(defun bitlbee-identify () 
				"If we're on the bitlbee server, send the identify command to the #bitlbee channel."
				(when (and (= 6667 erc-session-port)
									 (string= "&bitlbee" (buffer-name)))
					(erc-send-command (format "PRIVMSG &bitlbee :identify %s" bitlbee-password))))
			(add-hook 'erc-join-hook 'bitlbee-identify)
			)
		)


	(de-erc-connect erc-freenode "irc.freenode.net" 6667 "floatingman")
	(de-erc-connect erc-bitlbee "thenewmans.no-ip.org" 6667 "floatingman")
	(asf-erc-bouncer-connect erc-freenode "irc.freenode.net" 6667 "floatingman" t freenode-password)
	(asf-erc-bouncer-connect erc-gitter "irc.gitter.im" 6667 "floatingman" nil gitter-password)
	
	(use-package ercn
		:ensure t
		:config
		(progn
			(setq ercn-notify-rules
						'((current-nick . all)
							(keyword . all)
							(pal . ("#84115"))
							(query-buffer . all)))
			(defun do-notify (nickname message)
				(alert message :title (concat (buffer-name) ": " nickname)))
			(add-hook 'ercn-notify-hook #'do-notify)))

	;; connect irc
	(call-interactively 'erc-bitlbee)
	(sit-for 1)
	(call-interactively 'erc-freenode)
	(sit-for 1)
	)


(provide 'init-chat)
;;; init-chat ends here
