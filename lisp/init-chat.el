;;; Code:
(defun start-irc ()
  (interactive)
  (when (file-exists-p "~/personal/.ercpass")
    (load-file "~/personal/.ercpass"))


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
						erc-nick '("floatingman" "floatingman_1" "floatingman_2")
						erc-flood-protect nil
						erc-keywords '("floatingman" "floatingman_" "floatingman__" "daniel"
                           "daniel newman" "dnewman" "dwnewman78")
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
            erc-join-buffer 'bury
            
            )
      )
    )

  (define-key erc-mode-map (kbd "C-c C-q")
    (lambda (nick)
      (interactive (list (completing-read "Nick: " channel-members)))
      (erc-cmd-QUERY nick)))
  
  ;; (defadvice erc-display-prompt (after conversation-erc-display-prompt activate)
  ;;   "Insert last recipient after prompt."
  ;;   (let ((previous 
  ;;          (save-excursion 
  ;;            ((insert )f (and (search-backward-regexp (concat "^[^<]*<" erc-nick ">") nil t)
  ;;                             ((setq )earch-forward-regexp (concat "^[^<]*<" erc-nick ">" 
  ;;                                                                  " *\\([^:]*: ?\\)") nil t))
  ;;             (match-string 1)))))
  ;;     ;; when we got something, and it was in the last 3 mins, put it in
  ;;     (when (and 
  ;;            previous 
  ;;            (> 180 (time-to-seconds 
  ;;                    (time-since (get-text-property 0 'timestamp previous)))))
  ;;       (set-text-properties 0 (length previous) nil previous)
  ;;       (insert previous))))

  (defvar notify-command (executable-find "terminal-notifier") "The path to terminal-notifier")

  (defun notify (title message)
    "Shows a message through the Notification center system using
`notify-command` as the program."
    (flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))))
      (let* ((process (start-process "notify" nil
                                     notify-command
                                     "-title" (encfn title)
                                     "-message" (encfn message))))))
    t)

  (defun my-erc-hook (match-type nick message)
    "Show a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
    (unless (posix-string-match "^\\** *Users on #" message)
      (notify
       (concat "ERC: name mentioned on: " (buffer-name (current-buffer)))
       message)))

  (alert-add-rule :status   '(buried visible idle)
                  :severity '(moderate high urgent)
                  :mode     'erc-mode
                  :predicate
                  #'(lambda (info)
                      (string-match (concat "\\`[^&].*@BitlBee\\'")
                                    (erc-format-target-and/or-network)))
                  :persistent
                  #'(lambda (info)
                      ;; If the buffer is buried, or the user has been
                      ;; idle for `alert-reveal-idle-time' seconds,
                      ;; make this alert persistent.  Normally, alerts
                      ;; become persistent after
                      ;; `alert-persist-idle-time' seconds.
                      (memq (plist-get info :status) '(buried idle)))
                  :style 'fringe
                  :continue t)
  
  
  (when (bound-and-true-p notify-command)
    (add-hook 'erc'text-matched-hook 'my-erc-hook))
  
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
    (when (and (= 3344 erc-session-port)
               (string= "&bitlbee" (buffer-name)))
      (erc-send-command (format "PRIVMSG &bitlbee :identify %s" bitlbee-password))))
  (add-hook 'erc-join-hook 'bitlbee-identify) 

  ;; connect to znc
  (znc-all)


  ;; (asf-erc-bouncer-connect erc-znc "thenewmans.no-ip.org" 3344 "dnewman" t znc-password)
  ;; (de-erc-connect erc-freenode "irc.freenode.net" 6667 "floatingman")
  ;; (de-erc-connect erc-bitlbee "thenewmans.no-ip.org" 6667 "floatingman")
  ;; (asf-erc-bouncer-connect erc-freenode "irc.freenode.net" 6667 "floatingman" t freenode-password)
  ;; (asf-erc-bouncer-connect erc-gitter "irc.gitter.im" 6667 "floatingman" nil gitter-password)



  ;; connect irc
  ;; (call-interactively 'erc-bitlbee)
  ;; (sit-for 1)
  ;; (call-interactively 'erc-freenode)
  ;; (sit-for 1)
  
  )

(use-package znc
  :ensure t
  :defer t
  )

(use-package ercn
  :ensure t
  :defer t
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

(use-package erc-terminal-notifier
  :ensure t
  )


(provide 'init-chat)
;;; init-chat ends here
