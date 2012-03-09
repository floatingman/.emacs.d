;; Copied from Ryan Mcgreary(EnigmaCurry.blogspot.com)
;; My passwords are obviously not here

;Bip server
(setq bip-username "YourBipUsername")
(setq bip-passsword "Username:Password:Network")
;Twitter
(setq twit-user "YourTwitterUsername")
(setq twit-pass "YourTwitterPassword")
;Gmail
(setq gmail-user "YourGmailUsername")
(setq gmail-pass "YourGmailPassword")

;; You can also do as I do and keep them in a sepearte folder:
(add-to-list 'load-path "~/.emacs.private")
;;Load the passwords file but don't complain if it doesn't exist
(load "real-passwords" 'noerror)