;; mutt-alias.el
(autoload 'mutt-alias-insert "mutt-alias"
  "Insert the expansion for ALIAS into the current buffer."
  t)
(autoload 'mutt-alias-lookup "mutt-alias"
  "Lookup and display the expansion for ALIAS."
  t)

;; muttrc-mode.el
(add-to-list 'auto-mode-alist '("muttrc" . muttrc-mode))

