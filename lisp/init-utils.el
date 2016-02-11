
(use-package engine-mode
  :ensure t
  :config
  (progn
    (defengine mail "https://mail.google.com/mail/u/0/#search/%s" :keybinding "m")
    (defengine google "http://google.com/search?q=%s" :keybinding "g")
    (defengine emacswiki "http://google.com/search?q=site:emacswiki.org+%s" :keybinding "e")
    (bind-key* "C-c /" 'my/engine-mode-hydra/body)
    (engine-mode)))

(provide 'init-utils)
