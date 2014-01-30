;; rebind quiting keys to something more useful
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)


(provide 'init-key-bindings)
