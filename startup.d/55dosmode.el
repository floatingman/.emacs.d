(autoload 'dos-mode "dos" "Edit Dos scripts." t)
      (add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
