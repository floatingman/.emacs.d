(cond ((eq window-system 'w32)
       (setq tramp-default-method "scpx"))
      (t
      (setq tramp-default-method "scpc")))
