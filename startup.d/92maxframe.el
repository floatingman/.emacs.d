(require 'maxframe)
(setq mf-max-width 1440)  ;; Pixel width of main monitor.
(add-hook 'window-setup-hook 'maximize-frame t)
