(setq ispell-program-name "~/.emacs.d/Aspell/bin/aspell.exe")
(menu-bar-mode -1)
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1))

(setq browse-url-browser-function 'browse-url-default-windows-browser)
