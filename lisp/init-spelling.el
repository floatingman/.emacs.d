(use-package flyspell
	:ensure t
	:bind (("C-c i b". flyspell-buffer)
				 ("C-c i f" . flyspell-mode))
	:init
	(use-package ispell
		:ensure t
		:bind (("C-c i c" . ispell-comments-and-strings)
					 ("C-c i d" . ispell-change-dictionary)
					 ("C-c i k" . ispell-kill-ispell)
					 ("C-c i m" . ispell-message)
					 ("C-c i r" . ispell-region)))
	:config
	(unbind-key "C-." flyspell-mode-map))

(provide 'init-spelling)
