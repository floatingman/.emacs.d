(when *is-windows*
  (setenv "PATH" (concat "C:\\utils\\cygwin\bin;" (getenv "PATH")))
  (setq null-device "/dev/null")
  (setq find-program "C:\\utils\\cygwin\\bin\\find.exe"
        grep-program "C:\\utils\\cygwin\\bin\\grep.exe"))

(provide 'init-mswindows)
