(when *is-windows*
  (setenv "PATH" (concat "C:\\cygwin\bin;" (getenv "PATH")))
  (setq null-device "/dev/null")
  (setq find-program "C:\\cygwin\\bin\\find.exe"
        grep-program "C:\\cygwin\\bin\\grep.exe"))

(provide 'init-mswindows)
