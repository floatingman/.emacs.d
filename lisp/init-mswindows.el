(when *is-windows*
  (setenv "PATH"
          (concat
           "C:/cygwin64/usr/local/bin" ";"
           "C:/cygwin64/usr/bin" ";"
           "C:/cygwin64/bin" ";"
           (getenv "PATH") ;
           )
          )
  (setq null-device "/dev/null")
  (setq ispell-program-name "C:\\cygwin64\\bin\\aspell.exe")
  (setq find-program "C:\\cygwin64\\bin\\find.exe"
        grep-program "C:\\cygwin64\\bin\\grep.exe")
  )
(provide 'init-mswindows)
