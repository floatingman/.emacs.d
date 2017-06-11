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
  (defun cygwin-shell ()
    "Run cygwin bash in shell mode."
    (interactive)
    (let ((explicit-shell-file-name "C:/cygwin64/bin/mintty.exe"))
      (call-interactively 'shell))))
(provide 'init-mswindows)
