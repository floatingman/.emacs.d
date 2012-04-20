;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows/Cygwin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs on Windows 7, M-x version output:
;;   GNU Emacs 24.0.50.1 (i386-mingw-nt6.1.7600)
;;   of 2011-01-17 on 3249CTO
;;
;; N.B. executed
;;
;;   setx HOME c:\cygwin\home\robert
;;
;; in cmd.exe to set the HOME environment variable

;; Adding cygwin stuff
(if (eq system-type 'windows-nt)
    (progn
      (setenv "PATH"
 	      (concat
 	       "c:\\cygwin\\usr\\local\\bin" ";"
 	       "c:\\cygwin\\usr\\bin" ";"
 	       "c:\\cygwin\\bin" ";"
 	       (getenv "PATH")))
      (setq exec-path (cons "c:/cygwin/bin/" exec-path))
      ;; Adding cygwin mounts
      (require 'cygwin-mount)
      (cygwin-mount-activate)
      ;; Adding cygwin bash shell
      (setq shell-file-name "c:/cygwin/bin/bash")
      (setenv "SHELL" shell-file-name)
      (setq explicit-shell-file-name shell-file-name)
      (setq ediff-shell shell-file-name)
      (setq explicit-shell-args '("--login" "-i"))
      (setq w32-quote-process-args ?\") ;"
      ))
