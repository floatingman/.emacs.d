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
      ;; Remove C-m (^M) characters that appear in output

      (add-hook 'comint-output-filter-functions
                'comint-strip-ctrl-m)
      (setq ediff-shell shell-file-name)
      (setq explicit-shell-args '("--login" "-i"))
      (setq w32-quote-process-args ?\") ;"
      (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
        "Use cygwin's /dev/null as the null-device."
        (let ((null-device "/dev/null"))
          ad-do-it))
      (ad-activate 'grep-compute-defaults)
      (require 'w32-browser)
      (require 'dired+)
      ))
