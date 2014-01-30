(require 'init-mswindows)
(require 'init-skewer)
(require 'init-html)
(require 'init-key-bindings)
(require 'init-octave)
(require 'init-os-keys)



(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'init-local)
