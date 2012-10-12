;;; 50vbnetmode.el ---

;; Copyright (C) 2012 Free Software Foundation, Inc.
;;
;; Author:  <dnewman@WGS-DNEWMAN-D01>
;; Maintainer:  <dnewman@WGS-DNEWMAN-D01>
;; Created: 06 Mar 2012
;; Version: 0.01
;; Keywords

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:


;;; Code:

(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
    (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
                                  vbnet-mode)) auto-mode-alist))

   (defun my-vbnet-mode-fn ()
     "My hook for VB.NET mode"
     (interactive)
     ;; This is an example only.
     ;; These statements are not required to use VB.NET, but
     ;; you might like them.
     (turn-on-font-lock)
     (turn-on-auto-revert-mode)
     (require 'flymake)
     (flymake-mode 1)
   )
   (add-hook 'vbnet-mode-hook 'my-vbnet-mode-fn)


