This is my current emacs config.  Most of the time I use ELPA. For Windows users, it's best to install cygwin (http://cygwin.com) and install things like grep and aspell. When initially loading the config and all of the elpa packages have downloaded then uncomment the function call (bh/org-agenda-to-appt) at the end of init-org.el this has caused problems with Invalid function: org-with-silent-modifications.

I have found that I must manually install org mode from elpa then restart emacs, hopefully no errors, and then uncomment the (bh/org-agenda-to-appt) function call. That should fix it, but you might have to monkey with it to get it to work.

