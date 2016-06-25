# My Anti-monolithic Emacs Config

## About
I got tired of my big fat emacs config file.  I felt that my config was growing out of control and it was increasingly hard to make changes or try out new packages.  My config was really just a missmash of other's configs and this is my attempt to make my config my own.  Feel free to copy it :)

## Installing
1. git clone repository
2. git submodule init
3. git submodule update
4. Install texinfo and texi2html with package manager.
4. cd into override/org-mode and run make
5. cd into override/cedet and run make or on Windows run "emacs -Q -l cedet-build.el -f cedet-build" instead of make

## Caveats
Initially or when deleting the elpa directory, I have to start and stop emacs a few times to make sure everything gets installed.  I also have to delete the org mode folder in the elpa directory, which gets installed because of another dependency I believe. 
Honestly, I don't use this much on Windows anymore **so your mileage may vary**.

## Customizations 
I have a symbolic linked directory, "personal", in my home directory to a dropbox folder I use for synchronizing my customizations, news feeds, passwords, etc.
