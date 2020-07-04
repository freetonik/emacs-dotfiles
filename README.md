# Emacs configuration file

I used to store my Emacs config as an .org-file. On save, it used to run babel to generate the real init.el file, then byte-compile it to .elc in order to gain marginal improvement in startup time.

In an attempt to simplify every little aspect of my computing environent, I have replaced this system with a simple init.el file without babel-tangling and byte-compilation. I also disabled many things and cleaned up a few details.

The last iteration of the .org-file is `init.old.org`. It is not used and stays here for posterity.
