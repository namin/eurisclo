# Eurisclo - A Common Lisp port of Doug Lenat's EURISKO

This is a work in progress. It doesn't work. It might progress.


##How to make the program not work, in hope of progressing:

*Fire up a common lisp REPL in the src folder.

*Make sure you have Quicklisp loaded one way or another.

*Load eurisclo.lisp, probably via: `(LOAD "eurisclo.lisp")`

*Optionally: Enter the eurisclo package using `(in-package :eurisclo)`. If you don't do this, you will need to preface every reference with `eurisclo:`

*start Eurisko: `(eurisko)`. You can put a number as an argument to make the program more or less evrbose in it's descriptions, and can pass t after that to make it run in eternal mode, though you probably don't want to.

*Next it will ask to initialize. You probably want to do this, but so far as I can tell, nothing really changes if you don't.

*next it asks to run. If you want to examine or change the initial state, you can answer no to this, and it will return you to the REPL. You can then start at a later point after you've performed whatever examinations or with `(start)`, optionally passing an eternal flag as an argument with `(start t)` if you want the program to run forever.

*Once you have the program running, you will probably encounter some kind of error you will need to debug. Enjoy!

License: (c) 2023-2024 White Flame. Do not use it, in any way, for any purpose. Don't even read it. Yet.
