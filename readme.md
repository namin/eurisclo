# Eurisclo - A Common Lisp port of Doug Lenat's EURISKO

This is a work in progress.
It seems to work.

The two diffs of interests are:
- [Angle's working branch (as of July 2024) vs my wip branch diff](https://github.com/namin/eurisclo/compare/namin:eurisclo:working...namin:eurisclo:wip?expand=1).
- [Angle's wip branch vs my anngle-wip branch diff](https://github.com/namin/eurisclo/compare/namin:eurisclo:angle...namin:eurisclo:angle-my?expand=1).

[Angle's wip branch] is on Gitlab.

## How to run:

* Fire up a common lisp REPL in the src folder.
  For example: `C-u M-x slime` then `sbcl --control-stack-size 100`, but it turns out `M-x slime` is enough.

* Make sure you have Quicklisp loaded one way or another.

* Add a symlink in ~/quicklisp/local-projects/ to the eurisclo directory.

* Load eurisclo using quicklisp: `(ql:quickload "eurisclo")`

* Optionally: Enter the eurisclo package using `(in-package :eurisclo)`. If you don't do this, you will need to preface every reference with `eurisclo:`

* start Eurisko: `(eurisko)`. You can put a number as an argument to make the program more or less verbose in its descriptions (e.g. `100` for maximum verbosity), and can pass `t` after that to make it run in eternal mode, for example `(eurisko 100 t)`.

* Next it will ask to initialize. You probably want to do this, but so far as I can tell, nothing really changes if you don't.

* Next it asks to run. If you want to examine or change the initial state, you can answer no to this, and it will return you to the REPL. You can then start at a later point after you've performed whatever examinations or with `(start)`, optionally passing an eternal flag as an argument with `(start t)` if you want the program to run forever.

* Once you have the program running, you will probably encounter some kind of error you will need to debug. Enjoy!

License: (c) 2023-2024 White Flame, Angle, Nada Amin.
