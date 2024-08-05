# Eurisclo - A Common Lisp port of Doug Lenat's EURISKO

This is a work in progress.
It seems to work a bit.
See some [sample output](output.md).

The three diffs of interests are:
- [Angle's working branch (as of July 2024) vs my wip branch diff](https://github.com/namin/eurisclo/compare/namin:eurisclo:working...namin:eurisclo:wip?expand=1).
- [Angle's wip branch vs my angle-wip branch diff](https://github.com/namin/eurisclo/compare/namin:eurisclo:angle...namin:eurisclo:angle-my?expand=1).
- [Angle's heuristic-tracking branch vs my heuristic-tracking-my branch diff](https://github.com/namin/eurisclo/compare/namin:eurisclo:heuristic-tracking...namin:eurisclo:heuristic-tracking-my?expand=1).
- [Angle and my task-tracking branch vs my zombies branch diff](https://github.com/namin/eurisclo/compare/failed-task-tracking...namin:eurisclo:zombies?expand=1)

## Related links

- [Angle's work is on Gitlab](https://gitlab.com/AngularAngel/eurisclo/-/branches).
- [Interlisp Medley version](https://github.com/seveno4/EURISKO) -- runs out of the fixed stack space.
- [Wiki](https://github.com/white-flame/eurisko/wiki).

## [How to setup](setup.md)

## How to run (recommended commands)

```
(ql:quickload "eurisclo")
(in-package :eurisclo)
(eurisko 100 t)
;; wait ad infinitum...
(stop)
(print-run-info)
```

## How to run (step-by-step explanations)

* Fire up a common lisp REPL in the src folder.
  For example: `C-u M-x slime` then `sbcl --control-stack-size 1000  --dynamic-space-size 10000000"`, or just `M-x slime` for default config.

* Make sure you have Quicklisp loaded one way or another.

* Add a symlink in ~/quicklisp/local-projects/ to the eurisclo directory.

* Load eurisclo using quicklisp: `(ql:quickload "eurisclo")`

* Optionally: Enter the eurisclo package using `(in-package :eurisclo)`. If you don't do this, you will need to preface every reference with `eurisclo:`

* start Eurisko: `(eurisko)`. You can put a number as an argument to make the program more or less verbose in its descriptions (e.g. `100` for maximum verbosity), and can pass `t` after that to make it run in eternal mode, for example `(eurisko 100 t)`.

* Next it will ask to initialize. You probably want to do this, but so far as I can tell, nothing really changes if you don't.

* Next it asks to run. If you want to examine or change the initial state, you can answer no to this, and it will return you to the REPL. You can then start at a later point after you've performed whatever examinations or with `(start)`, optionally passing an eternal flag as an argument with `(start t)` if you want the program to run forever. As a second optional argument, you can pass it a thunk with a stop condition, for example `(lambda () (remove-if-not #'int-examples (remove 'binary-pred *units*)))` to stop when the condition has been satisfied. The example thunk stops when any interesting example is discovered (the unit `binary-pred` is removed, because it has some built-in interesting examples).

* Once you have the program running, you will probably encounter some kind of error you will need to debug. Enjoy!

License: (c) 2023-2024 White Flame, Angle, Nada Amin.
