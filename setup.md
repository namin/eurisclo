# Setting up the software stack for Eurisclo

## Stack

- a Common Lisp implementation (we use [Steele Bank Common Lisp (`sbcl`)](https://www.sbcl.org/) mostly and [Allegro Common Lisp (`alisp`)](https://franz.com/downloads/clp/survey) occasionally)
- the [Quicklisp](https://www.quicklisp.org/) library manager
- Emacs + Slime for interactive development.

### Setup SBCL

My `~/.sbclrc` has the following, the Quicklisp loading having been added automatically during the Quicklisp installation.

```
(sb-ext:restrict-compiler-policy 'debug 3)

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
```

### Setup Allegro Common Lisp

I don't typically load Quicklisp automatically for Allegro Common Lisp, so when running Eurisclo, I execute the following before the run instructions:

```
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(setq excl:*enable-package-locked-errors* nil)
```

### Setup Slime in Emacs

The easiest way to install Slime is to use a package manager. In your Emacs configuration (`~/.emacs.d/init.el`), start by installing one, e.g.:

```
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
```

Then install Slime from a freshly loaded Emacs, do `M-x package-refresh` followed by `M-x package-install` entering `slime`.

You can configure the default inferior lisp program to SBCL by adding the following to your Emacs configuration: `(setq inferior-lisp-program "sbcl")`.

To run Slime, `M-x slime`.

To run Slime with an ad-hoc inferior lisp program, prefix the Slime run command, `C-u M-x slime` entering your program, e.g. `alisp` (which must be in your `PATH`).

Some tips:

- When you run into a stack trace, you can press `v` on a particular step to jump to source.
- To get the results including output in a new Emacs buffer, You can evaluate an expression with `C-c C-p` instead of the usual `C-x C-e`.
  This is handy to get printed information while Eurisclo is running and printing away in the REPL.
