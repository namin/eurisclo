;; Everything is an ANYTHING. If there's no generalizations, then its only parent is an ANYTHING. Else it should inherit that transitively.

;; Assumed derived slots:
;;  examples
;;  specializations (derived from GENERALIZATIONS)
;;  rarity
;;  arity ? (derived from DOMAIN)


;; in int-examples & int-applics, 'int' means 'interesting'?


;;;;-----------------------------
;;;; Units and their properties
;;;;
;;;; Heuristics are Units as well, but broken off into their own section

;; Many of these collection fields are machine-generated from RLL, and should be removed from the source (but use these lists to verify the generated versions). Worth 500 is likely default, don't need to specify those.

;; Rarity is maintained by run-alg or run-defn, so the values in the putprops are likely the runtime accumulated state saved from RLL or similar tool. The decimal values are too long to be hand-tweaked

(in-package "EURISCLO")

(defvar *units* '(int-applics mult-ele-struc-insert h29 h28 h27 h26 h25 rarity why-int h24 h23 is-a-int
                  int-examples less-interesting more-interesting h22 interestingness restrictions
                  extensions op-cat-by-nargs pred-cat-by-nargs tertiary-pred unary-pred binary-pred
                  higher-arity lower-arity non-empty-struc empty-struc set-of-sets
                  structure-of-structures truth-value atom implies not logic-op relation
                  set-of-o-pairs invert-op inverted-op restrict identity-1 proj-3-of-3 proj-2-of-3
                  proj-1-of-3 proj-2 proj-1 memb member all-but-last last-ele all-but-third all-but-second
                  all-but-first third-ele second-ele first-ele reverse-o-pair pair o-pair
                  parallel-join-2 parallel-join repeat2 tertiary-op repeat binary-op
                  parallel-replace-2 each-element-is-a unary-op type-of-structure parallel-replace
                  coalesce bag-difference o-set-difference list-difference set-difference
                  struc-difference bag-union list-union o-set-union struc-union bag-intersect
                  o-set-intersect list-intersect struc-intersect set-union set-intersect ord-struc-op
                  ord-struc-equal bag-equal list-equal o-set-equal suf-defn nec-defn un-ord-struc
                  ord-struc no-mult-ele-struc o-set-delete o-set-op o-set-insert o-set
                  mult-ele-struc-delete-1 mult-ele-struc-op mult-ele-struc bag-delete-1 bag-delete bag-op
                  bag-insert bag list-delete-1 list-delete list list-insert list-op set-delete
                  set-insert struc-delete struc-op struc-insert and abbrev add alg always-nil
                  always-nil-2 always-t always-t-2 anything applic-generator applics arity
                  best-choose best-subset bit category compiled-defn compose conjecture
                  conjecture-about conjectures constant-binary-pred constant-pred
                  constant-unary-pred creditors criterial-slot data-type defn direct-applics
                  divisors-of domain dont-copy double-check eq equal elim-slots english even-num
                  examples failed-record failed-record-for fast-alg fast-defn format
                  generalizations generator good-choose good-subset h1 h10 h11 h12 h13 h14 h15
                  h16 h17 h18 h19 h19-criterial h2 h20 h21 h3 h4 h5 h5-criterial h5-good h6 h7 h8
                  h9 h-avoid h-avoid-2 h-avoid-2-and h-avoid-3 h-avoid-3-first h-avoid-if-working heuristic
                  hind-sight-rule ieqp igeq igreaterp ileq ilessp if-about-to-work-on-task
                  if-finished-working-on-task if-parts if-potentially-relevant if-task-parts
                  if-truly-relevant if-working-on-task in-domain-of indirect-applics inverse isa
                  is-range-of iterative-alg iterative-defn math-concept math-obj math-op math-pred
                  multiply nnumber non-criterial-slot non-examples num-op or odd-num op
                  overall-record perf-num perf-square pred prime-num proto-conjec random-choose
                  random-subset range record record-for record-slot recursive-alg recursive-defn
                  repr-concept set set-equal set-of-numbers set-op sib-slots slot specializations
                  square struc-equal structure sub-slots subsetp subsumed-by subsumes successor
                  super-slots task the-first-of the-second-of then-add-to-agenda
                  then-add-to-agenda-failed-record then-add-to-agenda-record then-compute
                  then-compute-failed-record then-compute-record then-conjecture
                  then-conjecture-failed-record then-conjecture-record then-define-new-concepts
                  then-define-new-concepts-failed-record then-define-new-concepts-record
                  then-delete-old-concepts then-delete-old-concepts-failed-record
                  then-delete-old-concepts-record then-modify-slots then-modify-slots-failed-record
                  then-modify-slots-record then-parts then-print-to-user then-print-to-user failed-record
                  then-print-to-user-record to-delete to-delete-1 transpose unary-unit-op undefined
                  undefined-pred unit unit-op- unitized-alg unitized-defn worth los1 los2 los3 los4
                  los5 los6 los7 win1))

;; TODO - break these lexically out into topical groups
(defmacro defunit (&rest rest)
  `(putprops ,@rest))


(defunit int-applics
  worth 500
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  double-check t
  dont-copy t
  super-slots (applics)
  less-interesting (applics))

(defunit mult-ele-struc-insert
  worth 500
  isa (math-concept math-op op anything struc-op mult-ele-struc-op binary-op)
  arity 2
  domain (anything mult-ele-struc)
  range (mult-ele-struc)
  elim-slots (applics)
  specializations (list-insert bag-insert)
  fast-alg cons)

(defunit rarity
  worth 500
  isa (slot non-criterial-slot repr-concept anything)
  data-type number
  dont-copy t
  format (frequency-true number-t number-f))

(defunit why-int
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type next
  double-check t
  dont-copy t)

(defunit is-a-int
  worth 300
  inverse (int-examples)
  data-type unit
  double-check t
  isa (slot non-criterial-slot repr-concept anything))

(defunit int-examples
  worth 500
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  double-check t
  dont-copy t
  super-slots (examples)
  inverse (is-a-int)
  less-interesting (examples))

(defunit less-interesting
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  inverse (more-interesting))

(defunit more-interesting
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  inverse (less-interesting))

(defunit interestingness
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type lisp-pred
  double-check t
  abbrev ("What would make an instance of this unit interesting?")
  english ("What features or properties would an example or applic of this unit possess which would make it unusually interesting?"))

(defunit restrictions
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  double-check t
  inverse (extensions)
  super-slots (specializations))

(defunit extensions
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  double-check t
  inverse (restrictions)
  super-slots (generalizations))

(defunit op-cat-by-nargs
  worth 500
  isa (category anything repr-concept)
  examples (unary-pred binary-pred tertiary-pred unary-op binary-op tertiary-op)
  generalizations (category)
  specializations (pred-cat-by-nargs))

(defunit pred-cat-by-nargs
  worth 500
  isa (category anything repr-concept)
  examples (unary-pred binary-pred tertiary-pred)
  generalizations (category op-cat-by-nargs))

(defunit tertiary-pred
  lower-arity (binary-pred)
  worth 500
  generalizations (tertiary-op pred op anything)
  isa (repr-concept anything category pred-cat-by-nargs op-cat-by-nargs)
  fast-defn (lambda (f)
              (and (memb 'pred (isa f))
                   (eq 3 (arity f))))
  rarity (0.1827957 17 76))

(defunit unary-pred
  worth 500
  higher-arity (binary-pred)
  generalizations (unary-op pred op anything)
  isa (repr-concept anything category pred-cat-by-nargs op-cat-by-nargs)
  examples (always-t always-nil constant-unary-pred undefined-pred not)
  fast-defn (lambda (f)
              (and (memb 'pred (isa f))
                   (eq 1 (arity f))))
  rarity (0.1182796 11 82))

(defunit binary-pred
  worth 500
  lower-arity (unary-pred)
  higher-arity (tertiary-pred)
  generalizations (binary-op pred op anything)
  isa (repr-concept anything category pred-cat-by-nargs op-cat-by-nargs)
  examples (equal ieqp eq ileq igeq ilessp igreaterp and or the-second-of
                  the-first-of struc-equal set-equal subsetp constant-binary-pred
                  always-t-2 always-nil-2 o-set-equal bag-equal list-equal member memb
                  implies)
  fast-defn (lambda (f)
              (and (memb 'pred (isa f))
                   (eq 2 (arity f))))
  int-examples (ieqp eq struc-equal set-equal o-set-equal bag-equal list-equal memb member)
  rarity (0.07526882 7 86))

(defunit higher-arity
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  inverse (lower-arity))

(defunit lower-arity
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  inverse (higher-arity))

(defunit non-empty-struc
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  generalizations (structure anything set list bag mult-ele-struc o-set
                             no-mult-ele-struc ord-struc un-ord-struc pair o-pair)
  fast-defn consp
  examples nil)

(defunit empty-struc
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  generalizations (structure anything set list bag mult-ele-struc o-set
                             no-mult-ele-struc ord-struc un-ord-struc)
  fast-defn null
  elim-slots (examples))

(defunit set-of-sets
  isa (math-concept math-obj anything category)
  worth 500
  unitized-defn (lambda (s)
                  (and (run-defn 'set s)
                       (every (lambda (n) (run-defn 'set n)) s)))
  elim-slots (examples)
  generalizations (anything structure-of-structures)
  each-element-is-a set
  specializations (relation))

(defunit structure-of-structures
  isa (math-concept math-obj anything category)
  worth 500
  unitized-defn (lambda (s)
                  (and (run-defn 'structure s)
                       (every (lambda (n) (run-defn 'structure n)) s)))
  elim-slots (examples)
  generalizations (anything)
  each-element-is-a structure
  specializations (set-of-o-pairs set-of-sets))

(defunit truth-value
  generalizations (anything atom)
  worth 500
  isa (anything category math-obj)
  fast-defn (lambda (x)
              (or (eq x nil)
                  (eq x t)))
  examples (t nil))

(defunit atom
  generalizations (anything)
  worth 500
  isa (anything category repr-concept)
  fast-defn atom
  specializations (truth-value))

(defunit implies
  worth 500
  isa (op pred math-op math-pred anything binary-op logic-op binary-pred)
  arity 2
  domain (anything anything)
  range (anything)
  elim-slots (applics)
  fast-alg (lambda (x y)
             (or (null x) y))
  unitized-alg (lambda (x y)
                   (run-alg 'or (run-alg 'not x) y)))

(defunit not
  worth 500
  isa (op pred math-op math-pred anything unary-op logic-op unary-pred)
  arity 1
  domain (anything)
  range (bit)
  elim-slots (applics)
  fast-alg not)

(defunit logic-op
  generalizations (math-concept op math-op anything struc-op)
  worth 500
  isa (math-concept math-obj anything category)
  abbrev ("Logical operations")
  examples (and or the-first-of the-second-of not implies))

(defunit relation
  isa (math-concept math-obj anything category)
  worth 500
  unitized-defn (lambda (s)
                  (and (run-defn 'set s)
                       ;; BUGFIX - OPair was missing a quote
                       (every (lambda (n) (run-defn 'o-pair n)) s))))

(defunit set-of-o-pairs
  isa (math-concept math-obj anything category)
  worth 500
  unitized-defn (lambda (s)
                  (and (run-defn 'set s)
                       (every (lambda (n) (run-defn 'o-pair n)) s)))
  elim-slots (examples)
  generalizations (anything structure-of-structures)
  each-element-is-a o-pair
  specializations (relation))

(defunit invert-op
  worth 100
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (op)
  range (inverted-op)
  elim-slots (applics))

(defunit inverted-op
  generalizations (math-concept op math-op anything)
  worth 500
  isa (math-concept math-obj anything category)
  abbrev ("Operations which were formed via InvertOp")
  is-range-of (invert-op))

(defunit restrict
  worth 600
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (op)
  range (op)
  elim-slots (applics)
  fast-alg (lambda (f)
             (let* ((garg (random-choose (subset (domain f) #'specializations)))
                    (newdom (random-subst (random-choose (specializations garg))
                                          garg
                                          (domain f))))
               (cond ((and garg ;; TODO - these 2 were setf'd here. Is there return value ever NIL?
                           newdom
                           (not (equal newdom (domain f))))
                      (let ((nam (create-unit (pack* 'restrict- f))))
                        (put nam 'isa (copy (isa f)))
                        (put nam 'worth (average-worths 'restrict f))
                        (put nam 'arity (arity f))
                        ;; Since MAPCAR exits when one list runs out, this gives var names per param in DOMAIN
                        (let ((fargs (mapcar #'the-second-of
                                             (domain f)
                                             '(u v w x y z z2 z3 z4 z5))))
                          (put nam 'domain newdom)
                          (put nam 'range (copy (range f)))
                          (put nam 'unitized-alg (compile-report
                                                          `(lambda ,fargs
                                                             (run-alg ',f ,@fargs)))))
                        (put nam 'extensions (list f))
                        (put nam 'elim-slots '(applics))
                        (put nam 'creditors '(restrict))
                        (add-inv nam)
                        nam))
                     (t ;; ORIG: we should check for cases where 2 domain components of F have a common nontrivial specialization
                      'failed)))))

(defunit identity-1
  worth 500
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (anything)
  range (anything)
  elim-slots (applics)
  fast-alg (lambda (x) x)
  generalizations (proj1 proj2 proj-1-of-3 proj-2-of-3 proj-3-of-3))

(defunit proj-3-of-3
  worth 500
  isa (math-concept math-op anything tertiary-op)
  arity 3
  domain (anything anything anything)
  range (anything)
  elim-slots (applics)
  fast-alg (lambda (x y z)
             (declare (ignore x y))
             z)
  specializations (identity-1))

(defunit proj-2-of-3
  worth 500
  isa (math-concept math-op op anything tertiary-op)
  arity 3
  domain (anything anything anything)
  range (anything)
  elim-slots (applics)
  fast-alg (lambda (x y z)
             (declare (ignore x z))
             y)
  specializations (identity-1))

(defunit proj-1-of-3
  worth 500
  isa (math-concept math-op op anything tertiary-op)
  arity 3
  domain (anything anything anything)
  range (anything)
  elim-slots (applics)
  fast-alg (lambda (x y z)
             (declare (ignore y z))
             x)
  specializations (identity-1))

(defunit proj2
  worth 500
  isa (math-concept math-op op anything binary-op)
  arity 2
  domain (anything anything)
  range (anything)
  elim-slots (applic)
  fast-alg (lambda (x y)
             (declare (ignore x))
             y)
  specializations (identity-1))

(defunit proj1
  worth 500
  isa (math-concept math-op op anything binary-op)
  arity 2
  domain (anything anything)
  range (anything)
  elim-slots (applics)
  fast-alg (lambda (x y)
             (declare (ignore y))
             x)
  specializations (identity-1))

(defunit memb
  worth 500
  isa (math-concept math-op op math-pred pred anything binary-op binary-pred)
  fast-alg memb ;; original implementation: (lambda (x y) (memb x y))
  arity 2
  domain (anything structure)
  range (bit)
  elim-slots (applics)
  recursive-alg (lambda (x s)
                  (cond ((null s) nil)
                        ((eq x (car s)) t)
                        (t (run-alg 'memb x (cdr s)))))
  is-a-int (binary-pred)
  rarity (0.1.1 9))

(defunit member
  worth 500
  isa (math-concept math-op math-pred pred anything binary-op binary-pred)
  fast-alg member ;; original implementation: (lambda (x y) (member x y))
  arity 2
  domain (anything structure)
  range (bit)
  elim-slots (applics)
  recursive-alg (lambda (x s)
                  (cond ((null s) nil)
                        ((equal x (car s)) t)
                        (t (run-alg 'member x s))))
  is-a-int (binary-pred)
  rarity (0.1 1 9))

(defunit all-but-last
  worth 500
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (ord-struc)
  range (anything)
  elim-slots (applics)
  ;; OPTIMIZATION - not a very fast fast-alg, as this traverses twice.  just keep track of prior cell and NIL terminate it if we reach the last one.
  fast-alg (lambda (s)
             (ldiff s (last s))))

(defunit last-ele
  worth 500
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (ord-struc)
  range (anything)
  elim-slots (applics)
  fast-alg (lambda (s)
             (car (last s))))

(defunit all-but-third
  worth 500
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (ord-struc)
  range (anything)
  elim-slots (applics)
  fast-alg (lambda (s)
             (cons (car s)
                   (cons (cadr s)
                         (cdddr s)))))

(defunit all-but-second
  worth 500
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (ord-struc)
  range (anything)
  elim-slots (applics)
  fast-alg (lambda (s)
             (cons (car s)
                   (cddr s))))

(defunit all-but-first
  worth 500
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (ord-struc)
  range (anything)
  elim-slots (applics)
  fast-alg cdr)

(defunit third-ele
  worth 500
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (ord-struc)
  range (anything)
  elim-slots (applics)
  fast-alg caddr)

(defunit second-ele
  worth 500
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (ord-struc)
  range (anything)
  elim-slots (applics)
  fast-alg cadr
  rarity (0.85 17 3))

(defunit first-ele
  worth 500
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (ord-struc)
  range (anything)
  elim-slots (applics)
  fast-alg car)

(defunit reverse-o-pair
  worth 500
  isa (math-concept math-op op anything unary-op ord-struc-op list-op)
  arity 1
  domain (o-pair)
  range (o-pair)
  elim-slots (applics)
  fast-alg (lambda (p)
             (list (cadr p)
                   (car p))))

(defunit pair
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  ;; TODO - huh?
  generator ((nil)
             (get-a-o-pair)
             (old))
  fast-defn (lambda (s) (and (consp s) (eq 2 (length s))))
  generalizations (anything structure mult-ele-struc un-ord-struc bag)
  specializations (non-empty-struc))

(defunit o-pair
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  generator ((nil)
             (get-a-o-pair)
             (old))
  fast-defn (lambda (s) (and (consp s) (eq 2 (length s))))
  generalizations (anything structure mult-ele-struc ord-struc list)
  in-domain-of (reverse-o-pair)
  is-range-of (reverse-o-pair)
  specializations (non-empty-struc))

(defunit parallel-join
  worth 800
  isa (math-concept math-op op anything binary-op)
  arity 2
  domain (type-of-structure unary-op)
  range (unary-op)
  elim-slots (applics)
  fast-alg (lambda (s f)
             ;; ORIG: note that S is the name of a type of structure, such as List, rather than a particular individual structure, such as (a b c d)
             (cond ((and (memb 'structure (generalizations s))
                         (memb 'op (isa f))
                         (eq 1 (length (domain f)))
                         (or (eq 'anything (car (domain f)))
                             (let ((typmem (each-element-is-a s)))
                               (and typmem
                                    (is-a-kind-of typmem (car (domain f))))))
                         (is-a-kind-of (car (range f)) 'structure))
                    (let ((nam (create-unit (pack* 'join- f '-on- s 's))))
                      (put nam 'isa (copy (isa f)))
                      (put nam 'worth (average-worths 'parallel-join (average-worths f s)))
                      (put nam 'arity 1)
                      (put nam 'domain (list s))
                      (put nam 'range (list (let ((mu (pack* 'of (car (range f)) 's)))
                                              (cond ((unitp mu)
                                                     mu)
                                                    (t (cprin1 21 "~% It might be nice to have a unit called " mu "~%")
                                                       s)))))
                      (put nam 'unitized-alg (compile-report
                                                      (subst f 'f '(lambda (s)
                                                                    (mapappend s (lambda (e)
                                                                                   (run-alg 'f e)))))
                                                      ))
                      (put nam 'elim-slots '(applics))
                      (put nam 'creditors '(parallel-join))
                      (add-inv nam)
                      nam))
                   (t ;; ORIG: we should check for cases where f could sub for other than the first arg of g
                    'failed))))

(defunit parallel-join-2
  worth 800
  isa (math-concept math-op op anything tertiary-op)
  arity 3
  domain (type-of-structure type-of-structure binary-op)
  range (binary-op)
  elim-slots (applics)
  fast-alg (lambda (s s2 f)
             ;; ORIG: note that S is the name of a type of structure, such as List, rather than a particular individual structure, such as (a b c d)
             (cond ((and (memb 'structure (generalizations s))
                         (memb 'structure (generalizations s2))
                         (memb 'op (isa f))
                         (eq 2 (length (domain f)))
                         (is-a-kind-of s2 (cadr (domain f)))
                         (or (eq 'anything (car (domain f)))
                             (let ((typmem (each-element-is-a s)))
                               (and typmem
                                    (is-a-kind-of typmem (car (domain f))))))
                         (is-a-kind-of (car (range f)) 'structure))
                    (let ((nam (create-unit (pack* 'join- f '-on- s 's '-with-a- 's2 '-as-param))))
                      (put nam 'isa (isa f))
                      (put nam 'worth (average-worths 'parallel-replace-2
                                                      (average-worths f (average-worths s s2))))
                      (put nam 'arity 2)
                      (put nam 'domain (list s s2))
                      (put nam 'range (list (let ((mu (pack* s '-of- (car (range f)) 's)))
                                              (if (unitp mu)
                                                  mu
                                                  (progn
                                                    (cprin1 21 "~% It might be nice to have a unit called "
                                                            mu "~%")
                                                    s)))))
                      (put nam 'unitized-alg (compile-report
                                                       (subst f 'f '(lambda (s s2)
                                                              (mapappend s (lambda (e)
                                                                             (run-alg 'f e s2)))))))
                      (put nam 'elim-slots '(applics))
                      (put nam 'creditors 'parallel-replace-2)
                      (add-inv nam)
                      nam))
                   (t 'failed)))
  rarity (0.3272727 36 74))


(defunit tertiary-op
  generalizations (op anything)
  worth 500
  isa (repr-concept anything category op-cat-by-nargs)
  examples (parallel-replace-2 repeat2 parallel-join-2 proj-1-of-3 proj-2-of-3 proj-3-of-3)
  in-domain-of (repeat2)
  lower-arity (binary-op)
  specializations (tertiary-pred)
  fast-defn (lambda (f)
              (eq 3 (arity f)))
  rarity (0.3978495 37 56))

(defunit repeat
  worth 800
  isa (math-concept math-op op anything binary-op)
  arity 2
  domain (type-of-structure binary-op)
  range (unary-op)
  elim-slots (applics)
  fast-alg (lambda (s f)
             ;; ORIG: note that S is the name of a type of structure, such as List, rather than a particular individual structure, such as (a b c d)
             (cond ((and (memb 'structure (generalizations s))
                         (memb 'op (isa f))
                         (eq 2 (length (domain f)))
                         (or (eq 'anything (cadr (domain f)))
                             (let ((typmem (each-element-is-a s)))
                               (and typmem
                                    (is-a-kind-of typmem (cadr (domain f))))))
                         (is-a-kind-of (car (range f))
                                       (car (domain f))))
                    (let ((nam (create-unit (pack* 'repeat- f '-on- s 's))))
                      (put nam 'isa (subst 'unary-op 'binary-op (isa f)))
                      (put nam 'worth (average-worths 'repeat (average-worths f s)))
                      (put nam 'arity 1)
                      (put nam 'domain (list s))
                      (put nam 'range (copy (range f)))
                      (put nam 'unitized-alg
                           (compile-report
                                    (subst f 'f '(lambda (s)
                                                  ;; TODO - idiomize this loop, is this a REDUCE?
                                                  (let ((v (car s)))
                                                    (mapc (lambda (e)
                                                            (setf v (run-alg 'f v e)))
                                                          (cdr s))
                                                    v)))))
                      (put nam 'elim-slots '(applics))
                      (put nam 'creditors 'repeat)
                      (add-inv nam)
                      nam))
                   (t ;; ORIG: we should check for cases where f could sub for other than the first arg of g
                    'failed)))
  rarity (0.3555556 16 29))

(defunit repeat2
  worth 800
  isa (math-concept math-op op anything tertiary-op)
  arity 3
  domain (type-of-structure type-of-structure tertiary-op)
  range (binary-op)
  elim-slots (applics)
  fast-alg (lambda (s s2 f)
             ;; ORIG: note that S is the name of a type of structure, such as List, rather than a particular individual structure, such as (a b c d)
             (cond ((and (memb 'structure (generalizations s))
                         (memb 'structure (generalizations s2))
                         (memb 'op (isa f))
                         (eq 3 (length (domain f)))
                         (or (eq 'anything (caddr (domain f)))
                             (let ((typmem (each-element-is-a s)))
                               (and typmem
                                    (is-a-kind-of typmem (caddr (domain f))))))
                         (is-a-kind-of (car (range f))
                                       (car (domain f)))
                         (is-a-kind-of s2 (cadr (domain f))))
                    (let ((nam (create-unit (pack* 'repeat2- f '-on- 's-with-a- s2 '-as-param))))
                      (put nam 'isa (cons 'binary-op (remove 'tertiary-op (isa f))))
                      (put nam 'worth (average-worths 'repeat2 (average-worths f (average-worths s s2))))
                      (put nam 'arity 2)
                      (put nam 'domain (list s s2))
                      (put nam 'range (copy (range f)))
                      (put nam 'unitized-alg
                           (compile-report
                                    (subst f 'f '(lambda (s s2)
                                                  (let ((v (car s)))
                                                    (mapc (lambda (e)
                                                            (setf v (run-alg 'f v s2 e)))
                                                          (cdr s))
                                                          v)))))
                      (put nam 'elim-slots '(applics))
                      (put nam 'creditors '(repeat2))
                      (add-inv nam)
                      nam))
                   (t ;; ORIG:  we should check for cases where f could sub for other than the first arg of g
                    'failed)))
  rarity (0.2295082 14 47))

(defunit binary-op
  in-domain-of (parallel-replace-2 repeat parallel-join-2)
  generalizations (op anything)
  worth 500
  examples (parallel-replace bag-difference o-set-difference list-difference
                             set-difference struc-difference bag-union list-union
                             o-set-union struc-union bag-intersect set-union set-intersect
                             ord-struc-equal bag-equal list-equal o-set-equal o-set-delete
                             o-set-insert mult-ele-struc-delete-1 bag-delete-1 bag-delete
                             bag-insert list-delete-1 list-delete list-insert set-delete
                             set-insert struc-delete struc-insert and add always-nil-2
                             always-t-2 compose eq equal ieqp igeq igreaterp ileq
                             ilessp multiply or set-equal struc-equal subsetp
                             the-first-of the-second-of repeat parallel-join member memb
                             proj1 proj2 implies mult-ele-struc-insert)
  isa (repr-concept anything category op-cat-by-nargs)
  is-range-of (parallel-replace-2 repeat2 parallel-join-2)
  lower-arity (unary-op)
  higher-arity (tertiary-op)
  specializations (binary-pred)
  fast-defn (lambda (f)
              (eq 2 (arity f)))
  rarity (0.1827957 17 76))

(defunit parallel-replace-2
  worth 800
  isa (math-concept math-op op anything tertiary-op)
  arity 3
  domain (type-of-structure type-of-structure binary-op)
  range (binary-op)
  elim-slots (applics)
  fast-alg (lambda (s s2 f)
             ;; ORIG: note that S is the name of a type of structure, such as List, rather than a particular individual structure, such as (a b c d)
             (cond ((and (memb 'structure (generalizations s))
                         (memb 'structure (generalizations s2))
                         (memb 'op (isa f))
                         (eq 2 (length (domain f)))
                         (is-a-kind-of s2 (cadr (domain f)))
                         (or (eq 'anything (car (domain f)))
                             (let ((typmem (each-element-is-a s)))
                               (and typmem
                                    (is-a-kind-of typmem (car (domain f)))))))
                    (let ((nam (create-unit (pack* 'perform- f '-on- s 's-with-a- s2 '-as-param))))
                      (put nam 'isa (isa f))
                      (put nam 'worth (average-worths 'parallel-replace-2 (average-worths f (average-worths s s2))))
                      (put nam 'arity 2)
                      (put nam 'domain (list s s2))
                      (put nam 'range (list
                                       (let ((mu (pack* s '-of- (car (range f)) 's)))
                                         (cond ((unitp mu)
                                                mu)
                                               (t (cprin1 21 "~% It might be nice to have a unit called " mu "~%")
                                                  s)))))
                      (put nam 'unitized-alg
                           (compile-report
                                    (subst f 'f '(lambda (s s2)
                                                           (mapcar (lambda (e)
                                                                     (run-alg 'f e s2))
                                                            s)))))
                      (put nam 'elim-slots '(applics))
                      (put nam 'creditors '(parallel-replace-2))
                      (add-inv nam)
                      nam))
                   (t 'failed)))
  rarity (0.375 3 5))

(defunit each-element-is-a
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type unit)

(defunit unary-op
  generalizations (op anything)
  worth 500
  examples (coalesce always-nil always-t best-choose best-subset constant-binary-pred
                     constant-unary-pred divisors-of good-choose good-subset
                     random-choose random-subset square successor undefined-pred
                     reverse-o-pair first-ele second-ele third-ele all-but-first
                     all-but-second all-but-third last-ele all-but-last identity-1 restrict
                     invert-op not)
  isa (repr-concept anything category op-cat-by-nargs)
  in-domain-of (parallel-replace parallel-join)
  is-range-of (parallel-replace repeat parallel-join)
  higher-arity (binary-op)
  specializations (unary-pred)
  fast-defn (lambda (f)
              (eq 1 (arity f)))
  rarity (0.2473118 23 70))

(defunit type-of-structure
  in-domain-of (parallel-replace parallel-replace-2 repeat repeat2 parallel-join
                                 parallel-join-2)
  worth 500
  isa (category anything repr-concept)
  examples (set list bag mult-ele-struc o-set no-mult-ele-struc ord-struc
                un-ord-struc o-pair pair empty-struc non-empty-struc)
  generalizations (category))

(defunit parallel-replace
  worth 888
  isa (math-concept math-op op anything binary-op)
  arity 2
  domain (type-of-structure unary-op)
  range (unary-op)
  elim-slots (applics)
  fast-alg (lambda (s f)
             ;; ORIG: note that S is the name of a type of structure, such as List, rather than a particular individual structure, such as (a b c d)
             (cond ((and (memb 'structure (generalizations s))
                         (memb 'op (isa f))
                         (eq 1 (length (domain f)))
                         (or (eq 'anything (car (domain f)))
                             (let ((typmem (each-element-is-a s)))
                               (and typmem
                                    (is-a-kind-of typmem (car (domain f)))))))
                    (let ((nam (create-unit (pack* 'perform- f '-on- s 's))))
                      (put nam 'isa (copy (isa f)))
                      (put nam 'worth (average-worths 'parallel-replace (average-worths f s)))
                      (put nam 'arity 1)
                      (put nam 'domain (list s))
                      ;; TODO - abstract out this existence check?
                      (put nam 'range (list (let ((mu (pack* s '-of- (car (range f)) '-s)))
                                              (cond ((unitp mu)
                                                     mu)
                                                    (t (cprin1 21 "~% It might be nice to have a unit called " mu "~%")
                                                       s)))))
                      (put nam 'unitized-alg
                           (compile-report
                                    (subst f 'f '(lambda (s)
                                                           (mapcar (lambda (e)
                                                                     (run-alg 'f e))
                                                            s)))))
                      (put nam 'elim-slots '(applics))
                      (put nam 'creditors '(parallel-replace))
                      (add-inv nam)
                      nam))
                   (t ;; ORIG: we should check for cases where f could sub for other than the first arg of g
                    'failed)))
  rarity (0.2372881 14 45))

(defunit coalesce
  worth 900
  isa (math-concept math-op op anything unary-op)
  arity 1
  domain (op)
  range (op)
  elim-slots (applics)
  fast-alg (lambda (f)
             (let ((coargs (random-pair (domain f) 'is-a-kind-of)))
               (cond (coargs
                      (let ((nam (create-unit (pack* 'coa- f))))
                        (put nam 'isa (set-diff (isa f) (examples 'op-cat-by-nargs)))
                        ;; ORIG: We really should check that each such unit still claims Coa-f as an example -- eg, suppose f was a BinaryPred
                        (put nam 'worth (average-worths 'coalesce f))
                        (put nam 'arity (1- (arity f)))
                        (let* ((fargs (mapcar #'the-second-of
                                              (domain f)
                                              '(u v w x y z z2 z3 z4 z5)))
                               (newargs (copy fargs)))
                          ;; TODO - unravel this operation
                          (rplaca (nth newargs (cadr coargs))
                                  (car (nth newargs (car coargs))))
                          ;; TODO - can this be hoisted up to the prior LET? does the RPLACA above mutate this?
                          (let ((newdom (copy (domain f))))
                            (rplaca (nth newdom (cadr coargs))
                                    (car (nth newdom (car coargs))))
                            (if (<= (cadr coargs) 1)
                                (pop newdom)
                                (rplacd (nth newdom (1- (cadr coargs)))
                                        (cdr (nth newdom (cadr coargs)))))
                            (if (<= (cadr coargs) 1)
                                (pop fargs)
                                (rplacd (nth fargs (1- (cadr coargs)))
                                        (cdr (nth fargs (cadr coargs)))))
                            (put nam 'domain newdom)
                            (put nam 'range (copy (range f)))
                            (put nam 'unitized-alg
                                 (compile-report
                                          `(lambda ,fargs
                                                      (run-alg ',f ,@newargs))))
                            (put nam 'elim-slots '(applics))
                            (put nam 'creditors '(coalesce))
                            (put nam 'isa (append (isa nam)
                                                  (subset (examples 'op-cat-by-nargs)
                                                          (lambda (pc)
                                                            (run-defn pc nam)))))
                            (add-inv nam)
                            nam))))
                     (t ;; ORIG: we should check for cases where 2 domain components of f have a common nontrivial specialization
                      'failed))))
  rarity (0.3928571 22 34))

(defunit bag-difference
  worth 500
  isa (math-concept math-op op anything struc-op bag-op binary-op)
  arity 2
  domain (bag bag)
  range (bag)
  elim-slots (applics)
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1) nil)
                        ((member (car s2) s2)
                         (run-alg 'bag-delete-1 (car s1) s2))
                        (t (cons (car s1)
                                 (run-alg 'bag-difference
                                          (cdr s1)
                                          (run-alg 'bag-delete-1
                                                   (car s2)
                                                   s2))))))
  generalizations (struc-difference))

(defunit o-set-difference
  worth 500
  isa (math-concept math-op op anything struc-op o-set-op binary-op)
  arity 2
  domain (o-set o-set)
  range (o-set)
  elim-slots (applics)
  fast-alg set-difference
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1) nil)
                        ((member (car s1) s2)
                         (run-alg 'o-set-difference (cdr s1) s2))
                        (t (cons (car s1)
                                 (run-alg 'o-set-difference (cdr s1) s2)))))
  generalizations (struc-difference))

(defunit list-difference worth 500
  isa (math-concept math-op op anything struc-op list-op binary-op)
  arity 2
  domain (list list)
  range (list)
  elim-slots (applics)
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1) nil)
                        ((member (car s1) s2)
                         (run-alg 'list-difference
                                  (cdr s1)
                                  (run-alg 'list-delete-1 (car s1) s2)))
                        (t (cons (car s2)
                                 (run-alg 'list-difference
                                          (cdr s1)
                                          (run-alg 'list-delete-1 (car s1) s2))))))
  generalizations (struc-difference))

(defunit set-difference
  worth 500
  isa (math-concept math-op op anything struc-op set-op binary-op)
  arity 2
  domain (set set)
  range (set)
  elim-slots (applics)
  fast-alg set-difference
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1) nil)
                        ((member (car s1) s2)
                         (run-alg 'set-difference (cdr s1) s2))
                        (t (cons (car s1)
                                 (run-alg 'set-difference (cdr s1) s2)))))
  generalizations (struc-difference))

(defunit struc-difference
  worth 500
  isa (math-concept math-op op anything struc-op binary-op)
  arity 2
  domain (structure structure)
  range (structure)
  elim-slots (applics)
  specializations (set-difference list-difference o-set-difference
                                  bag-difference))

(defunit bag-union
  worth 500
  isa (math-concept math-op op anything struc-op binary-op)
  arity 2
  domain (bag bag)
  range (bag)
  elim-slots (applics)
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1) s2)
                        (t (run-alg 'bag-insert (car s1)
                                    (run-alg 'bag-union (cdr s1)
                                             (run-alg 'bag-delete-1 (car s1) s2))))))
  generalizations (struc-union))

(defunit list-union
  worth 500
  isa (math-concept math-op op anything struc-op list-op binary-op)
  arity 2
  domain (list list)
  range (list)
  elim-slots (applics)
  fast-alg append
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1) s2)
                        (t (cons (car s1)
                                 (run-alg 'list-union (cdr s1) s2)))))
  generalizations (struc-union))

(defunit o-set-union
  worth 500
  isa (math-concept math-op op anything struc-op o-set-op binary-op)
  arity 2
  domain (o-set o-set)
  range (o-set)
  elim-slots (applics)
  fast-alg set-union
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1) s2)
                        ((member (car s1) s2)
                         (run-alg 'o-set-union (cdr s1) s2))
                        (t (cons (car s1)
                                 (run-alg 'o-set-union (cdr s1) s2)))))
  generalizations (struc-union))

(defunit struc-union
  worth 500
  isa (math-concept math-op op anything struc-op binary-op)
  arity 2
  domain (structure structure)
  range (structure)
  elim-slots (applics)
  specializations (set-union o-set-union list-union bag-union))

(defunit bag-intersect
  worth 500
  isa (math-concept math-op op anything struc-op bag-op binary-op)
  arity 2
  domain (bag bag)
  range (bag)
  elim-slots (applics)
  iterative-alg (lambda (s1 s2)
                  (dolist (x (copy-list s1))
                    (cond ((member x s2)
                           (setf s2 (run-alg 'bag-delete-1 x s2)))
                          (t (setf s1 (run-alg 'bag-delete-1 x s1)))))
                  s1)
  generalizations (struc-intersect))

(defunit o-set-intersect
  worth 500
  isa (math-concept math-op op anything struc-op o-set-op binary-op)
  arity 2
  domain (o-set o-set)
  range (o-set)
  elim-slots (applics)
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1) nil)
                        ((member (car s1) s2)
                         (cons (car s1)
                               (run-alg 'o-set-intersect (cdr s1) s2)))
                        (t (run-alg 'o-set-intersect (cdr s1) s2))))
  generalizations (struc-intersect))

(defunit list-intersect
  worth 500
  isa (math-concept math-op op anything struc-op list-op binary-op)
  arity 2
  domain (list list)
  range (list)
  elim-slots (applics)
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1) nil)
                        ((member (car s1) s2)
                         (cons (car s1)
                               (run-alg 'list-intersect (cdr s1)
                                        (run-alg 'list-delete-1 (car s1) s2))))
                        (t (run-alg 'list-intersect (cdr s1) s2))))
  generalizations (struc-intersect))

(defunit struc-intersect
  worth 500
  isa (math-concept math-op op anything struc-op binary-op)
  arity 2
  domain (structure structure)
  range (structure)
  elim-slots (applics)
  specializations (set-intersect list-intersect o-set-intersect bag-intersect))

(defunit set-union
  worth 500
  isa (math-concept math-op op anything struc-op set-op binary-op)
  arity 2
  domain (set set)
  range (set)
  elim-slots (applics)
  fast-alg set-union
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1) s2)
                        ((member (car s1) s2)
                         (run-alg 'set-union (cdr s1) s2))
                        (t (cons (car s1)
                                 (run-alg 'set-union (cdr s1) s2)))))
  generalizations (struc-union))

(defunit set-intersect
  worth 500
  isa (math-concept math-op anything struc-op set-op binary-op)
  arity 2
  domain (set set)
  range (set)
  elim-slots (applics)
  fast-alg set-intersect
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1) nil)
                        ((member (car s1) s2)
                         (cons (car s1)
                               (run-alg 'set-intersect (cdr s1) s2)))
                        (t (run-alg 'set-intersect (cdr s1) s2))))
  generalizations (struc-intersect))

(defunit ord-struc-op
  generalizations (math-concept op math-op anything struc-op)
  worth 500
  isa (math-concept math-obj anything category)
  abbrev ("Operations on structures which are ordered")
  specializations (list-op o-set-op)
  examples (ord-struc-equal reverse-o-pair))

(defunit ord-struc-equal
  worth 500
  isa (math-concept math-op anything struc-op ord-struc-op binary-op)
  arity 2
  domain (ord-struc ord-struc)
  range (anything)
  elim-slots (applics)
  specializations (list-equal o-set-equal)
  fast-alg equal)

(defunit bag-equal
  worth 500
  isa (math-concept math-op op math-pred pred anything struc-op bag-op binary-op binary-pred)
  arity 2
  domain (bag bag)
  range (bit)
  elim-slots (applics)
  generalizations (equal struc-equal)
  recursive-alg (lambda (s1 s2)
                  (cond ((and (null s1)
                              (null s2))
                         t)
                        (t (and (consp s1)
                                (consp s2)
                                (member (car s1) s2)
                                (run-alg 'bag-equal (cdr s2) (run-alg 'bag-delete-1 (car s1) s2))))))
  specializations (list-equal)
  is-a-int (binary-pred)
  rarity (0.1 1 9))

(defunit list-equal
  worth 500
  isa (math-concept math-op op math-pred pred anything struc-op list-op binary-op binary-pred)
  arity 2
  domain (list list)
  range (bit)
  elim-slots (applics)
  generalizations (equal struc-equal bag-equal ord-struc-equal)
  recursive-alg (lambda (s1 s2)
                  (cond ((and (null s1)
                              (null s2))
                         t)
                        (t (and (consp s1)
                                (consp s2)
                                (equal (car s1) (car s2))
                                (run-alg 'list-equal (cdr s1) (cdr s2))))))
  fast-alg equal
  is-a-int (binary-pred)
  rarity (0.1 1 9))

(defunit o-set-equal
  worth 500
  isa (math-concept math-op op math-pred pred anything struc-op o-set-op binary-op binary-pred)
  arity 2
  domain (o-set o-set)
  range (bit)
  elim-slots (applics)
  generalizations (equal struc-equal subsetp set-equal ord-struc-equal)
  recursive-alg (lambda (s1 s2)
                  (cond ((and (null s1) (null s2))
                         t)
                        (t (and (consp s1)
                                (consp s2)
                                (equal (car s1) (car s2))
                                (run-alg 'o-set-equal (cdr s1) (cdr s2))))))
  fast-alg equal
  is-a-int (binary-pred)
  rarity (0.1 1 9))

(defunit suf-defn
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-pred
  generalizations (defn)
  super-slots (defn))

(defunit nec-defn
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-pred
  generalizations (defn)
  super-slots (defn))

(defunit un-ord-struc
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  specializations (bag set pair empty-struc non-empty-struc)
  generalizations (structure anything))

(defunit ord-struc
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  specializations (list o-set o-pair empty-struc non-empty-struc)
  generalizations (structure anything)
  in-domain-of (ord-struc-equal all-but-first first-ele second-ele third-ele all-but-second
                                all-but-third last-ele all-but-last))

(defunit no-mult-ele-struc
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  specializations (set o-set empty-struc non-empty-struc)
  generalizations (structure anything)
  nec-defn no-repeats-in)

(defunit o-set-delete
  worth 500
  isa (math-concept math-op op anything struc-op o-set-op binary-op)
  arity 2
  domain (anything o-set)
  range (o-set)
  elim-slots (applics)
  recursive-alg (lambda (x s)
                  (cond ((null s) nil)
                        ((equal x (car s))
                         (cdr s))
                        (t (cons (car s)
                                 (run-alg 'o-set-delete x (cdr s))))))
  fast-alg remove
  generalizations (struc-delete))

(defunit o-set-op
  generalizations (math-concept op math-op anything struc-op ord-struc-op)
  worth 500
  isa (math-concept math-obj anything category)
  abbrev ("O-Set Operations")
  examples (o-set-insert o-set-delete o-set-equal o-set-intersect o-set-union o-set-difference))

(defunit o-set-insert
  worth 500
  isa (math-concept math-op op anything struc-op o-set-op binary-op)
  arity 2
  domain (anything o-set)
  range (o-set)
  elim-slots (applics)
  recursive-alg (lambda (x s)
                  (cond ((null s)
                         (cons x s))
                        ((equal x (car s))
                         s)
                        (t (cons (car s)
                                 (run-alg 'o-set-insert x (cdr s))))))
  generalizations (struc-insert)
  fast-alg (lambda (x s)
             (cond ((member x s)
                    s)
                   ((null s)
                    (cons x s))
                   (t (cons (car s)
                            (run-alg 'o-set-insert x (cdr s)))))))

(defunit o-set
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  generator ((nil)
             (get-a-set)
             (old))
  fast-defn (lambda (s)
              (or (eq s nil)
                  (no-repeats-in s)))
  recursive-defn (lambda (s)
                   (if (consp s)
                       (and (not (member (car s) (cdr s)))
                            (run-defn 'o-set (cdr s)))
                       (eq s nil)))
  generalizations (anything structure bag list set no-mult-ele-struc ord-struc)
  in-domain-of (o-set-insert o-set-delete o-set-equal o-set-intersect o-set-union o-set-difference)
  is-range-of (o-set-insert o-set-delete o-set-intersect o-set-union o-set-difference)
  specializations (empty-struc non-empty-struc)
  rarity (0 2 2)
  elim-slots (examples))

(defunit mult-ele-struc-delete-1
  worth 500
  isa (math-concept math-op op anything struc-op mult-ele-struc-op binary-op)
  arity 2
  domain (anything mult-ele-struc)
  range (mult-ele-struc)
  elim-slots (applics)
  specializations (list-delete-1 bag-delete-1)
  recursive-alg (lambda (x s)
                  (cond ((null s) nil)
                        ((equal x (car s))
                         (cdr s))
                        (t (cons (car s)
                                 (run-alg 'mult-ele-struc-delete-1 x (cdr s)))))))

(defunit mult-ele-struc-op
  generalizations (math-concept op math-op anything struc-op)
  worth 500
  isa (math-concept math-obj anything category)
  abbrev ("Operations on structures which have multiple elements")
  specializations (list-op bag-op)
  examples (mult-ele-struc-delete-1 mult-ele-struc-insert))

(defunit mult-ele-struc
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  specializations (list bag o-pair pair empty-struc non-empty-struc)
  generalizations (structure anything)
  in-domain-of (mult-ele-struc-delete-1 mult-ele-struc-insert)
  is-range-of (mult-ele-struc-delete-1 mult-ele-struc-insert)
  suf-defn repeats-in)

(defunit bag-delete-1
  worth 500
  isa (math-concept math-op op anything struc-op bag-op binary-op)
  arity 2
  domain (anything bag)
  range (bag)
  elim-slots (applics)
  recursive-alg (lambda (x s)
                  (cond ((null s) nil)
                        ((equal x (car s))
                         (cdr s))
                        (t (cons (car s)
                                 (run-alg 'bag-delete-1 x (cdr s))))))
  generalizations (mult-ele-struc-delete-1))

(defunit bag-delete
  worth 500
  isa (math-concept math-op op anything struc-op bag-op binary-op)
  arity 2
  domain (anything bag)
  range (bag)
  elim-slots (applics)
  recursive-alg (lambda (x s)
                  (cond ((null s) nil)
                        ((equal x (car s))
                         (run-alg 'bag-delete x (cdr s)))
                        (t (cons (car s)
                                 (run-alg 'bag-delete x (cdr s))))))
  fast-alg remove
  generalizations (struc-delete))

(defunit bag-op
  generalizations (math-concept op math-op anything struc-op mult-ele-struc-op)
  worth 500
  isa (math-concept math-obj anything category)
  abbrev ("Bag Operations")
  examples (bag-insert bag-delete bag-delete-1 bag-equal bag-intersect bag-union bag-difference))

(defunit bag-insert
  worth 500
  isa (math-concept math-op op anything struc-op bag-op binary-op)
  arity 2
  domain (anything bag)
  range (bag)
  elim-slots (applics)
  fast-alg cons
  generalizations (struc-insert mult-ele-struc-insert))

(defunit bag
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  generator ((nil)
             (get-a-list)
             (old))
  fast-defn listp
  recursive-defn (lambda (s)
                   (cond ((not (consp s))
                          (eq s nil))
                         (t (run-defn 'bag (cdr s)))))
  generalizations (anything structure mult-ele-struc un-ord-struc)
  specializations (set o-set pair empty-struc non-empty-struc)
  in-domain-of (bag-insert bag-delete bag-delete-1 bag-equal bag-intersect bag-union bag-difference)
  is-range-of (bag-insert bag-delete bag-delete-1 bag-intersect bag-union bag-difference)
  rarity (0 2 2)
  elim-slots (examples))

(defunit list-delete-1
  worth 500
  isa (math-concept math-op anything struc-op list-op binary-op)
  arity 2
  domain (anything list)
  range (list)
  elim-slots (applics)
  recursive-alg (lambda (x s)
                  (cond ((null s) nil)
                        ((equal x (car s))
                         (cdr s))
                        (t (cons (car s)
                                 (run-alg 'list-delete-1 x (cdr s))))))
  generalizations (mult-ele-struc-delete-1))

(defunit list-delete
  worth 500
  isa (math-concept math-op op anything struc-op list-op binary-op)
  arity 2
  domain (anything list)
  range (list)
  elim-slots (applics)
  fast-alg remove
  recursive-alg (lambda (x s)
                  (cond ((null s) nil)
                        ((equal x (car s))
                         (run-alg 'list-delete x (cdr s)))
                        (t (cons (car s)
                                 (run-alg 'list-delete x (cdr s))))))
  generalizations (struc-delete))

(defunit list
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  generator ((nil)
             (get-a-list)
             (old))
  fast-defn listp
  recursive-defn (lambda (s)
                   (cond ((not (consp s))
                          ;; Checking for proper list termination
                          (eq s nil))
                         (t (run-defn 'list (cdr s)))))
  generalizations (anything structure mult-ele-struc ord-struc)
  is-range-of (list-insert list-delete list-delete-1 list-intersect list-union list-difference)
  in-domain-of (list-insert list-delete list-delete-1 list-equal list-intersect list-union list-difference)
  specializations (set o-set o-pair empty-struc non-empty-struc)
  rarity (0 2 2)
  elim-slots (examples))

(defunit list-insert
  worth 500
  isa (math-concept math-op op anything struc-op list-op binary-op)
  arity 2
  domain (anything list)
  range (list)
  elim-slots (applics)
  fast-alg cons
  generalizations (struc-insert mult-ele-struc-insert))

(defunit list-op
  generalizations (math-concept op math-op anything struc-op mult-ele-struc-op ord-struc-op)
  worth 500
  isa (math-concept math-obj anything category)
  abbrev ("List Operations")
  examples (list-insert list-delete list-delete-1 list-equal list-intersect list-union
                        list-difference reverse-o-pair))

(defunit set-delete
  worth 500
  isa (math-concept math-op op anything struc-op set-op binary-op)
  arity 2
  domain (anything set)
  range (set)
  elim-slots (applics)
  recursive-alg (lambda (x s)
                  (cond ((null s) nil)
                        ((equal x (car s))
                         (cdr s))
                        (t (cons (car s)
                                 (run-alg 'set-delete x (cdr s))))))
  fast-alg remove
  generalizations (struc-delete))

(defunit set-insert
  worth 500
  isa (math-concept math-op op anything struc-op set-op binary-op)
  arity 2
  domain (anything set)
  range (set)
  elim-slots (applics)
  fast-alg (lambda (x s)
             (cond ((member x s) s)
                   (t (cons x s))))
  recursive-alg (lambda (x s)
                  (cond ((null s)
                         (cons x s))
                        ((equal x (car s))
                         s)
                        (t (cons (car s)
                                 (run-alg 'set-insert x (cdr s))))))
  generalizations (struc-insert))

(defunit struc-delete
  worth 500
  isa (math-concept math-op op anything struc-op binary-op)
  arity 2
  domain (anything structure)
  range (structure)
  elim-slots (applics)
  specializations (list-delete bag-delete set-delete o-set-delete))

(defunit struc-op
  generalizations (math-concept op math-op anything)
  worth 500
  isa (math-concept math-obj anything category)
  abbrev ("Operations on structures")
  examples (struc-insert struc-delete random-choose random-subset good-choose best-choose
                         best-subset good-subset set-insert set-delete list-insert
                         list-delete list-delete-1 bag-insert bag-delete bag-delete-1
                         mult-ele-struc-delete-1 o-set-insert o-set-delete o-set-equal
                         set-equal bag-equal list-equal ord-struc-equal set-intersect
                         set-union struc-intersect list-intersect o-set-intersect
                         bag-intersect struc-union o-set-union list-union bag-union
                         struc-difference set-difference list-difference o-set-difference
                         bag-difference mult-ele-struc-insert)
  specializations (set-op list-op bag-op mult-ele-struc-op o-set-op ord-struc-op logic-op))

(defunit struc-insert
  worth 500
  isa (math-concept math-op op anything struc-op binary-op)
  arity 2
  domain (anything structure)
  range (structure)
  elim-slots (applics)
  specializations (list-insert bag-insert set-insert o-set-insert))

(defunit and
  worth 569
  isa (op pred math-op math-pred anything binary-op logic-op binary-pred)
  fast-alg (lambda (x y)
             (and x y))
  arity 2
  domain (anything anything)
  range (anything)
  elim-slots (applics)
  generalizations (the-second-of the-first-of or)
  rarity (1.0 2 0))

(defunit abbrev
  worth 307
  isa (slot non-criterial-slot repr-concept anything)
  data-type text)

(defunit add
  worth 500
  isa (math-concept math-op op num-op anything binary-op)
  fast-alg (lambda (x y)
             (+ x y))
  recursive-alg (lambda (x y)
                  (cond ((eq x 0)
                         y)
                        (t (run-alg 'successor (run-alg 'add (1- x) y)))))
  unitized-alg (lambda (x y)
                 (cond ((eq x 0) y)
                       (t (run-alg 'successor (run-alg 'add (1- x) y)))))
  iterative-alg (lambda (x y)
                  (loop for i from 1 to x
                        do (incf y))
                  y)
  arity 2
  domain (nnumber nnumber)
  range (nnumber)
  elim-slots (applics))

(defunit alg
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-fn
  sub-slots (fast-alg iterative-alg recursive-alg unitized-alg))

(defunit always-nil
  worth 500
  isa (op pred anything constant-pred unary-op math-op unary-pred)
  arity 1
  domain (anything)
  range (bit)
  elim-slots (applics)
  generalizations (constant-unary-pred)
  fast-alg (lambda (x)
             (declare (ignore x))
             nil))

(defunit always-nil-2
  worth 500
  isa (op pred anything constant-pred binary-op math-op binary-pred)
  arity 2
  domain (anything anything)
  range (bit)
  elim-slots (applics)
  generalizations (constant-binary-pred)
  fast-alg (lambda (x y)
             (declare (ignore x y))
             nil))

(defunit always-t
  worth 500
  isa (op pred anything constant-pred unary-op math-op unary-pred)
  arity 1
  domain (anything)
  range (bit)
  elim-slots (applics)
  generalizations (constant-unary-pred)
  fast-alg (lambda (x)
             (declare (ignore x))
             t))

(defunit always-t-2
  worth 500
  isa (op pred anything constant-pred binary-op math-op binary-pred)
  arity 2
  domain (anything anything)
  range (bit)
  elim-slots (applics)
  generalizations (constant-binary-pred)
  fast-alg (lambda (x y)
             (declare (ignore x y))
             t))

(defunit anything
  worth 550
  specializations (set heuristic slot nnumber unit prime-num conjecture even-num
                       task odd-num perf-num perf-square set-of-numbers criterial-slot
                       bit non-criterial-slot hind-sight-rule unary-unit-op math-concept
                       repr-concept math-op math-obj set-op unit-op num-op math-pred op
                       pred record-slot structure constant-pred struc-op list-op list
                       bag bag-op mult-ele-struc-op mult-ele-struc o-set o-set-op
                       no-mult-ele-struc ord-struc un-ord-struc ord-struc-op unary-op
                       binary-op tertiary-op o-pair pair inverted-op set-of-o-pairs
                       relation logic-op atom truth-value structure-of-structures
                       set-of-sets empty-struc non-empty-struc unary-pred binary-pred
                       tertiary-pred)
  isa (repr-concept anything category)
  is-range-of (random-choose good-choose best-choose and or the-second-of the-first-of
                             first-ele second-ele third-ele all-but-first all-but-second
                             all-but-third last-ele all-but-last proj1 proj2 proj-1-of-3
                             proj-2-of-3 proj-3-of-3 identity-1 implies ord-struc-equal)
  in-domain-of (equal eq and or the-second-of the-first-of always-t always-nil
                      constant-binary-pred always-t-2 always-nil-2 constant-unary-pred
                      undefined-pred struc-insert struc-delete set-insert set-delete
                      list-insert list-delete list-delete-1 bag-insert bag-delete
                      bag-delete-1 mult-ele-struc-delete-1 o-set-insert o-set-delete member
                      memb proj1 proj2 proj-1-of-3 proj-2-of-3 proj-3-of-3 identity-1 not
                      implies mult-ele-struc-insert)
  fast-defn (lambda (x)
              (declare (ignore x))
              t)
  examples (and or the-first-of the-second-of square divisors-of multiply-add successor
                random-choose random-subset good-choose best-choose best-subset
                good-subset equal ieqp eq ileq igeq ilessp igreaterp los1 los2 los3
                los4 los5 los6 los7 win1 t nil proto-conjec 1 3 5 7 9 11 13 15 17
                19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55 57 59 61
                63 65 67 69 71 73 75 77 79 81 83 85 6 28 if-about-to-work-on-task
                applics if-finished-working-on-task isa if-truly-relevant sub-slots
                if-parts if-potentially-relevant examples data-type english worth
                inverse creditors generalizations specializations then-add-to-agenda
                then-compute then-conjecture abbrev then-define-new-concepts
                then-modify-slots then-print-to-user then-parts super-slots if-task-parts
                format dont-copy double-check generator if-working-on-task is-range-of
                to-delete-1 alg fast-defn recursive-defn unitized-defn fast-alg
                iterative-alg recursive-alg unitized-alg iterative-defn to-delete
                applic-generator arity non-examples compiled-defn elim-slots
                in-domain-of domain range indirect-applics direct-applics defn
                sib-slots transpose then-delete-old-concepts subsumes subsumed-by
                overall-record then-print-to-user-failed-record
                then-add-to-agenda-failed-record then-delete-old-concepts-failed-record
                then-define-new-concepts-failed-record then-conjecture-failed-record
                then-modify-slots-failed-record then-compute-failed-record
                then-print-to-user-record then-add-to-agenda-record
                then-delete-old-concepts-record then-define-new-concepts-record
                then-conjecture-record then-modify-slots-record then-compute-record
                record-for failed-record-for record failed-record h1 h5 h6 h3 h4 h7 h8
                h9 h10 h11 h2 h12 h-avoid h-avoid-2 h-avoid-3 h13 h14 h15 h16 h17 h18
                h19 h-avoid-2-and h-avoid-3-first h-avoid-if-working h5-criterial h5-good
                h19-criterial set heuristic anything math-concept slot math-obj
                nnumber unit prime-num conjecture repr-concept even-num task math-op
                odd-num perf-num perf-square op set-of-numbers set-op unit-op num-op
                criterial-slot pred math-pred bit non-criterial-slot hind-sight-rule
                unary-unit-op record-slot h20 conjectures h21 conjecture-about
                structure category struc-equal set-equal subsetp constant-pred
                always-t always-nil constant-binary-pred always-t-2 always-nil-2
                constant-unary-pred compose undefined-pred struc-insert struc-op
                struc-delete set-insert set-delete list-op list list-insert list-delete
                list-delete-1 bag bag-op bag-insert bag-delete bag-delete-1 mult-ele-struc
                mult-ele-struc-op mult-ele-struc-delete-1 o-set o-set-insert o-set-op
                o-set-delete no-mult-ele-struc ord-struc un-ord-struc nec-defn suf-defn
                o-set-equal bag-equal list-equal ord-struc-op ord-struc-equal set-intersect
                set-union struc-intersect list-intersect o-set-intersect bag-intersect
                struc-union o-set-union list-union bag-union struc-difference
                set-difference list-difference o-set-difference bag-difference coalesce
                type-of-structure unary-op parallel-replace each-element-is-a binary-op
                parallel-replace-2 repeat tertiary-op repeat2 parallel-join
                parallel-join-2 o-pair pair reverse-o-pair first-ele second-ele third-ele
                all-but-first all-but-second all-but-third last-ele all-but-last member
                memb proj1 proj2 proj-1-of-3 proj-2-of-3 proj-3-of-3 identity-1 restrict
                inverted-op invert-op set-of-o-pairs relation logic-op not implies atom
                truth-value structure-of-structures set-of-sets empty-struc
                non-empty-struc undefined lower-arity higher-arity unary-pred
                binary-pred tertiary-pred pred-cat-by-nargs op-cat-by-nargs extensions
                restrictions interestingness h22 more-interesting less-interesting
                int-examples h23 h24 why-int rarity is-a-int h25 h26 h27 28 h29
                mult-ele-struc-insert int-applics english-1 restric-random-subset-3)
  rarity (1 12 0))

(defunit applic-generator
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-fn
  format (applic-gen-init applic-gen-build applic-gen-args))

(defunit applics
  worth 338
  isa (slot non-criterial-slot repr-concept anything)
  format ((situation resultant-units directness)
          (situation resultant-units directness)
          etc.)
  data-type io-pair
  sub-slots (direct-applics indirect-applics int-applics)
  double-check t
  dont-copy t
  more-interesting (int-applics))

(defunit arity
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type number)

(defunit best-choose
  worth 500
  isa (math-concept math-op op set-op anything struc-op unary-op)
  fast-alg best-choose
  domain (set)
  range (anything)
  generalizations (random-choose good-choose)
  elim-slots (applics)
  arity 1)

(defunit best-subset
  worth 500
  isa (math-concept math-op op set-op anything struc-op unary-op)
  fast-alg best-subset
  domain (set)
  range (set)
  generalizations (random-subset good-subset)
  elim-slots (applics)
  arity 1
  rarity (0.95 19 1))

(defunit bit
  is-range-of (equal ieqp eq ileq igeq ilessp igreaterp struc-equal set-equal subsetp
                     always-t always-nil constant-binary-pred always-t-2 always-nil-2
                     constant-unary-pred o-set-equal bag-equal list-equal member memb not)
  worth 500
  isa (math-concept math-obj anything category)
  examples (t nil)
  fast-defn (lambda (b)
              (or (eq b nil)
                  (eq b t)))
  generalizations (anything))

(defunit category
  worth 500
  isa (category anything repr-concept)
  examples (set heuristic anything math-concept slot math-obj nnumber unit prime-num
                conjecture repr-concept even-num task math-op odd-num perf-num
                perf-square op set-of-numbers set-op unit-op num-op criterial-slot pred
                math-pred bit non-criterial-slot hind-sight-rule unary-unit-op record-slot
                structure category constant-pred struc-op list-op list bag bag-op
                mult-ele-struc mult-ele-struc-op o-set o-set-op no-mult-ele-struc ord-struc
                un-ord-struc ord-struc-op type-of-structure unary-op binary-op tertiary-op
                o-pair pair inverted-op set-of-o-pairs relation logic-op atom truth-value
                structure-of-structures set-of-sets empty-struc non-empty-struc unary-pred
                binary-pred tertiary-pred pred-cat-by-nargs op-cat-by-nargs)
  specializations (type-of-structure pred-cat-by-nargs op-cat-by-nargs)
  ;; TODO - this is evaluating based on a call to a newer heuristic? also, interp2 is the default interpreter, not interp3?
  interestingness (interp3 'h24 u 'why-int))

(defunit compiled-defn
  super-slots (defn)
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type compiled-lisp-code)

(defunit compose
  isa (math-concept math-op op anything binary-op)
  worth 990
  arity 2
  domain (op op)
  range (op)
  elim-slots (applics)
  fast-alg (lambda (f g)
             (cond ((and (range f)
                         (domain g)
                         (is-a-kind-of (car (range f))
                                       (car (domain g))))
                    (let ((fargs (mapcar #'the-second-of
                                         (domain f)
                                         '(u v w x y z z2 z3 z4 z5)))
                          (gargs (mapcar #'the-second-of
                                         (cdr (domain g))
                                         '(a b c d e f g h i j k)))
                          (nam (create-unit (pack* g '-o- f))))
                      (put nam 'isa (set-diff (isa g)
                                              (examples 'op-cat-by-nargs)))
                      (put nam 'worth (average-worths 'compose (average-worths f g)))
                      (put nam 'arity (+ (length fargs)
                                         (length gargs)))
                      (put nam 'domain (append (copy (domain f))
                                               (cdr (domain g))))
                      (put nam 'range (copy (range g)))
                      (put nam 'unitized-alg
                           (compile-report
                            `(lambda ,(nconc (copy fargs) (copy gargs))
                              (run-alg ',g (run-alg ',f ,@fargs) ,@gargs))))
                      (put nam 'elim-slots '(applics))
                      (put nam 'creditors '(compose))
                      (put nam 'isa (append (isa nam)
                                            (subset (examples 'op-cat-by-nargs)
                                                    (lambda (pc)
                                                      (run-defn pc nam)))))
                      (add-inv nam)
                      nam))
                   (t ;; ORIG: we should check for cases where f could sub for other than the first arg of g
                    'failed)))
  rarity (0.3612903 56 99))

(defunit conjecture
  worth 500
  examples (proto-conjec)
  isa (repr-concept anything category)
  generalizations (anything))

(defunit conjecture-about
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  double-check t
  dont-copy t
  inverse (conjectures))

(defunit conjectures
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type conjecture
  double-check t
  dont-copy t
  inverse (conjecture-about))

(defunit constant-binary-pred
  worth 500
  isa (op pred anything unary-op math-op binary-pred)
  arity 2
  domain (anything)
  range (bit)
  elim-slots (applics)
  specializations (always-t-2 always-nil-2))

(defunit constant-pred
  generalizations (op pred anything)
  worth 500
  isa (anything category math-op repr-concept)
  examples (always-t always-nil always-t-2 always-nil-2))

(defunit constant-unary-pred
  worth 500
  isa (op pred anything unary-op math-op unary-pred)
  arity 1
  domain (anything)
  range (bit)
  elim-slots (applics)
  specializations (always-t always-nil))

(defunit creditors
  to-delete-1 (lambda (u1 p u2)
                (declare (ignore p))
                ;; ORIG: U1 is on the P property of unit U2, and is now being deleted. We must remove accreditation of U2 from the Applics slot of U1
                ;; TODO - but P is unused in the original code, too? or is that another dynamic binding? or it's legitimately ignored, as we're removing effects of stuff being deleted from P, but not ever accessing it ourselves?
                (rem1prop u1 'applics (find-if (lambda (a)
                                                 (eq (caadr a) u2))
                                               (applics u1))))
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit)

(defunit criterial-slot
  isa (repr-concept math-concept anything category)
  worth 500
  generalizations (slot anything repr-concept)
  examples (alg applic-generator compiled-defn data-type defn domain elim-slots
                fast-alg fast-defn generator if-about-to-work-on-task
                if-finished-working-on-task if-parts if-potentially-relevant
                if-task-parts if-truly-relevant if-working-on-task iterative-alg
                iterative-defn non-examples recursive-alg recursive-defn
                then-add-to-agenda then-compute then-conjecture
                then-define-new-concepts then-modify-slots then-parts
                then-print-to-user to-delete to-delete-1 unitized-alg unitized-defn
                then-delete-old-concepts nec-defn suf-defn each-element-is-a))

(defunit data-type
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type data-type
  double-check t)

(defunit defn
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type list-pred
  sub-slots (compiled-defn fast-defn iterative-defn recursive-defn unitized-defn suf-defn nec-defn)
  specializations (nec-defn suf-defn))

(defunit direct-applics
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  format ((situation resultant-units directness)
          (situation resultant-units directness)
          etc.)
  data-type io-pair
  super-slots (applics)
  double-check t
  dont-copy t)

(defunit divisors-of
  worth 500
  isa (math-concept math-op op num-op anything unary-op)
  fast-alg (lambda (n)
             (sort (loop for i from 1
                         ;; OPTIMIZATION - hoist calculation
                         until (> (square i) n)
                         ;; OPTIMIZATION - cache computation between divides & floor
                         when (divides i n)
                           collect i
                           and
                             collect (floor n i))
                   #'<))
  iterative-alg (lambda (n)
                  (loop for i from 1 to n
                        when (divides i n)
                          collect i))
  domain (nnumber)
  range (set-of-numbers)
  elim-slots (applics)
  arity 1)

(defunit domain
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type unit
  inverse (in-domain-of))

(defunit dont-copy
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type bit)

(defunit double-check
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type bit)

(defunit eq
  worth 507
  isa (math-concept math-op op math-pred pred anything binary-op binary-pred)
  fast-alg eq
  arity 2
  domain (anything anything)
  range (bit)
  generalizations (equal)
  elim-slots (applics)
  is-a-int (binary-pred)
  rarity (0.1 1 9))

(defunit equal
  worth 502
  isa (math-concept math-op op math-pred pred anything binary-op binary-pred)
  fast-alg equal
  arity 2
  domain (anything anything)
  range (bit)
  specializations (ieqp eq struc-equal set-equal o-set-equal bag-equal list-equal)
  elim-slots (applics))

(defunit elim-slots
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type slot
  double-check t)

(defunit english
  worth 304
  isa (slot non-criterial-slot repr-concept anything)
  data-type text)

(defunit even-num
  generalizations (nnumber anything)
  worth 800
  unitized-defn (lambda (n)
                  (run-alg 'divides 2 n))
  isa (math-concept math-obj anything category)
  fast-defn (lambda (n)
              (and (fixp n)
                   ;; OPTIMIZATION - just test low bit
                   (divides 2 n)))
  elim-slots (examples))

(defunit examples
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  inverse (isa)
  data-type unit
  double-check t
  dont-copy t
  sub-slots (int-examples)
  more-interesting (int-examples))

(defunit failed-record
  worth 600
  isa (slot non-criterial-slot repr-concept anything)
  double-check t
  data-type slot
  inverse (failed-record-for))

(defunit failed-record-for
  worth 600
  isa (slot non-criterial-slot repr-concept anything)
  double-check t
  data-type slot
  inverse (failed-record))

(defunit fast-alg
  super-slots (alg)
  isa (slot criterial-slot repr-concept anything)
  worth 600
  data-type lisp-fn
  dont-copy t)

(defunit fast-defn
  super-slots (defn)
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-pred)

(defunit format
  worth 400
  isa (slot non-criterial-slot repr-concept anything)
  data-type data-type)

(defunit generalizations
  worth 306
  isa (slot non-criterial-slot repr-concept anything)
  sub-slots (super-slots extensions)
  inverse (specializations)
  data-type unit
  double-check t)

(defunit generator
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-fn
  format (gen-init gen-build gen-args))

(defunit good-choose
  worth 500
  isa (math-concept math-op op set-op anything struc-op unary-op)
  fast-alg good-choose
  domain (set)
  range (anything)
  generalizations (random-choose)
  specializations (best-choose)
  elim-slots (applics)
  arity 1)

(defunit good-subset
  worth 500
  isa (math-concept math-op op set-op anything struc-op unary-op)
  fast-alg good-subset
  domain (set)
  range (set)
  generalizations (random-subset)
  specializations (best-subset)
  elim-slots (applics)
  arity 1)

(defunit heuristic
  worth 900
  examples (h1 h5 h6 h3 h4 h7 h8 h9 h10 h11 h2 h12 h-avoid h-avoid-2 h-avoid-3 h13 h14
               h15 h16 h17 h18 h19 h-avoid-2-and h-avoid-3-first h-avoid-if-working
               h5-criterial h5-good h19-criterial h20 h21 h22 h23 h24 h25 h26 h27
               ;; TODO - H1-6 is referenced twice in EUR, but not found anywhere.
               ;;        Removed it, because it was always selected and did nothing
               ;;        There is also an h1-11 referred to in applics logs
               ;;        These are specializations of h1 that ran at some point, but weren't saved?
               ;; TODO - but why does the interlisp version work with these uncommented in live data?
               ;;        ensure that when this is re-enabled, it doesn't always try to fire it
               h28 h20 #|h1-6|#)
  isa (repr-concept anything category)
  generalizations (op anything repr-concept)
  specializations (hind-sight-rule))

(defunit hind-sight-rule
  worth 900
  isa (repr-concept anything category)
  generalizations (op heuristic anything repr-concept)
  abbrev ("Heuristic rules for learning from bitter experiences")
  examples (h12 h13 h14))

(defunit ieqp
  worth 500
  isa (math-concept math-op op math-pred pred anything num-op binary-op binary-pred)
  ;; OPTIMIZATION - a version with fixnum declarations, and fixed arity?
  fast-alg =
  arity 2
  domain (nnumber nnumber)
  range (bit)
  generalizations (equal ileq igeq)
  elim-slots (applics)
  is-a-int (binary-pred)
  rarity (0.1 1 9))

(defunit igeq
  worth 509
  isa (math-concept math-op op math-pred pred anything num-op binary-op binary-pred)
  fast-alg >= ;; OPTIMIZATION - fixed arity, fixnum decls
  arity 2
  domain (nnumber nnumber)
  range (bit)
  specializations (ieqp igreaterp)
  transpose (ileq)
  elim-slots (applics))

(defunit igreaterp
  worth 501
  isa (math-concept math-op op math-pred pred anything num-op binary-op binary-pred)
  fast-alg > ;; OPTIMIZATION
  arity 2
  domain (nnumber nnumber)
  range (bit)
  generalizations (igeq)
  transpose (ilessp)
  elim-slots (applics))

(defunit ileq
  worth 500
  isa (math-concept math-op op math-pred pred anything num-op binary-op binary-pred)
  fast-alg <=
  arity 2
  domain (nnumber nnumber)
  range (bit)
  specializations (ieqp ilessp)
  transpose (igeq)
  elim-slots (applics))

(defunit ilessp
  worth 500
  isa (math-concept math-op op math-pred pred anything num-op binary-op binary-pred)
  fast-alg <
  arity 2
  domain (nnumber nnumber)
  range (bit)
  generalizations (ileq)
  transpose (igreaterp)
  elim-slots (applics))

(defunit if-about-to-work-on-task
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (if-parts if-task-parts)
  data-type lisp-pred)

(defunit if-finished-working-on-task
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (if-task-parts if-parts)
  data-type lisp-pred)

(defunit if-parts
  worth 600
  sub-slots (if-potentially-relevant if-truly-relevant if-about-to-work-on-task
                                     if-working-on-task if-finished-working-on-task)
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-pred)

(defunit if-potentially-relevant
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (if-parts)
  data-type lisp-pred)

(defunit if-task-parts
  worth 600
  isa (slot criterial-slot repr-concept anything)
  sub-slots (if-about-to-work-on-task if-working-on-task if-finished-working-on-task)
  data-type lisp-pred)

(defunit if-truly-relevant
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (if-parts)
  data-type (lisp-pred))

(defunit if-working-on-task
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (if-parts if-task-parts)
  data-type lisp-pred)

(defunit in-domain-of
  inverse (domain)
  isa (slot non-criterial-slot repr-concept anything)
  worth 300
  data-type unit)

(defunit indirect-applics
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  format ((situation resultant-units directness)
          (situation resultant-units directness)
          etc.)
  data-type io-pair
  super-slots (applics)
  double-check t
  dont-copy t)

(defunit inverse
  worth 600
  isa (slot non-criterial-slot repr-concept anything)
  inverse (inverse)
  data-type slot
  double-check t)

(defunit isa
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  inverse (examples)
  data-type unit
  double-check t)

(defunit is-range-of
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  inverse (range))

(defunit iterative-alg
  super-slots (alg)
  isa (slot criterial-slot repr-concept anything)
  worth 600
  data-type lisp-fn)

(defunit iterative-defn
  super-slots (defn)
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-pred)

(defunit math-concept
  generalizations (anything)
  worth 500
  examples (nnumber prime-num perf-num perf-square odd-num even-num square
                    divisors-of multiply add successor set set-of-numbers
                    random-choose random-subset good-choose best-choose best-subset
                    good-subset bit equal ieqp eq ileq igeq ilessp igreaterp
                    slot unit criterial-slot non-criterial-slot math-concept
                    math-obj math-op math-pred num-op set-op los1 los2 los3 los4
                    los5 los6 los7 win1 record-slot structure struc-equal
                    set-equal subsetp compose struc-insert struc-op struc-delete
                    set-insert set-delete list-op list list-insert list-delete
                    list-delete-1 bag bag-op bag-insert bag-delete bag-delete-1
                    mult-ele-struc mult-ele-struc-op mult-ele-struc-delete-1 o-set
                    o-set-insert o-set-op o-set-delete no-mult-ele-struc ord-struc
                    un-ord-struc o-set-equal bag-equal list-equal ord-struc-op
                    ord-struc-equal set-intersect set-union struc-intersect
                    list-intersect o-set-intersect bag-intersect struc-union
                    o-set-union list-union bag-union struc-difference set-difference
                    list-difference o-set-difference bag-difference coalesce
                    parallel-replace parallel-replace-2 repeat repeat2
                    parallel-join parallel-join-2 o-pair pair reverse-o-pair first-ele
                    second-ele third-ele all-but-first all-but-second all-but-third
                    last-ele all-but-last member memb proj1 proj2 proj-1-of-3
                    proj-2-of-3 proj-3-of-3 identity-1 restrict inverted-op invert-op
                    set-of-o-pairs relation logic-op structure-of-structures
                    set-of-sets empty-struc non-empty-struc mult-ele-struc-insert
                    restric-random-subset-3)
  specializations (math-op math-obj set-op unit-op num-op math-pred struc-op list-op
                           bag-op mult-ele-struc-op o-set-op ord-struc-op inverted-op
                           logic-op)
  isa (math-concept math-obj anything category))

(defunit math-obj
  generalizations (math-concept anything)
  worth 500
  examples (nnumber prime-num perf-num perf-square odd-num even-num set set-of-numbers bit
                    math-concept num-op set-op math-pred math-obj math-op los1 los2 los3
                    los4 los5 los6 los7 win1 structure struc-op list-op list bag
                    bag-op mult-ele-struc mult-ele-struc-op o-set o-set-op no-mult-ele-struc
                    ord-struc un-ord-struc ord-struc-op o-pair pair inverted-op
                    set-of-o-pairs relation logic-op structure-of-structures set-of-sets
                    empty-struc non-empty-struc truth-value)
  isa (math-concept math-obj anything category))

(defunit math-op
  generalizations (math-concept op anything)
  worth 500
  examples (divisors-of square multiply add successor random-choose random-subset
                        good-choose best-choose best-subset good-subset equal ieqp eq
                        ileq igeq ilessp igreaterp and or the-first-of the-second-of
                        struc-equal set-equal subsetp compose struc-insert struc-delete
                        set-insert set-delete list-insert list-delete list-delete-1
                        bag-insert bag-delete bag-delete-1 mult-ele-struc-delete-1 o-set-insert
                        o-set-delete o-set-equal bag-equal list-equal ord-struc-equal
                        set-intersection set-union struc-intersect list-intersect
                        o-set-intersect bag-intersect struc-union o-set-union list-union
                        bag-union struc-difference set-difference list-difference
                        o-set-difference bag-difference coalesce parallel-replace
                        parallel-replace-2 repeat repeat2 parallel-join parallel-join-2
                        reverse-o-pair first-ele second-ele third-ele all-but-first
                        all-but-second all-but-third last-ele all-but-last member memb proj1
                        proj2 proj-1-of-3 proj-2-of-3 proj-3-of-3 identity-1 restrict invert-op
                        not implies always-nil always-nil-2 always-t always-t-2
                        constant-binary-pred constant-pred constant-unary-pred
                        undefined-pred mult-ele-struc-insert restric-random-subset-3)
  isa (math-concept math-obj anything category)
  specializations (set-op unit-op num-op struc-op list-op bag-op mult-ele-struc-op o-set-op
                          ord-struc-op inverted-op logic-op))

(defunit math-pred
  generalizations (math-concept op pred anything)
  worth 500
  isa (math-concept math-obj anything category)
  examples (equal ieqp eq ileq igeq ilessp igreaterp and or the-first-of the-second-of
                  struc-equal set-equal subsetp o-set-equal bag-equal list-equal member
                  memb not implies))

(defunit multiply
  worth 500
  isa (math-concept math-op op num-op anything binary-op)
  fast-alg (lambda (x y)
             (* x y))
  recursive-alg (lambda (x y)
                  (cond ((eq x 0) 0)
                        ((eq x 1) y)
                        (t (run-alg 'add y (run-alg 'multiply (1- x) y)))))
  unitized-alg (lambda (x y)
                 (cond ((eq x 0) 0)
                       ((eq x 1) y)
                       (t (run-alg 'add y (run-alg 'multiply (1- x) y)))))
  iterative-alg (lambda (x y)
                  (loop for i from 1 to x
                        sum y))
  arity 2
  domain (nnumber nnumber)
  range (nnumber)
  elim-slots (applics))

(defunit nnumber
  worth 500
  isa (math-concept math-obj anything category)
  specializations (prime-num perf-num perf-square odd-num even-num)
  generator ((0) (1+) (old))
  fast-defn (lambda (n) (fixp n))
  in-domain-of (divisors-of multiply add successor square ieqp ileq igeq ilessp igreaterp)
  is-range-of (multiply add successor)
  elim-slots (examples)
  generalizations (anything)
  rarity (0 1 3))

(defunit non-criterial-slot
  isa (repr-concept math-concept anything category)
  worth 500
  generalizations (slot anything repr-concept)
  examples (abbrev applics arity creditors direct-applics dont-copy
                   double-check english examples format generalizations
                   in-domain-of indirect-applics isa is-range-of range sib-slots
                   specializations sub-slots super-slots transpose worth
                   inverse subsumes subsumed-by overall-record
                   then-print-to-user-failed-record then-add-to-agenda-failed-record
                   then-delete-old-concepts-failed-record
                   then-define-new-concepts-failed-record
                   then-conjecture-failed-record then-modify-slots-failed-record
                   then-compute-failed-record then-print-to-user-record
                   then-add-to-agenda-record then-delete-old-concepts-record
                   then-define-new-concepts-record then-conjecture-record
                   then-modify-slots-record then-compute-record record-for
                   failed-record-for record failed-record conjectures
                   conjecture-about lower-arity higher-arity extensions
                   restrictions interestingness more-interesting
                   less-interesting int-examples why-int rarity is-a-int
                   int-applics))

(defunit non-examples
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type unit
  double-check t
  dont-copy t)

(defunit num-op
  generalizations (math-concept op math-op anything)
  worth 500
  isa (math-concept math-obj anything category)
  abbrev ("Numeric Operations")
  examples (divisors-of square multiply add successor ieqp ileq igeq ilessp igreaterp))

(defunit or
  worth 500
  isa (op pred math-op math-pred anything binary-op logic-op binary-pred)
  fast-alg (lambda (x y)
             (or x y))
  arity 2
  domain (anything anything)
  range (anything)
  elim-slots (applics)
  specializations (the-first-of the-second-of and))

(defunit odd-num
  generalizations (nnumber anything)
  worth 700
  unitized-defn (lambda (n)
                  ;; BUGFIX - missed quote on DIVIDES
                  (not (run-alg 'divides 2 n)))
  isa (math-concept math-obj anything category)
  fast-defn (lambda (n)
              (and (fixp n)
                   (oddp n)))
  elim-slots (examples))

(defunit op
  worth 500
  isa (repr-concept anything category)
  specializations (math-op heuristic set-op unit-op num-op pred math-pred hind-sight-rule
                           constant-pred struc-op list-op bag-op mult-ele-struc-op o-set-op
                           ord-struc-op unary-op binary-op tertiary-op inverted-op logic-op
                           unary-pred binary-pred tertiary-pred)
  examples (random-choose random-subset good-choose-best-choose best-subset good-subset
                          divisors-of square multiply add successor equal ieqp eq ileq
                          igeq ilessp igreaterp h12 h13 h14 h1 h5 h6 h3 h4 h7 h8 h9 h10
                          h11 h2 h-avoid h-avoid-2 h-avoid-3 h15 and or the-second-of the-first-of
                          h19 h-avoid-2-and h-avoid-3-first h-avoid-if-working h5-criterial h5-good
                          h19-criterial h20 h21 struc-equal set-equal subsetp always-t
                          always-nil constant-binary-pred always-t-2 always-nil-2
                          constant-unary-pred compose undefined-pred struc-insert struc-delete
                          set-insert set-delete list-insert list-delete list-delete-1 bag-insert
                          bag-delete bag-delete-1 mult-ele-struc-delete-1 o-set-insert o-set-delete
                          o-set-equal bag-equal list-equal ord-struc-equal set-intersect
                          set-union struc-intersect list-intersect o-set-intersect
                          bag-intersect struc-union o-set-union list-union bag-union
                          struc-difference set-difference list-difference o-set-difference
                          bag-difference coalesce parallel-replace parallel-replace-2 repeat
                          repeat2 parallel-join parallel-join-2 reverse-o-pair first-ele
                          second-ele third-ele all-but-first all-but-second all-but-third last-ele
                          all-but-last member memb proj1 proj2 proj-1-of-3 proj-2-of-3 proj-3-of-3
                          identity-1 restrict invert-op not implies h22 h23 h24 h29 h16 h17
                          h18 h25 h26 h27 h28 mult-ele-struc-insert #|h1-6|#)
  generalizations (anything)
  in-domain-of (compose coalesce restrict invert-op)
  is-range-of (compose coalesce restrict))

(defunit overall-record
  worth 300
  isa (slot non-criterial-slot repr-concept anything record-slot)
  data-type dotted-pair
  dont-copy t)

(defunit perf-num
  generalizations (nnumber anything)
  worth 800
  unitized-defn (lambda (n)
                  (eq (run-alg 'double n)
                      (apply #'+ (run-alg 'divisors-of n))))
  isa (math-concept math-obj anything category)
  iterative-defn (lambda (n)
                   (and (fixp n)
                        (eq (1- n)
                            (loop for i from 2 to (1- n)
                                  sum (cond ((divides i n) i)
                                            (t 0))))))
  elim-slots nil
  non-examples (0 1)
  examples (6 28))

(defunit perf-square
  generalizations (nnumber anything)
  worth 950
  is-range-of (square)
  isa (math-concept math-obj anything category)
  elim-slots (examples))

(defunit pred
  generalizations (op anything)
  worth 500
  isa (repr-concept anything category)
  abbrev ("Boolean predicates")
  specializations (math-pred constant-pred unary-pred binary-pred tertiary-pred)
  examples (equal ieqp eq ileq igeq ilessp igreaterp and or the-second-of the-first-of
                  struc-equal set-equal subsetp always-t always-nil constant-binary-pred
                  always-t-2 always-nil-2 constant-unary-pred undefined-pred o-set-equal
                  bag-equal list-equal member memb not implies))

(defunit prime-num
  generalizations (nnumber anything)
  worth 950
  unitized-defn (lambda (n)
                  (run-defn (run-alg 'divisors-of n)
                            'doubleton))
  isa (math-concept math-obj anything category)
  iterative-defn (lambda (n)
                   (and (fixp n)
                        (eq 0 (loop for i from 2 to (1- n)
                                    sum (cond ((divides i n) i)
                                              (t 0))))))
  fast-defn (lambda (n)
              (and (fixp n)
                   (loop for i from 2 to (isqrt n)
                         never (divides i n))))
  non-examples (0 1)
  elim-slots (examples))

(defunit proto-conjec
  worth 802
  isa (conjecture repr-concept anything))

(defunit random-choose
  worth 507
  isa (math-concept math-op op set-op anything struc-op unary-op)
  fast-alg random-choose
  domain (set)
  range (anything)
  specializations (good-choose best-choose)
  elim-slots (applics)
  arity 1)

(defunit random-subset
  worth 520
  isa (math-concept math-op op set-op anything struc-op unary-op)
  fast-alg random-subset
  domain (set)
  range (set)
  specializations (best-subset good-subset)
  elim-slots (applics)
  arity 1
  rarity (0.4065041 50 73))

(defunit range
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  inverse (is-range-of))

(defunit record
  worth 600
  isa (slot non-criterial-slot repr-concept anything)
  double-check t
  data-type slot
  inverse (record-for))

(defunit record-for
  worth 600
  isa (slot non-criterial-slot repr-concept anything)
  double-check t
  data-type slot
  inverse (record))

(defunit record-slot
  isa (repr-concept math-concept anything category)
  worth 500
  generalizations (slot anything repr-concept)
  examples (then-compute-record then-compute-failed-record then-modify-slots-record
                                then-modify-slots-failed-record then-conjecture-record
                                then-conjecture-failed-record
                                then-define-new-concepts-record
                                then-define-new-concepts-failed-record
                                then-delete-old-concepts-record
                                then-delete-old-concepts-failed-record
                                then-add-to-agenda-record then-add-to-agenda-failed-record
                                then-print-to-user-record then-print-to-user-failed-record
                                overall-record))

(defunit recursive-alg
  super-slots (alg)
  isa (slot criterial-slot repr-concept anything)
  worth 600
  data-type lisp-fn)

(defunit recursive-defn
  super-slots (defn)
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-pred)

(defunit repr-concept
  generalizations (anything)
  worth 500
  examples (slot unit criterial-slot non-criterial-slot heuristic hind-sight-rule
                 unit-op unary-unit-op repr-concept conjecture task anything pred
                 op proto-conjec abbrev alg applic-generator applics arity
                 compiled-defn creditors data-type defn direct-applics domain
                 dont-copy double-check elim-slots english examples failed-record
                 failed-record-for fast-alg fast-defn format generalizations
                 generator if-about-to-work-on-task if-finished-working-on-task if-parts
                 if-potentially-relevant if-task-parts if-truly-relevant
                 if-working-on-task in-domain-of indirect-applics inverse isa
                 is-range-of iterative-alg iterative-defn non-examples overall-record
                 range record record-for recursive-alg recursive-defn sib-slots
                 specializations sub-slots subsumed-by subsumes super-slots
                 then-add-to-agenda then-add-to-agenda-failed-record
                 then-add-to-agenda-record then-compute then-compute-failed-record
                 then-compute-record then-conjecture then-conjecture-failed-record
                 then-conjecture-record then-define-new-concepts
                 then-define-new-concepts-failed-record then-define-new-concepts-record
                 then-delete-old-concepts then-delete-old-concepts-failed-record
                 then-delete-old-concepts-record then-modify-slots
                 then-modify-slots-failed-record then-modify-slots-record then-parts
                 then-print-to-user then-print-to-user-failed-record
                 then-print-to-user-record to-delete to-delete-1 transpose unitized-alg
                 unitized-defn worth record-slot conjectures conjecture-about
                 category nec-defn suf-defn type-of-structure unary-op
                 each-element-is-a binary-op tertiary-op atom constant-pred undefined
                 lower-arity higher-arity unary-pred binary-pred tertiary-pred
                 pred-cat-by-nargs op-cat-by-nargs extensions restrictions
                 interestingness more-interesting less-interesting int-examples
                 why-int rarity is-a-int int-applics english-1)
  isa (repr-concept anything category)
  specializations (slot criterial-slot non-criterial-slot unit heuristic hind-sight-rule record-slot))

(defunit set
  worth 500
  isa (math-concept math-obj anything category type-of-structure)
  generator ((nil)
             (get-a-set)
             (old))
  fast-defn (lambda (s)
              (or (eq s nil)
                  (no-repeats-in s)))
  recursive-defn (lambda (s)
                   (cond ((not (consp s))
                          (eq s nil))
                         (t (and (not (member (car s) (cdr s)))
                                 (run-defn 'set (cdr s))))))
  in-domain-of (random-choose random-subset good-choose best-choose best-subset good-subset
                              set-equal subsetp set-insert set-delete set-intersect set-union
                              set-difference)
  is-range-of (random-subset best-subset good-subset set-insert set-delete set-intersect
                             set-union set-difference restric-random-subset-2-1
                             restric-random-subset-1-2)
  generalizations (anything structure bag list no-mult-ele-struc un-ord-struc)
  specializations (o-set empty-struc non-empty-struc set-of-sets)
  rarity (0 2 2)
  elim-slots (examples))

(defunit set-equal
  worth 500
  isa (math-concept math-op op math-pred pred anything struc-op set-op binary-op binary-pred)
  arity 2
  domain (set set)
  range (bit)
  generalizations (equal struc-equal subsetp)
  fast-alg (lambda (s1 s2)
             (cond ((not (eq (length s1)
                             (length s2)))
                    nil)
                   ((equal s1 s2)
                    t)
                   (t (and (is-subset-of s1 s2)
                           (is-subset-of s2 s1)))))
  recursive-alg (lambda (s1 s2)
                  (cond ((and (null s1)
                              (null s2))
                         t)
                        (t (and (consp s1)
                                (consp s2)
                                (member (car s1) s2)
                                (run-alg 'set-equal (cdr s2) (remove (car s1) s2))))))
  unitized-alg (lambda (s1 s2)
                 (and (run-alg 'subsetp s1 s2)
                      (run-alg 'subsetp s2 s1)))
  specializations (o-set-equal)
  is-a-int (binary-pred)
  rarity (0.1 1 9))

(defunit set-of-numbers
  is-range-of (divisors-of)
  isa (math-concept math-obj anything category)
  worth 500
  unitized-defn (lambda (s)
                  (and (run-defn 'set s)
                       (every (lambda (n)
                                (run-defn 'nnumber n))
                              s)))
  fast-defn (lambda (s)
              (and (run-defn 'set s)
                   (every #'numberp s)))
  elim-slots (examples)
  generalizations (anything)
  each-element-is-a nnumber)

(defunit set-op
  generalizations (math-concept op math-op anything struc-op)
  worth 500
  isa (math-concept math-obj anything category)
  abbrev ("Set Operations")
  specializations (unit-op)
  examples (random-choose random-subset good-choose best-choose best-subset good-subset
                          set-insert set-delete set-equal set-intersect set-union set-difference))

(defunit sib-slots
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  inverse (sib-slots)
  data-type slot
  double-check t)

(defunit slot
  isa (repr-concept math-concept anything category)
  worth 530
  examples (if-about-to-work-on-task applics if-finished-working-on-task isa if-truly-relevant
                                     sub-slots if-parts if-potentially-relevant examples
                                     data-type english worth inverse creditors
                                     generalizations specializations then-add-to-agenda
                                     then-compute then-conjecture abbrev
                                     then-define-new-concepts then-modify-slots then-print-to-user
                                     then-parts super-slots if-task-parts format dont-copy
                                     double-check generator if-working-on-task is-range-of
                                     to-delete-1 alg fast-defn recursive-defn unitized-defn
                                     fast-alg iterative-alg recursive-alg unitized-alg
                                     iterative-defn to-delete applic-generator arity
                                     non-examples compiled-defn elim-slots in-domain-of domain
                                     range indirect-applics direct-applics defn sib-slots
                                     transpose then-delete-old-concepts subsumes subsumed-by
                                     overall-record then-print-to-user-failed-record
                                     then-add-to-agenda-failed-record
                                     then-delete-old-concepts-failed-record
                                     then-define-new-concepts-failed-record
                                     then-conjecture-failed-record then-modify-slots-failed-record
                                     then-compute-failed-record then-print-to-user-record
                                     then-add-to-agenda-record then-delete-old-concepts-record
                                     then-define-new-concepts-record then-conjecture-record
                                     then-modify-slots-record then-compute-record record-for
                                     failed-record-for record failed-record conjectures
                                     conjecture-about nec-defn suf-defn each-element-is-a
                                     lower-arity higher-arity extensions restrictions
                                     interestingness more-interesting less-interesting
                                     int-examples why-int rarity is-a-int int-applics)
  specializations (criterial-slot non-criterial-slot record-slot)
  generalizations (unary-unit-op repr-concept anything))

(defunit specializations
  worth 356
  isa (slot non-criterial-slot repr-concept anything)
  sub-slots (sub-slots restrictions)
  inverse (generalizations)
  data-type unit
  double-check t)

(defunit square
  worth 500
  unitized-alg (lambda (n)
                 (run-alg 'multiply n n))
  isa (math-concept math-op op num-op anything unary-op)
  fast-alg (lambda (n)
             (* n n))
  domain (nnumber)
  range (perf-square)
  elim-slots (applics)
  arity 1
  rarity (1.0 220 0))

(defunit struc-equal
  worth 500
  isa (math-concept math-op op math-pred pred anything binary-op binary-pred)
  arity 2
  domain (structure structure)
  range (bit)
  elim-slots (applics)
  generalizations (equal)
  specializations (set-equal o-set-equal bag-equal list-equal)
  is-a-int (binary-pred)
  rarity (0.02 1 49))

(defunit structure
  worth 500
  isa (math-concept math-obj anything category)
  fast-defn listp
  recursive-defn (lambda (s)
                   (cond ((not (consp s))
                          (eq s nil))
                         (t (run-defn 'structure (cdr s)))))
  generalizations (anything)
  specializations (set list bag mult-ele-struc o-set no-mult-ele-struc ord-struc
                       un-ord-struc o-pair pair empty-struc non-empty-struc)
  in-domain-of (struc-equal struc-insert struc-delete struc-intersect struc-union
                            struc-difference member memb)
  is-range-of (struc-insert struc-delete struc-intersect struc-union struc-difference)
  interestingness (some (lambda (p)
                          (and (or (has-high-worth p)
                                   (memb p (int-examples 'unary-pred)))
                               (leq-nn (car (rarity p))
                                       0.3)
                               (let ((tempdef (defn (car (domain p)))))
                                 (when (every tempdef u)
                                   (let ((tempdef2 (subset u (lambda (e)
                                                           (run-alg p e)))))
                                   (let ((temp2 (find-if (lambda (p2)
                                                        (and (run-defn (cadr (domain p2)) tempdef2)
                                                             (run-alg p2 u tempdef2)))
                                                      (ok-bin-preds u))))
                                     (cprin1 14 "~%The set of elements of " u
                                       " which satisfy the rare predicate " p
                                       " form a very special subset; namely, there are in relation " temp2
                                       " to the entire structure.~%")
                                     (cprin1 40 "    They are, by the way: " tempdef "~%")))))))
                        (examples 'unary-pred))
  rarity (0 2 2))

(defunit sub-slots
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  inverse (super-slots)
  super-slots (specializations)
  data-type slot
  double-check t)

(defunit subsetp
  worth 500
  isa (math-concept math-op op math-pred pred anything binary-op binary-pred)
  arity 2
  domain (set set)
  range (bit)
  elim-slots (applics)
  specializations (set-equal o-set-equal)
  recursive-alg (lambda (s1 s2)
                  (cond ((null s1)
                         t)
                        (t (and (consp s1)
                                (member (car s1) s2)
                                (run-alg 'subsetp (cdr s1) s2)))))
  fast-alg is-subset-of)

(defunit subsumed-by
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  inverse (subsumes)
  data-type unit
  double-check t)

(defunit subsumes
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  double-check t
  inverse (subsumed-by))

(defunit successor
  worth 500
  isa (math-concept math-op op num-op anything unary-op)
  ;; TODO - this originally was (LAMBDA (X Y) (ADD1 X Y)) for some reason? makes no sense
  fast-alg (lambda (x)
             (1+ x))
  domain (nnumber)
  range (nnumber)
  elim-slots (applics)
  arity 1)

(defunit super-slots
  worth 300
  inverse (sub-slots)
  isa (slot non-criterial-slot repr-concept anything)
  super-slots (generalizations)
  data-type slot
  double-check t)

(defunit task
  worth 500
  format (priority-value unit-name slot-name reasons misc-args)
  isa (repr-concept anything category)
  generalizations (anything))

(defunit the-first-of
  worth 500
  isa (op pred math-op math-pred anything binary-op logic-op binary-pred)
  fast-alg (lambda (x y)
             (declare (ignore y))
             x)
  arity 2
  domain (anything anything)
  range (anything)
  elim-slots (applics)
  specializations (and)
  generalizations (or)
  rarity (1.0 42 0))

(defunit the-second-of
  worth 500
  isa (op pred math-op math-pred anything binary-op logic-op binary-pred)
  fast-alg (lambda (x y)
             (declare (ignore x))
             y)
  arity 2
  domain (anything anything)
  range (anything)
  elim-slots (applics)
  specializations (and)
  generalizations (or))

(defunit then-add-to-agenda
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (then-parts)
  data-type lisp-fn
  failed-record (then-add-to-agenda-failed-record)
  record (then-add-to-agenda-record))

(defunit then-add-to-agenda-failed-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  failed-record-for (then-add-to-agenda)
  dont-copy t)

(defunit then-add-to-agenda-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  record-for (then-add-to-agenda)
  dont-copy t)

(defunit then-compute
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (then-parts)
  data-type lisp-fn
  failed-record (then-compute-failed-record)
  record (then-compute-record))

(defunit then-compute-failed-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  failed-record-for (then-compute)
  dont-copy t)

(defunit then-compute-record
  worth 300
  isa (slot-non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  record-for (then-compute)
  dont-copy t)

(defunit then-conjecture
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (then-parts)
  data-type lisp-fn
  failed-record (then-conjecture-failed-record)
  record (then-conjecture-record))

(defunit then-conjecture-failed-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  failed-record-for (then-conjecture)
  dont-copy t)

(defunit then-conjecture-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  record-for (then-conjecture)
  dont-copy t)

(defunit then-define-new-concepts
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (then-parts)
  data-type lisp-fn
  failed-record (then-define-new-concepts-failed-record)
  record (then-define-new-concepts-record))

(defunit then-define-new-concepts-failed-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  failed-record-for (then-define-new-concepts)
  dont-copy t)

(defunit then-define-new-concepts-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  record-for (then-define-new-concepts)
  dont-copy t)

(defunit then-delete-old-concepts
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (then-parts)
  data-type lisp-fn
  failed-record (then-delete-old-concepts-failed-record)
  record (then-delete-old-concepts-record))

(defunit then-delete-old-concepts-failed-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  failed-record-for (then-delete-old-concepts)
  dont-copy t)

(defunit then-delete-old-concepts-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  record-for (then-delete-old-concepts)
  dont-copy t)

(defunit then-modify-slots
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (then-parts)
  data-type lisp-fn
  failed-record (then-modify-slots-failed-record)
  record (then-modify-slots-record))

(defunit then-modify-slots-failed-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  failed-record-for (then-modify-slots)
  dont-copy t)

(defunit then-modify-slots-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  record-for (then-modify-slots)
  dont-copy t)

(defunit then-parts
  worth 600
  isa (slot criterial-slot repr-concept anything)
  sub-slots (then-compute then-modify-slots then-conjecture then-define-new-concepts
                          then-delete-old-concepts then-add-to-agenda then-print-to-user)
  data-type lisp-fn)

(defunit then-print-to-user
  worth 600
  isa (slot criterial-slot repr-concept anything)
  super-slots (then-parts)
  data-type lisp-fn
  failed-record (then-print-to-user-failed-record)
  record (then-print-to-user-record))

(defunit then-print-to-user-failed-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  failed-record-for (then-print-to-user)
  dont-copy t)

(defunit then-print-to-user-record
  worth 300
  isa (slot non-criterial-slot repr-concept record-slot anything)
  data-type dotted-pair
  record-for (then-print-to-user)
  dont-copy t)

(defunit to-delete
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-fn)

(defunit to-delete-1
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-fn)

(defunit transpose
  worth 300
  isa (slot non-criterial-slot repr-concept anything)
  data-type unit
  double-check t
  inverse (transpose))

(defunit unary-unit-op
  generalizations (unit-op anything)
  worth 500
  isa (repr-concept anything category)
  abbrev ("Operations performable upon a unit")
  specializations (slot))

(defunit undefined
  is-range-of (undefined-pred)
  worth 100
  isa (anything repr-concept))

(defunit undefined-pred
  worth 100
  isa (op pred anything unary-op math-op unary-pred)
  arity 1
  domain (anything)
  range (undefined)
  elim-slots (applics))

(defunit unit
  isa (repr-concept math-concept anything category)
  worth 500
  generalizations (anything repr-concept))

(defunit unit-op
  generalizations (math-concept op math-op set-op anything)
  worth 500
  isa (repr-concept anything category)
  abbrev ("Operations performable upon a set of units")
  specializations (unary-unit-op))

(defunit unitized-alg
  super-slots (alg)
  isa (slot criterial-slot repr-concept anything)
  worth 600
  data-type lisp-fn)

(defunit unitized-defn
  super-slots (defn)
  worth 600
  isa (slot criterial-slot repr-concept anything)
  data-type lisp-pred)

(defunit worth
  worth 305
  isa (slot non-criterial-slot repr-concept anything)
  data-type number)

;; TODO - what are these?  unfinished?
(defunit los1
  worth 100
  isa (math-obj math-concept anything))

(defunit los2
  worth 100
  isa (math-obj math-concept anything))

(defunit los3
  worth 100
  isa (math-obj math-concept anything))

(defunit los4
  worth 100
  isa (math-obj math-concept anything))

(defunit los5
  worth 100
  isa (math-obj math-concept anything))

(defunit los6
  worth 100
  isa (math-obj math-concept anything))

(defunit los7
  worth 100
  isa (math-obj math-concept anything))

(defunit win1
  worth 100
  isa (math-obj math-concept anything))
