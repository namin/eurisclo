# Sample output of running Eurisclo, the Common Lisp port of Doug Lenat's EURISKO

## Statistics on Success of Heuristics

Output of `(print-run-info)`:

```
H10-WORK-ON-TASK -> 7% (94 tries)
H15-WORK-ON-TASK -> 11% (94 tries)
H9-WORK-ON-TASK -> 26% (1301 tries)
H6-WORK-ON-TASK -> 40% (150 tries)
H23-WORK-ON-TASK -> 48% (676 tries)
*-WORK-ON-UNIT -> 51% (4660 tries)
H18-WORK-ON-TASK -> 51% (1087 tries)
H11-WORK-ON-TASK -> 51% (808 tries)
H8-WORK-ON-TASK -> 52% (590 tries)
H7-INTERP2 -> 100% (2008 tries)
H22-WORK-ON-TASK -> 100% (676 tries)
H4-WORK-ON-TASK -> 100% (616 tries)
H19-CRITERIAL-WORK-ON-TASK -> 100% (589 tries)
H26-INTERP2 -> 100% (378 tries)
H25-INTERP2 -> 100% (378 tries)
H24-INTERP2 -> 100% (291 tries)
H17-WORK-ON-TASK -> 100% (190 tries)
H16-INTERP2 -> 100% (190 tries)
H28-INTERP2 -> 100% (179 tries)
H27-INTERP2 -> 100% (179 tries)
H5-GOOD-WORK-ON-TASK -> 100% (31 tries)
H5-CRITERIAL-WORK-ON-TASK -> 100% (31 tries)
H1-INTERP2 -> 100% (31 tries)
H-AVOID-IF-WORKING-WORK-ON-TASK -> 100% (1 tries)
Tasks: 8984
```

## Sample Output of a Random Task

```
Task 106: Working on the promising task (700 RESTRICT-REPEAT2 APPLICS
                                         ((To properly study  RESTRICT-REPEAT2
                                            we must gather empirical data about instances of that concept))
                                         ((CREDIT-TO H7)))
  The IF-WORKING-ON-TASK slot of heuristic H8 (APPLICS (U) MAY BE FOUND AGAINST
                                                       APPLICS (GENL (U))) applies to the current task.
        the THEN-COMPUTE slot of H8 was applied to (700 RESTRICT-REPEAT2
                                                    APPLICS
                                                    ((To properly study
                                                      RESTRICT-REPEAT2
                                                       we must gather empirical data about instances of that concept))
                                                    ((CREDIT-TO H7))), but for some reason it signalled a failure.
  The IF-WORKING-ON-TASK slot of heuristic H11 Applics (f) may be found by running Alg (f) on members of u's Domain) applies to the current task.
Finding applic for: RESTRICT-REPEAT2
args: (MULT-ELE-STRUC O-SET ALWAYS-NIL-O-PARALLEL-JOIN-2)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (O-PAIR EMPTY-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-UN-ORD-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (BAG O-PAIR IEQP-O-ADD)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (LIST O-SET
            REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-UN-ORD-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (O-SET SET REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-PAIR-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (O-SET O-SET IEQP-O-ADD)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (LIST UN-ORD-STRUC
            REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-O-PAIR-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (NON-EMPTY-STRUC BAG
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-O-PAIR-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (O-PAIR PAIR
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-NON-EMPTY-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (MULT-ELE-STRUC SET
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-O-PAIR-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (ORD-STRUC EMPTY-STRUC THE-FIRST-OF-O-PROJ1)
known-applic: NIL
compilation of (LAMBDA (S S2)
                 (LET ((V (CAR S)))
                   (MAPC
                    (LAMBDA (E)
                      (SETF V (RUN-ALG 'THE-FIRST-OF-O-PROJ1 V S2 E)))
                    (CDR S))
                   V))
+Finding applic for: RESTRICT-REPEAT2
args: (PAIR MULT-ELE-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-UN-ORD-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (MULT-ELE-STRUC EMPTY-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (O-SET MULT-ELE-STRUC ALWAYS-NIL-O-PARALLEL-JOIN-2)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (O-SET NON-EMPTY-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-UN-ORD-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (MULT-ELE-STRUC O-PAIR RESTRICT-IEQP-O-ADD)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (BAG NO-MULT-ELE-STRUC ALWAYS-NIL-O-PARALLEL-JOIN-2)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (NON-EMPTY-STRUC ORD-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (O-PAIR O-SET REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-PAIR-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (PAIR EMPTY-STRUC THE-FIRST-OF-O-PROJ1)
known-applic: NIL
compilation of (LAMBDA (S S2)
                 (LET ((V (CAR S)))
                   (MAPC
                    (LAMBDA (E)
                      (SETF V (RUN-ALG 'THE-FIRST-OF-O-PROJ1 V S2 E)))
                    (CDR S))
                   V))
+Finding applic for: RESTRICT-REPEAT2
args: (PAIR EMPTY-STRUC REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-PAIR-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (ORD-STRUC PAIR RESTRICT-IEQP-O-ADD)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (BAG PAIR IEQP-O-ADD)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (MULT-ELE-STRUC EMPTY-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-NON-EMPTY-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (UN-ORD-STRUC NO-MULT-ELE-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (MULT-ELE-STRUC O-SET THE-FIRST-OF-O-PROJ1)
known-applic: NIL
compilation of (LAMBDA (S S2)
                 (LET ((V (CAR S)))
                   (MAPC
                    (LAMBDA (E)
                      (SETF V (RUN-ALG 'THE-FIRST-OF-O-PROJ1 V S2 E)))
                    (CDR S))
                   V))
+Finding applic for: RESTRICT-REPEAT2
args: (O-PAIR O-SET
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-UN-ORD-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (ORD-STRUC NON-EMPTY-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-O-SET-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (MULT-ELE-STRUC NO-MULT-ELE-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-O-PAIR-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (NON-EMPTY-STRUC MULT-ELE-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM193)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (MULT-ELE-STRUC MULT-ELE-STRUC IEQP-O-ADD)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (EMPTY-STRUC SET REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-O-PAIR-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (EMPTY-STRUC EMPTY-STRUC IEQP-O-ADD)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (NO-MULT-ELE-STRUC O-PAIR IEQP-O-ADD)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (EMPTY-STRUC SET
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (MULT-ELE-STRUC BAG
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-O-SET-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (EMPTY-STRUC EMPTY-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-NON-EMPTY-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (EMPTY-STRUC O-SET RESTRICT-IEQP-O-ADD)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (BAG ORD-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-NON-EMPTY-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (NO-MULT-ELE-STRUC ORD-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-UN-ORD-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (O-SET PAIR
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM193)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (ORD-STRUC PAIR
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM193)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (UN-ORD-STRUC NON-EMPTY-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-O-SET-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (EMPTY-STRUC NON-EMPTY-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (ORD-STRUC BAG IEQP-O-ADD)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (O-SET NO-MULT-ELE-STRUC ALWAYS-NIL-O-PARALLEL-JOIN-2)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (NON-EMPTY-STRUC EMPTY-STRUC
       REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-PAIR-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (O-SET O-SET ALWAYS-NIL-O-PARALLEL-JOIN-2)
known-applic: NIL
-Finding applic for: RESTRICT-REPEAT2
args: (O-PAIR PAIR REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-O-SET-AS-PARAM)
known-applic: NIL
Finding applic for: RESTRICT-REPEAT2
args: (SET LIST REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-UN-ORD-STRUC-AS-PARAM)
known-applic: NIL
        the THEN-COMPUTE slot of H11 has been applied successfully to (700
                                                                       RESTRICT-REPEAT2
                                                                       APPLICS
                                                                       ((To properly study
                                                                         RESTRICT-REPEAT2
                                                                          we must gather empirical data about instances of that concept))
                                                                       ((CREDIT-TO
                                                                         H7)))

Instantiated RESTRICT-REPEAT2; found 3 APPLICS
    Namely: (((ORD-STRUC EMPTY-STRUC THE-FIRST-OF-O-PROJ1)
              (REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM))
             ((PAIR EMPTY-STRUC THE-FIRST-OF-O-PROJ1)
              (REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM193))
             ((MULT-ELE-STRUC O-SET THE-FIRST-OF-O-PROJ1)
              (REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-O-SET-AS-PARAM)))
        the THEN-PRINT-TO-USER slot of H11 has been applied successfully to (700
                                                                             RESTRICT-REPEAT2
                                                                             APPLICS
                                                                             ((To properly study
                                                                               RESTRICT-REPEAT2
                                                                                we must gather empirical data about instances of that concept))
                                                                             ((CREDIT-TO
                                                                               H7)))
       The Then Parts of the rule have been executed.

 The results of this task were: ((NEW-VALUES
                                  (RESTRICT-REPEAT2 APPLICS
                                   (((ORD-STRUC EMPTY-STRUC
                                      THE-FIRST-OF-O-PROJ1)
                                     (REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM))
                                    ((PAIR EMPTY-STRUC THE-FIRST-OF-O-PROJ1)
                                     (REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-EMPTY-STRUC-AS-PARAM193))
                                    ((MULT-ELE-STRUC O-SET
                                      THE-FIRST-OF-O-PROJ1)
                                     (REPEAT2-THE-FIRST-OF-O-PROJ1-ON-S-WITH-A-O-SET-AS-PARAM)))
                                   (By running algorithm for RESTRICT-REPEAT2
                                    on random examples from
                                    (TYPE-OF-STRUCTURE TYPE-OF-STRUCTURE
                                     TERTIARY-PRED)
                                    , 3 were found))))

```
