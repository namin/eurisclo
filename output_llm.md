# Sample output of running Eurisclo, the Common Lisp port of Doug Lenat's EURISKO

Configured LLM: OLLAMA with model qwen2.5:14b

## Statistics on Success of Heuristics

Output of `(print-run-info)`:

```
H24-INTERP3 -> 0% (48 tries, 0 successes)
H24-INTERP2 -> 0% (1 tries, 0 successes)
H23-WORK-ON-TASK -> 5% (20 tries, 1 successes)
H10-WORK-ON-TASK -> 8% (13 tries, 1 successes)
H9-WORK-ON-TASK -> 10% (231 tries, 24 successes)
H-LLM-IMPLEMENT-CONCEPT-INTERP2 -> 23% (44 tries, 10 successes)
H15-WORK-ON-TASK -> 23% (13 tries, 3 successes)
H18-WORK-ON-TASK -> 42% (33 tries, 14 successes)
H11-WORK-ON-TASK -> 60% (65 tries, 39 successes)
H8-WORK-ON-TASK -> 60% (53 tries, 32 successes)
*-WORK-ON-UNIT -> 69% (252 tries, 173 successes)
H-LLM-ANALOGIZE-INTERP2 -> 89% (174 tries, 155 successes)
H7-INTERP2 -> 100% (160 tries, 160 successes)
H26-INTERP2 -> 100% (23 tries, 23 successes)
H25-INTERP2 -> 100% (23 tries, 23 successes)
H22-WORK-ON-TASK -> 100% (20 tries, 20 successes)
H28-INTERP2 -> 100% (17 tries, 17 successes)
H27-INTERP2 -> 100% (17 tries, 17 successes)
H19-CRITERIAL-WORK-ON-TASK -> 100% (14 tries, 14 successes)
H4-WORK-ON-TASK -> 100% (14 tries, 14 successes)
H17-WORK-ON-TASK -> 100% (5 tries, 5 successes)
H16-INTERP2 -> 100% (5 tries, 5 successes)
H-AVOID-IF-WORKING-WORK-ON-TASK -> 100% (1 tries, 1 successes)
Tasks: 956
T
```

## LLM Heuristic Statistics

```
Total LLM calls: 174
Successful parses: 173
Failed parses: 0
Concepts created: 319
```

## Sample Output of Random Tasks

### Sample

```
Task 391: Focusing on CURRYING
H7 applies.
        the THEN-ADD-TO-AGENDA slot of H7 has been applied successfully to CURRYING

Since CURRYING has no known APPLICS, it is probably worth looking for some.
        the THEN-PRINT-TO-USER slot of H7 has been applied successfully to CURRYING
Heuristic H7 achieved success!

  All the ThenParts of H7 (Instantiate a concept having no known instances) have been successfully executed.
H-LLM-ANALOGIZE applies.
Prompt: Find 2-3 mathematical or computational concepts analogous to CURRYING which has properties: Domain: NIL, Range: NIL, Arity: NIL, Type: MATH-CONCEPT

IMPORTANT: You must respond ONLY in the following format. Do not include any explanations or additional text outside this format:

CONCEPT-NAME: description in one line
CONCEPT-NAME: description in one line
CONCEPT-NAME: description in one line

Begin your response immediately with the formatted output:
Response: FUNCTIONAL-COMPOSITION: combining two functions where the output of one is the input of another
PARTIAL-APPLICATION: fixing some arguments of a function to produce a new function
HIGHER-ORDER-FUNCTIONS: functions that can take other functions as arguments or return them as resultsParsed 3 analogies from LLM response
        the THEN-COMPUTE slot of H-LLM-ANALOGIZE has been applied successfully to CURRYING
Created analog concept: FUNCTIONAL-COMPOSITION
Created analog concept: PARTIAL-APPLICATION
Created analog concept: HIGHER-ORDER-FUNCTIONS
H-LLM-ANALOGIZE created 3 new concepts
        the THEN-DEFINE-NEW-CONCEPTS slot of H-LLM-ANALOGIZE has been applied successfully to CURRYING
        the THEN-ADD-TO-AGENDA slot of H-LLM-ANALOGIZE has been applied successfully to CURRYING
Heuristic H-LLM-ANALOGIZE achieved success!

  All the ThenParts of H-LLM-ANALOGIZE Find analogies to interesting concepts via LLM have been successfully executed.
H-LLM-IMPLEMENT-CONCEPT applies.
Prompt: Write a lambda expression for CURRYING (transforming a function that takes multiple arguments into a sequence of functions each taking a single argument).
Use only: cons, car, cdr, list, null, equal, cond, if, +, -, *, <, >
Example: (lambda (x y) (cons x y))
To call another Eurisko operation, use: (run-alg 'operation-name args)
Example: (lambda (x) (run-alg 'square x))
Write only the lambda, no explanation:
Response: (lambda (f)
  (lambda (x)
    (lambda (y)
      ((f x) y))))LLM response: (lambda (f)
  (lambda (x)
    (lambda (y)
      ((f x) y))))
Successfully parsed LLM code: (LAMBDA (F) (LAMBDA (X) (LAMBDA (Y) ((F X) Y))))
        the THEN-COMPUTE slot of H-LLM-IMPLEMENT-CONCEPT has been applied successfully to CURRYING
Installing code for CURRYING
; in: LAMBDA (F)
;     ((EURISCLO::F EURISCLO::X) EURISCLO::Y)
; 
; caught ERROR:
;   illegal function call

;     (LAMBDA (EURISCLO::F)
;       (LAMBDA (EURISCLO::X)
;         (LAMBDA (EURISCLO::Y) ((EURISCLO::F EURISCLO::X) EURISCLO::Y))))
; 
; caught STYLE-WARNING:
;   The variable F is defined but never used.

;     (LAMBDA (EURISCLO::X)
;       (LAMBDA (EURISCLO::Y) ((EURISCLO::F EURISCLO::X) EURISCLO::Y)))
; 
; caught STYLE-WARNING:
;   The variable X is defined but never used.

;     (LAMBDA (EURISCLO::Y) ((EURISCLO::F EURISCLO::X) EURISCLO::Y))
; 
; caught STYLE-WARNING:
;   The variable Y is defined but never used.
; 
; compilation unit finished
;   caught 1 ERROR condition
;   caught 3 STYLE-WARNING conditions
Inferred signature for CURRYING: domain=(ANYTHING), range=(ANYTHING), arity=1
Successfully implemented CURRYING
        the THEN-MODIFY-SLOTS slot of H-LLM-IMPLEMENT-CONCEPT has been applied successfully to CURRYING
        the THEN-ADD-TO-AGENDA slot of H-LLM-IMPLEMENT-CONCEPT has been applied successfully to CURRYING
Heuristic H-LLM-IMPLEMENT-CONCEPT achieved success!

  All the ThenParts of H-LLM-IMPLEMENT-CONCEPT LLM provides executable code for hollow concepts have been successfully executed.
Heuristic * achieved success!
```

### Sample

```
Task 392
  The IF-WORKING-ON-TASK slot of heuristic H8 (APPLICS (U) MAY BE FOUND AGAINST
                                                       APPLICS (GENL (U))) applies to the current task.
zzzzzzzzzzzzzzzzzzzH8 Grumble...   this rule is taking too much space!  On to less expansive rules!

        the THEN-COMPUTE slot of H8 has been applied successfully to (790
                                                                      CURRYING
                                                                      APPLICS
                                                                      ((To properly study
                                                                        CURRYING
                                                                         we must gather empirical data about instances of that concept)
                                                                       (Now that
                                                                        CURRYING
                                                                        has code, find applications))
                                                                      ((CREDIT-TO
                                                                        H7)))

Instantiated CURRYING; found 582 APPLICS
        the THEN-PRINT-TO-USER slot of H8 has been applied successfully to (790
                                                                            CURRYING
                                                                            APPLICS
                                                                            ((To properly study
                                                                              CURRYING
                                                                               we must gather empirical data about instances of that concept)
                                                                             (Now that
                                                                              CURRYING
                                                                              has code, find applications))
                                                                            ((CREDIT-TO
                                                                              H7)))
Heuristic H8 achieved success!
  The IF-WORKING-ON-TASK slot of heuristic H11 Applics (f) may be found by running Alg (f) on members of u's Domain) applies to the current task.
Finding applic for: CURRYING
        the THEN-COMPUTE slot of H11 has been applied successfully to (790
                                                                       CURRYING
                                                                       APPLICS
                                                                       ((To properly study
                                                                         CURRYING
                                                                          we must gather empirical data about instances of that concept)
                                                                        (Now that
                                                                         CURRYING
                                                                         has code, find applications))
                                                                       ((CREDIT-TO
                                                                         H7)))

Instantiated CURRYING; found 36 APPLICS
        the THEN-PRINT-TO-USER slot of H11 has been applied successfully to (790
                                                                             CURRYING
                                                                             APPLICS
                                                                             ((To properly study
                                                                               CURRYING
                                                                                we must gather empirical data about instances of that concept)
                                                                              (Now that
                                                                               CURRYING
                                                                               has code, find applications))
                                                                             ((CREDIT-TO
                                                                               H7)))
Heuristic H11 achieved success!
for CURRYING interestingness looked-thru: (CURRYING); 0 results total.
 The new units were: NIL
```
