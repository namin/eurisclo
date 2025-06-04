# Sample output of running Eurisclo, the Common Lisp port of Doug Lenat's EURISKO

Configured LLM: OLLAMA with model qwen2.5:14b

## Statistics on Success of Heuristics

Output of `(print-run-info)`:

```
H24-INTERP3 -> 0% (47 tries, 0 successes)
H24-INTERP2 -> 0% (1 tries, 0 successes)
H9-WORK-ON-TASK -> 10% (197 tries, 20 successes)
H23-WORK-ON-TASK -> 11% (19 tries, 2 successes)
H10-WORK-ON-TASK -> 25% (16 tries, 4 successes)
H15-WORK-ON-TASK -> 31% (16 tries, 5 successes)
H18-WORK-ON-TASK -> 42% (33 tries, 14 successes)
H11-WORK-ON-TASK -> 55% (56 tries, 31 successes)
H8-WORK-ON-TASK -> 55% (47 tries, 26 successes)
*-WORK-ON-UNIT -> 73% (268 tries, 195 successes)
H-LLM-ANALOGIZE-INTERP2 -> 92% (198 tries, 183 successes)
H7-INTERP2 -> 100% (184 tries, 184 successes)
H26-INTERP2 -> 100% (20 tries, 20 successes)
H25-INTERP2 -> 100% (20 tries, 20 successes)
H22-WORK-ON-TASK -> 100% (19 tries, 19 successes)
H28-INTERP2 -> 100% (16 tries, 16 successes)
H27-INTERP2 -> 100% (16 tries, 16 successes)
H19-CRITERIAL-WORK-ON-TASK -> 100% (14 tries, 14 successes)
H4-WORK-ON-TASK -> 100% (14 tries, 14 successes)
H17-WORK-ON-TASK -> 100% (5 tries, 5 successes)
H16-INTERP2 -> 100% (5 tries, 5 successes)
H-AVOID-IF-WORKING-WORK-ON-TASK -> 100% (1 tries, 1 successes)
Tasks: 1083
T
```

## LLM Heuristic Statistics

```
Total LLM calls: 235
Successful parses: 234
Failed parses: 0
Concepts created: 462
```

## Sample Output of Random Tasks

### H-LLM-ANALOGIZE

```
Task 139: Focusing on CURRYING
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
PARTIAL-APPLICATION: fixing some arguments of a function to create a new function with fewer parameters
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
Heuristic * achieved success!
```
