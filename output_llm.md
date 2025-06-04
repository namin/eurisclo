# Sample output of running Eurisclo, the Common Lisp port of Doug Lenat's EURISKO

Configured LLM: OLLAMA with model qwen2.5:14b

## Statistics on Success of Heuristics

Output of `(print-run-info)`:

```
H18-WORK-ON-TASK -> 41% (32 tries, 13 successes)
H9-WORK-ON-TASK -> 50% (4 tries, 2 successes)
H15-WORK-ON-TASK -> 50% (2 tries, 1 successes)
H10-WORK-ON-TASK -> 50% (2 tries, 1 successes)
H8-WORK-ON-TASK -> 67% (27 tries, 18 successes)
H11-WORK-ON-TASK -> 79% (29 tries, 23 successes)
*-WORK-ON-UNIT -> 89% (53 tries, 47 successes)
H7-INTERP2 -> 100% (39 tries, 39 successes)
H31-LLM-SLOT-EVOLVER-WORK-ON-TASK -> 100% (37 tries, 37 successes)
H19-CRITERIAL-WORK-ON-TASK -> 100% (13 tries, 13 successes)
H4-WORK-ON-TASK -> 100% (13 tries, 13 successes)
H30-LLM-CONCEPT-GENERATOR-INTERP2 -> 100% (10 tries, 10 successes)
H28-INTERP2 -> 100% (8 tries, 8 successes)
H27-INTERP2 -> 100% (8 tries, 8 successes)
H26-INTERP2 -> 100% (8 tries, 8 successes)
H25-INTERP2 -> 100% (8 tries, 8 successes)
H17-WORK-ON-TASK -> 100% (5 tries, 5 successes)
H16-INTERP2 -> 100% (5 tries, 5 successes)
Tasks: 131
T
```

## LLM-Generated Insights

```
Concepts: (E:** TENSORIAL HOMOMORPHIC ENCRYPTION HYPEREDGE-PARTITIONING
           E:** ALGEBRAIC KNOT INVARIANT E:** HYPERGROUPOID STRUCTURE
           E:** LATTICE OF IDEALS E:** MULTISET PARTITION ALGEBRA
           E:** LATTICE-BASED ENCRYPTED HASH (LBESH) E:** TETRADIC LATTICE
           E:** VECTOR SPACE OVER FINITE FIELDS TENSOR DECOMPOSITION THEORY)
Slots: ((H6 DOMAIN (ALGEBRA)) (H6 DOMAIN (ALGEBRA)) (H6 DOMAIN (ALGEBRA))
        (H6 DOMAIN (ALGEBRA)) (H6 DOMAIN (ALGEBRA)) (H6 DOMAIN (ALGEBRA))
        (H14 PREVENTION_HEURISTIC "ENTITY\_FILTER")
        (H14 ABSTRACTION PREVENTION-HEURISTIC)
        (H14 GENERALIZATION PREVENTION-CRITERIA)
        (H14 GENERALIZATION PREVENTION-HEURISTIC)
        (H14 GENERALIZATION PREVENTION_HEURISTIC)
        (H14 GENERALIZATION PREVENTION_HEURISTIC)
        (H14 GENERALIZATION PREVENTION-HEURISTIC)
        (H13 GENERALIZATION PREVENTIVE_HEURISTICS)
        (H13 GENERALIZATION PREVENTIVE-HEURISTIC)
        (H13 ABSTRACTION PREVENTION-HEURISTIC)
        (H13 ABSTRACTION PREVENTION-HEURISTIC)
        (H13 ABSTRACTION PREVENTION-HEURISTIC)
        (H13 GENERALIZATION PREVENTION-HEURISTIC)
        (H13 GENERALIZATION PREVENTIVE_HEURISTIC)
        (H13 ABSTRACTION PREVENTION-HEURISTIC)
        (H13 GENERALIZATION AVOIDANCE-HEURISTIC)
        (H13 GENERALIZATION PREVENTION-CRITERIA)
        (H12 GENERALIZATION PREVENTION-CRITERIA) (H12 PREVENTION_THRESHOLD 0)
        (H12 WORTH 400) (H12 GENERALIZATION PREVENTION-CRITERIA)
        (H12 GENERALIZATION PREVENTION-HEURISTIC)
        (H12 GENERALIZATION PREVENTION-CRITERIA) (H12 PREVENTION-COST C')
        (H12 GENERALIZATION PREVENTION-HEURISTIC) (H1 THRESHOLD 0)
        (H1 THRESHOLD 0) (H1 THRESHOLD 0) (H1 THRESHOLD 0) (H1 THRESHOLD 0)
        (H1 THRESHOLD 0))
Rules: NIL
```

## Sample Output of a Random Task

```
Task 104: Focusing on H14
H16 applies.
        the THEN-CONJECTURE slot of H16 has been applied successfully to H14
        the THEN-ADD-TO-AGENDA slot of H16 has been applied successfully to H14

CONJEC304:
Since some applications of H14 (i.e., Form a rule that would have prevented this mistake) are very valuable, so EURISKO wants to find new concepts which are slightly more generalized than H14, and (to that end) has added a new task to the agenda to find such concepts.         the THEN-PRINT-TO-USER slot of H16 has been applied successfully to H14
Heuristic H16 achieved success!

  All the ThenParts of H16 Generalize a sometimes-useful action have been successfully executed.
H30-LLM-CONCEPT-GENERATOR applies.
H30: Analyzing successful patterns to generate new concepts
Creating LLM-generated concept: TENSOR DECOMPOSITION THEORY
LLM generated new concept: TENSOR DECOMPOSITION THEORY
        the THEN-COMPUTE slot of H30-LLM-CONCEPT-GENERATOR has been applied successfully to H14
Heuristic H30-LLM-CONCEPT-GENERATOR achieved success!

  All the ThenParts of H30-LLM-CONCEPT-GENERATOR LLM-guided concept generation from patterns have been successfully executed.
Heuristic * achieved success!
```
