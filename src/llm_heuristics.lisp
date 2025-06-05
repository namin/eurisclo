(in-package "EURISCLO")

;;; LLM-Enhanced Heuristics for Eurisko
;;; These heuristics use Large Language Models to suggest new mathematical concepts,
;;; patterns, and conjectures that Eurisko can then explore systematically.

;;; Global variables for heuristic communication
(defvar *suggested-concepts* nil)
(defvar *pattern-predicates* nil)
(defvar *llm-conjectures* nil)
(defvar *llm-specializations* nil)

;;; Global statistics for debugging LLM heuristics
(defparameter *llm-heuristic-stats* 
  '(:total-calls 0 
    :successful-parses 0 
    :failed-parses 0
    :concepts-created 0
    :parse-failures nil))

;;; Utility functions

(defun llm-query-formatted (prompt format-instructions)
  "Query LLM with explicit format instructions"
  (let ((full-prompt (format nil "~A

IMPORTANT: You must respond ONLY in the following format. Do not include any explanations or additional text outside this format:

~A

Begin your response immediately with the formatted output:" 
                            prompt format-instructions)))
    (incf (getf *llm-heuristic-stats* :total-calls))
    (llm-query full-prompt)))

(defun log-parse-failure (heuristic-name input reason)
  "Log parsing failures for debugging"
  (push (list :heuristic heuristic-name 
              :timestamp (get-universal-time)
              :reason reason
              :input-snippet (subseq input 0 (min 200 (length input))))
        (getf *llm-heuristic-stats* :parse-failures))
  (cprin1 20 "Parse failure in " heuristic-name ": " reason "~%"))

(defun split-string (string delimiter)
  "Split string by delimiter character"
  (let ((parts nil)
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) delimiter)
          do (push (subseq string start i) parts)
             (setf start (1+ i)))
    (push (subseq string start) parts)
    (reverse parts)))

(defun starts-with-p (string prefix)
  "Check if string starts with prefix"
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))

(defun take (n list)
  "Take first n elements of list"
  (loop for item in list
        for i from 0 below n
        collect item))

(defun describe-concept-properties (unit)
  "Generate a concise description of a unit's key properties"
  (format nil "Domain: ~A, Range: ~A, Arity: ~A, Type: ~A" 
          (domain unit)
          (range unit) 
          (arity unit)
          (car (isa unit))))

(defun get-key-properties (unit)
  "Extract key properties for LLM context"
  (format nil "~{~A~^, ~}"
          (remove nil
                  (list (when (domain unit) 
                          (format nil "domain: ~A" (domain unit)))
                        (when (worth unit)
                          (format nil "worth: ~A" (worth unit)))
                        (when (arity unit)
                          (format nil "arity: ~A" (arity unit)))))))

(defun find-related-high-worth-concepts (unit)
  "Find concepts related to unit with high worth"
  (remove-duplicates
   (append (generalizations unit)
           (specializations unit)
           (remove-if-not (lambda (u) (> (worth u) 600))
                         (examples (car (isa unit)))))))

(defun clean-concept-name (name)
  "Convert a natural language concept name into a valid Eurisko unit name"
  (let* ((cleaned (remove-if (lambda (c) 
                              (member c '(#\* #\` #\( #\) #\[ #\])))
                            name))
         (words (split-string cleaned #\Space))
         (final-name (format nil "~{~A~^-~}" 
                            (mapcar #'string-upcase 
                                   (remove-if (lambda (w) (< (length w) 2))
                                             words)))))
    (when (and (> (length final-name) 2)
               (not (every #'digit-char-p final-name)))
      (intern final-name))))

;;; Parsing functions for analogies

(defun parse-llm-analogies (response)
  "Parse LLM response for analog concepts"
  (let ((analogies nil))
    ;; First try structured format
    (dolist (line (split-string response #\Newline))
      (when (and (> (length line) 3)
                 (find #\: line))
        (let* ((colon-pos (position #\: line))
               (name-part (string-trim " " (subseq line 0 colon-pos)))
               (desc-part (string-trim " " (subseq line (1+ colon-pos)))))
          ;; Skip numbered items like "1. Function Composition:"
          (when (and (not (find #\. name-part))
                     (not (digit-char-p (char name-part 0))))
            (let ((cleaned-name (clean-concept-name name-part)))
              (when (and cleaned-name 
                         (> (length desc-part) 10)
                         (not (search "**" name-part))) ; Skip markdown
                (push (list :name cleaned-name 
                           :description desc-part
                           :original-name name-part)
                      analogies)))))))
    
    ;; If structured parsing failed, try to extract from numbered list
    (when (null analogies)
      (let ((lines (split-string response #\Newline)))
        (dolist (line lines)
          ;; Look for patterns like "1. **Name**:" or "1. Name:"
          (when (and (> (length line) 5)
                     (digit-char-p (char line 0))
                     (char= (char line 1) #\.))
            (let* ((content (string-trim " " (subseq line 2)))
                   (name-end (or (position #\: content)
                                (position #\- content)))
                   (name-part (when name-end
                               (string-trim " *" (subseq content 0 name-end)))))
              (when (and name-part (> (length name-part) 2))
                (push (list :name (clean-concept-name name-part)
                           :description (if name-end
                                          (string-trim " " (subseq content (1+ name-end)))
                                          "LLM-suggested concept")
                           :original-name name-part)
                      analogies)))))))
    (reverse analogies)))

(defun analog-name (analog-spec)
  (getf analog-spec :name))

(defun analog-description (analog-spec)
  (getf analog-spec :description))

;;; Parsing functions for patterns

(defun parse-llm-predicates (response)
  "Parse pattern specifications from LLM response"
  (let ((predicates nil)
        (current-pattern nil))
    (dolist (line (split-string response #\Newline))
      (cond 
        ((starts-with-p line "PATTERN:")
         (when current-pattern
           (push current-pattern predicates))
         (setf current-pattern 
               (list :name (clean-concept-name (subseq line 8))
                     :test nil
                     :property nil)))
        ((starts-with-p line "TEST:")
         (when current-pattern
           (setf (getf current-pattern :test) 
                 (string-trim " " (subseq line 5)))))
        ((starts-with-p line "PROPERTY:")
         (when current-pattern
           (setf (getf current-pattern :property) 
                 (string-trim " " (subseq line 9)))))))
    (when current-pattern
      (push current-pattern predicates))
    (reverse predicates)))

(defun compile-pattern-test (pred-spec)
  "Generate a test function from a pattern specification"
  (let ((test-desc (getf pred-spec :test))
        (property (getf pred-spec :property)))
    ;; For simple patterns, generate basic test functions
    (cond
      ((search "palindrom" test-desc)
       (lambda (x) 
         (and (numberp x)
              (let ((s (princ-to-string x)))
                (string= s (reverse s))))))
      ((search "even" test-desc)
       (lambda (x) 
         (and (numberp x) (evenp x))))
      ((search "prime" test-desc)
       (lambda (x)
         (and (numberp x)
              (> x 1)
              (loop for i from 2 to (isqrt x)
                    never (zerop (mod x i))))))
      ((search "perfect square" test-desc)
       (lambda (x)
         (and (numberp x)
              (= x (expt (isqrt x) 2)))))
      ;; Default: create a placeholder that always returns nil
      (t 
       (lambda (x)
         (cprin1 50 "LLM pattern test for " property " on " x "~%")
         nil)))))

(defun pred-spec-description (pred-spec)
  (format nil "Tests for ~A: ~A" 
          (getf pred-spec :property)
          (getf pred-spec :test)))

;;; Parsing functions for conjectures

(defun describe-concept-network (unit related-units)
  "Describe a concept and its relationships for conjecture generation"
  (format nil "~A (worth ~A) with properties: ~A. Related concepts: ~{~A~^, ~}"
          unit
          (worth unit)
          (get-key-properties unit)
          (mapcar (lambda (u) (format nil "~A(~A)" u (car (isa u))))
                  related-units)))

(defun parse-llm-conjectures (response)
  "Parse structured conjectures from LLM response"
  (let ((conjectures nil)
        (current-conj nil))
    (dolist (line (split-string response #\Newline))
      (cond
        ((starts-with-p line "CONJECTURE:")
         (when current-conj
           (push current-conj conjectures))
         (setf current-conj 
               (list :name (clean-concept-name (subseq line 11))
                     :statement nil
                     :concepts nil)))
        ((starts-with-p line "STATEMENT:")
         (when current-conj
           (setf (getf current-conj :statement)
                 (string-trim " " (subseq line 10)))))
        ((starts-with-p line "INVOLVES:")
         (when current-conj
           (setf (getf current-conj :concepts)
                 (parse-concept-list (subseq line 9)))))))
    (when current-conj
      (push current-conj conjectures))
    (reverse conjectures)))

(defun parse-concept-list (concept-string)
  "Parse comma-separated concept names"
  (mapcar (lambda (s) 
            (clean-concept-name (string-trim " " s)))
          (split-string concept-string #\,)))

(defun conj-spec-text (conj-spec)
  (getf conj-spec :statement))

(defun conj-spec-concepts (conj-spec)
  (getf conj-spec :concepts))

;;; Parsing functions for specializations

(defun parse-specialization-suggestions (response)
  "Parse specialization suggestions"
  (let ((suggestions nil)
        (current-sugg nil))
    (dolist (line (split-string response #\Newline))
      (cond
        ((starts-with-p line "SPECIALIZE:")
         (when current-sugg
           (push current-sugg suggestions))
         (setf current-sugg
               (list :slot (intern (string-upcase 
                                   (string-trim " " (subseq line 11))))
                     :constraint nil
                     :rationale nil)))
        ((starts-with-p line "CONSTRAINT:")
         (when current-sugg
           (setf (getf current-sugg :constraint)
                 (string-trim " " (subseq line 11)))))
        ((starts-with-p line "RATIONALE:")
         (when current-sugg
           (setf (getf current-sugg :rationale)
                 (string-trim " " (subseq line 10)))))))
    (when current-sugg
      (push current-sugg suggestions))
    (reverse suggestions)))

(defun spec-slot (spec)
  (getf spec :slot))

(defun spec-modification (spec)
  (getf spec :constraint))

;;; Statistics functions

(defun show-llm-stats ()
  "Display LLM heuristic statistics"
  (format t "~%=== LLM Heuristic Statistics ===~%")
  (format t "Total LLM calls: ~A~%" (getf *llm-heuristic-stats* :total-calls))
  (format t "Successful parses: ~A~%" (getf *llm-heuristic-stats* :successful-parses))
  (format t "Failed parses: ~A~%" (getf *llm-heuristic-stats* :failed-parses))
  (format t "Concepts created: ~A~%" (getf *llm-heuristic-stats* :concepts-created))
  (when (> (getf *llm-heuristic-stats* :failed-parses) 0)
    (format t "~%Recent parse failures:~%")
    (dolist (failure (subseq (getf *llm-heuristic-stats* :parse-failures) 
                            0 (min 5 (length (getf *llm-heuristic-stats* :parse-failures)))))
      (format t "  ~A: ~A~%" 
              (getf failure :heuristic)
              (getf failure :reason)))))

(defun reset-llm-stats ()
  "Reset statistics"
  (setf *llm-heuristic-stats* 
        '(:total-calls 0 
          :successful-parses 0 
          :failed-parses 0
          :concepts-created 0
          :parse-failures nil)))

;;; LLM Heuristics

(defheuristic h-llm-analogize
  isa (heuristic op anything)
  english "IF a concept has interesting properties, THEN ask LLM for analogous concepts from mathematics/CS that might share similar structure"
  if-potentially-relevant (lambda (f)
                          (and (has-high-worth f)
                               (or (memb 'math-concept (isa f))
                                   (memb 'op (isa f)))))
  worth 650
  abbrev "Find analogies to interesting concepts via LLM"
  then-compute (lambda (f)
                (let* ((prompt (format nil "Find 2-3 mathematical or computational concepts analogous to ~A which has properties: ~A"
                                     f
                                     (describe-concept-properties f)))
                       (format-spec "CONCEPT-NAME: description in one line
CONCEPT-NAME: description in one line
CONCEPT-NAME: description in one line")
                       (response (llm-query-formatted prompt format-spec))
                       (analogies (parse-llm-analogies response)))
                  (if analogies
                      (incf (getf *llm-heuristic-stats* :successful-parses))
                      (progn
                        (incf (getf *llm-heuristic-stats* :failed-parses))
                        (log-parse-failure 'h-llm-analogize response "No valid entries found")))
                  (cprin1 30 "Parsed " (length analogies) " analogies from LLM response~%")
                  (setf *suggested-concepts* analogies)))
  then-define-new-concepts (lambda (f)
                            (let ((created-count 0))
                              (dolist (analog *suggested-concepts*)
                                (when (and analog 
                                          (not (unitp (analog-name analog))))
                                  (let ((new-unit (create-unit (analog-name analog))))
                                    (put new-unit 'isa (copy (isa f)))
                                    (put new-unit 'worth (floor (* 0.8 (worth f))))
                                    (put new-unit 'english (analog-description analog))
                                    (put new-unit 'creditors '(h-llm-analogize))
                                    (put new-unit 'llm-generated t)
                                    (addprop f 'analogies new-unit)
                                    (incf created-count)
                                    (incf (getf *llm-heuristic-stats* :concepts-created))
                                    (cprin1 25 "Created analog concept: " new-unit "~%"))))
                              (when (> created-count 0)
                                (cprin1 20 "H-LLM-ANALOGIZE created " created-count " new concepts~%")
                                t)))
  then-add-to-agenda (lambda (f)
                      (when *suggested-concepts*
                        (let ((tasks nil))
                          (dolist (analog *suggested-concepts*)
                            (let ((unit-name (analog-name analog)))
                              (when (unitp unit-name)
                                (push `(,(average-worths unit-name 'h-llm-analogize)
                                        ,unit-name
                                        examples
                                        (("Exploring LLM-suggested analog of" ,f))
                                        ((credit-to h-llm-analogize)))
                                      tasks))))
                          (when tasks
                            (add-to-agenda tasks)
                            (add-task-results 'new-tasks 
                                             (list (length tasks) 
                                                   "analog concepts to explore"))))))
  arity 1)

(defheuristic h-llm-pattern-find
  isa (heuristic op anything)
  english "IF a concept has many interesting examples, THEN ask LLM to identify patterns that might suggest new predicates"
  if-potentially-relevant (lambda (f)
                          (and (>= (length (int-examples f)) 5)
                               (memb 'category (isa f))))
  worth 700
  abbrev "LLM identifies patterns in interesting examples"
  then-compute (lambda (f)
                (let* ((examples (take 10 (int-examples f)))
                       (prompt (format nil "Find patterns in: ~{~A~^, ~}" examples))
                       (format-spec "PATTERN: name-here
TEST: how to test
PROPERTY: what property

PATTERN: name-here
TEST: how to test  
PROPERTY: what property")
                       (response (llm-query-formatted prompt format-spec))
                       (patterns (parse-llm-predicates response)))
                  (if patterns
                      (incf (getf *llm-heuristic-stats* :successful-parses))
                      (progn
                        (incf (getf *llm-heuristic-stats* :failed-parses))
                        (log-parse-failure 'h-llm-pattern-find response "No valid patterns found")))
                  (setf *pattern-predicates* patterns)))
  then-define-new-concepts (lambda (f)
                            (let ((created 0))
                              (dolist (pred-spec *pattern-predicates*)
                                (when pred-spec
                                  (let ((new-pred (create-unit (gensym "LLM-PRED-"))))
                                    (put new-pred 'isa '(pred unary-pred math-pred anything))
                                    (put new-pred 'worth 500)
                                    (put new-pred 'arity 1)
                                    (put new-pred 'domain (list f))
                                    (put new-pred 'range '(bit))
                                    (put new-pred 'english (pred-spec-description pred-spec))
                                    (put new-pred 'fast-defn (compile-pattern-test pred-spec))
                                    (put new-pred 'creditors '(h-llm-pattern-find))
                                    (put new-pred 'llm-generated t)
                                    (addprop f 'pattern-predicates new-pred)
                                    (incf created)
                                    (incf (getf *llm-heuristic-stats* :concepts-created)))))
                              (cprin1 20 "Created " created " pattern predicates~%")
                              (> created 0)))
  arity 1)

(defheuristic h-llm-conjecture
  isa (heuristic op anything)
  english "IF interesting relationships exist between concepts, THEN ask LLM to suggest mathematical conjectures"
  if-potentially-relevant (lambda (f)
                          (and (conjectures f)
                               (> (worth f) 700)))
  worth 600
  abbrev "LLM suggests mathematical conjectures"
  then-compute (lambda (f)
                (let* ((related-concepts (find-related-high-worth-concepts f))
                       (prompt (format nil "Given: ~A" 
                                     (describe-concept-network f related-concepts)))
                       (format-spec "CONJECTURE: name-without-spaces
STATEMENT: mathematical statement here
INVOLVES: concept1, concept2, concept3

CONJECTURE: another-name
STATEMENT: another mathematical statement
INVOLVES: concept1, concept2")
                       (response (llm-query-formatted prompt format-spec))
                       (conjectures (parse-llm-conjectures response)))
                  (if conjectures
                      (incf (getf *llm-heuristic-stats* :successful-parses))
                      (progn
                        (incf (getf *llm-heuristic-stats* :failed-parses))
                        (log-parse-failure 'h-llm-conjecture response "No valid conjectures found")))
                  (setf *llm-conjectures* conjectures)))
  then-conjecture (lambda (f)
                   (dolist (conj-spec *llm-conjectures*)
                     (let ((conjec (new-name 'conjec)))
                       (create-unit conjec 'proto-conjec)
                       (put conjec 'english (conj-spec-text conj-spec))
                       (put conjec 'worth (floor (* 0.7 (worth f))))
                       (put conjec 'conjecture-about (conj-spec-concepts conj-spec))
                       (put conjec 'llm-generated t)
                       (push conjec *conjectures*)
                       (incf (getf *llm-heuristic-stats* :concepts-created))))
                   (length *llm-conjectures*))
  arity 1)

(defheuristic h-llm-specialize-guided
  isa (heuristic op anything)
  english "IF specializing a concept, THEN ask LLM for mathematically interesting ways to constrain it"
  if-potentially-relevant null
  worth 750
  if-working-on-task (lambda (task)
                      (and (is-a-kind-of *cur-slot* 'specializations)
                           (null (assoc 'slot-to-change *cur-sup*))))
  then-compute (lambda (task)
                (let* ((prompt (format nil "Concept ~A has ~A. Suggest specializations."
                                     *cur-unit*
                                     (get-key-properties *cur-unit*)))
                       (format-spec "SPECIALIZE: slot-name
CONSTRAINT: how to constrain it
RATIONALE: why this makes mathematical sense

SPECIALIZE: another-slot
CONSTRAINT: another constraint
RATIONALE: another rationale")
                       (response (llm-query-formatted prompt format-spec))
                       (suggestions (parse-specialization-suggestions response)))
                  (if suggestions
                      (incf (getf *llm-heuristic-stats* :successful-parses))
                      (progn
                        (incf (getf *llm-heuristic-stats* :failed-parses))
                        (log-parse-failure 'h-llm-specialize-guided response "No valid suggestions")))
                  (setf *llm-specializations* suggestions)))
  then-add-to-agenda (lambda (task)
                      (when *llm-specializations*
                        (add-to-agenda 
                          (mapcar (lambda (spec)
                                    `(,(average-worths *cur-unit* 'h-llm-specialize-guided)
                                      ,*cur-unit*
                                      specializations
                                      (("LLM suggests specializing" ,(spec-slot spec) 
                                        "because" ,(getf spec :rationale)))
                                      ((slot-to-change ,(spec-slot spec))
                                       (credit-to h-llm-specialize-guided))))
                                  *llm-specializations*))
                        (add-task-results 'new-tasks 
                                         (list (length *llm-specializations*)
                                               "LLM-guided specializations"))))
  arity 1)

;;; =============================================================================
;;; LLM heuristic to implement concepts as code
;;; =============================================================================

;;; Global variables for code generation
(defvar *llm-generated-code* nil)
(defvar *allowed-eurisko-symbols* 
  '(lambda let let* progn
    ;; List operations
    cons car cdr caar cadr cdar cddr caddr list append remove member 
    remove-if remove-if-not copy-list reverse length nth nthcdr
    ;; Predicates  
    consp atom listp null equal eq eql = < > <= >= zerop plusp minusp
    numberp integerp symbolp
    ;; Logic
    and or not cond if when unless
    ;; Arithmetic
    + - * / 1+ 1- floor ceiling round mod rem min max abs
    ;; Eurisko-specific
    run-alg run-defn random-choose subset divides
    ;; Variables (common in Eurisko patterns)
    x y z s s1 s2 n m i j k a b c d e f g h
    nil t))

(defun validate-code-tree (code allowed-symbols)
  "Recursively validate that code only uses allowed symbols"
  (cond ((null code) t)
        ((numberp code) t)  ; Numbers are always allowed
        ((stringp code) t)  ; Strings are allowed
        ((keywordp code) t) ; Keywords allowed
        ((symbolp code) 
         (or (member code allowed-symbols)
             (member code lambda-list-keywords))) ; Allow &optional, &rest, etc
        ((listp code)
         ;; Special handling for quoted forms
         (cond ((eq (first code) 'quote) t)
               ((eq (first code) 'function) 
                (validate-code-tree (second code) allowed-symbols))
               (t (every (lambda (x) (validate-code-tree x allowed-symbols)) code))))
        (t nil)))

(defun parse-llm-code (response)
  "Parse and validate LLM-generated code - extract just the lambda"
  (setf *llm-generated-code* nil)
  (handler-case
      (let ((code nil)
            (clean-response response))
        ;; Remove markdown code blocks if present
        (when (search "```" clean-response)
          (let ((start (+ 3 (search "```" clean-response)))
                (end (search "```" clean-response :start2 4)))
            (when end
              (setf clean-response (subseq clean-response start end)))))
        
        ;; Find the lambda expression
        (let ((lambda-start (search "(lambda" clean-response)))
          (when lambda-start
            ;; Read just from the lambda forward
            (setf code (read-from-string (subseq clean-response lambda-start)))
            
            ;; Validate the code
            (when (and (listp code)
                       (eq (first code) 'lambda)
                       (listp (second code))
                       (validate-code-tree code *allowed-eurisko-symbols*))
              (cprin1 35 "Successfully parsed LLM code: " code "~%")
              (setf *llm-generated-code* code)))))
    (error (e) 
      (cprin1 20 "Failed to parse LLM code: " e "~%")
      (log-parse-failure 'h-llm-implement-concept response 
                        (format nil "Parse error: ~A" e))
      nil)))

(defun infer-signature (unit code)
  "Infer domain and range from lambda expression"
  (when (and (listp code) (eq (first code) 'lambda))
    (let ((params (second code))
          (arity (length (second code))))
      ;; Set arity
      (put unit 'arity arity)
      ;; Default domain - ANYTHING for each parameter
      (put unit 'domain (make-list arity :initial-element 'anything))
      ;; Try to infer better domain from parameter names
      (when (and (= arity 1) 
                 (member (first params) '(s list struc struct)))
        (put unit 'domain '(structure)))
      (when (and (= arity 1)
                 (member (first params) '(n num number x)))
        (put unit 'domain '(nnumber)))
      (when (and (= arity 2)
                 (member (first params) '(x n))
                 (member (second params) '(s list struct)))
        (put unit 'domain '(anything structure)))
      ;; Default range
      (put unit 'range '(anything))
      (cprin1 40 "Inferred signature for " unit ": domain=" (domain unit) 
              ", range=" (range unit) ", arity=" arity "~%"))))


(defun test-generated-code (unit code test-inputs)
  "Safely test generated code with timeout and error handling"
  (let ((success-count 0)
        (total-tests (length test-inputs)))
    (dolist (input test-inputs)
      (handler-case
          (bt:with-timeout (0.1) ; 100ms timeout per test
            (let ((result (apply code input)))
              (cprin1 50 "Test input " input " -> " result "~%")
              (incf success-count)))
        (error (e)
          (cprin1 40 "Code failed on input " input ": " e "~%"))))
    (cprin1 35 "Code testing: " success-count "/" total-tests " successful~%")
    (> success-count 0)))

(defheuristic h-llm-implement-concept
  isa (heuristic op anything)
  english "IF an LLM concept needs implementation, THEN ask LLM for Eurisko-style code"
  worth 750
  abbrev "LLM provides executable code for hollow concepts"
  if-potentially-relevant (lambda (f)
                          (and (get f 'llm-generated)
                               (null (alg f))
                               (null (defn f))))
  then-compute (lambda (f)
                (let* ((prompt (format nil "Write a lambda expression for ~A (~A).
Use only: cons, car, cdr, list, null, equal, cond, if, +, -, *, <, >
Example: (lambda (x y) (cons x y))
To call another Eurisko operation, use: (run-alg 'operation-name args)
Example: (lambda (x) (run-alg 'square x))
Write only the lambda, no explanation:" 
                                     f 
                                     (or (get f 'english) "concept")))
                       (response (llm-query prompt)))
                  (cprin1 30 "LLM response: " response "~%")
                  (parse-llm-code response)
                  (put f 'llm-code-response response)))
  then-modify-slots (lambda (f)
                     (when *llm-generated-code*
                       (cprin1 25 "Installing code for " f "~%")
                       (handler-case
                           (progn
                             (put f 'fast-alg (compile nil *llm-generated-code*))
                             (put f 'llm-generated-alg *llm-generated-code*)
                             (infer-signature f *llm-generated-code*)
                             (cprin1 20 "Successfully implemented " f "~%")
                             t)
                         (error (e)
                           (cprin1 20 "Failed to install code: " e "~%")
                           (put f 'llm-code-error (format nil "~A" e))
                           nil))))
  then-add-to-agenda (lambda (f)
                      (when (alg f)
                        (add-to-agenda 
                          `((,(+ 50 (worth f)) ,f applics 
                             (("Now that" ,f "has code, find applications"))
                             ((credit-to h-llm-implement-concept)))))
                        (add-task-results 'new-tasks 
                                         '("1 task to explore newly implemented concept"))))
  arity 1)

(defheuristic h-llm-implement-simple
  isa (heuristic op anything)
  english "IF LLM concept is too complex for full implementation, THEN create a simplified version"
  worth 700
  abbrev "Create simplified stub implementations"
  if-potentially-relevant (lambda (f)
                          (and (get f 'llm-generated)
                               (null (alg f))
                               (get f 'llm-code-error))) ; Failed before
  then-compute (lambda (f)
                ;; Create a very simple stub based on ISA
                (cond
                  ((member 'binary-op (isa f))
                   (setf *llm-generated-code* '(lambda (x y) (list x y))))
                  ((member 'unary-op (isa f))
                   (setf *llm-generated-code* '(lambda (x) x)))
                  ((member 'pred (isa f))
                   (setf *llm-generated-code* '(lambda (x) (not (null x)))))
                  (t 
                   (setf *llm-generated-code* '(lambda () 'not-implemented)))))
  then-modify-slots (lambda (f)
                     (when *llm-generated-code*
                       (put f 'fast-alg (compile nil *llm-generated-code*))
                       (put f 'simplified-implementation t)
                       (infer-signature f *llm-generated-code*)
                       (cprin1 25 "Created simplified implementation for " f "~%")
                       t))
  arity 1)

;;; Helper function to force implementation attempts
(defun try-implement-llm-concepts (&optional (limit 5))
  "Try to implement some LLM-generated concepts that lack algorithms"
  (let ((candidates (remove-if-not 
                      (lambda (u) 
                        (and (get u 'llm-generated)
                             (null (alg u))
                             (null (defn u))))
                      *units*))
        (count 0))
    (dolist (unit (take limit candidates))
      (format t "~%Attempting to implement ~A...~%" unit)
      (when (interp2 'h-llm-implement-concept unit)
        (incf count)))
    (format t "~%Successfully implemented ~A/~A concepts~%" count (min limit (length candidates)))))

;;; Add to statistics tracking
(setf (getf *llm-heuristic-stats* :concepts-implemented) 0)

(defun llm-implementation-report ()
  "Detailed report on implementation attempts"
  (format t "~%=== LLM IMPLEMENTATION REPORT ===~%")
  
  ;; Sort by success
  (let ((success nil)
        (failure nil))
    (dolist (u *units*)
      (when (and (get u 'llm-generated)
                 (get u 'llm-code-response))
        (if (alg u)
            (push u success)
            (push u failure))))
    
    (format t "~%SUCCESSFUL IMPLEMENTATIONS (~A):~%" (length success))
    (dolist (s success)
      (format t "~%~A:~%" s)
      (format t "  Code: ~A~%" (get s 'llm-generated-alg)))
    
    (format t "~%FAILED IMPLEMENTATIONS (~A):~%" (length failure))
    (dolist (f (take 5 failure))
      (format t "~%~A:~%" f)
      (format t "  Response: ~A...~%" 
              (subseq (get f 'llm-code-response) 0 
                      (min 80 (length (get f 'llm-code-response)))))
      (when (get f 'llm-code-error)
        (format t "  Error: ~A~%" (get f 'llm-code-error))))))

;;; =============================================================================
;;; INITIALIZATION
;;; =============================================================================

(defun register-llm-heuristics ()
  "Register LLM heuristics with the EURISKO system"
  (let ((llm-heuristics
          '(
            h-llm-analogize
            h-llm-pattern-find
            h-llm-conjecture
            h-llm-specialize-guided
            h-llm-implement-concept
            h-llm-implement-simple
            )))
    
    (when (fboundp 'union-prop)
      (dolist (h llm-heuristics)
        (union-prop 'heuristic 'examples h)))
    
    (format t "Registered ~A LLM heuristics~%" (length llm-heuristics))))

(defun initialize-llm-heuristics ()
  "Initialize LLM-enhanced heuristics system"
  (cprin1 13 "~%Initializing LLM-enhanced EURISKO heuristics...~%")
  (cprin1 13 "Use (configure-llm :ollama :model \"qwen2.5:14b\") to enable LLM features~%")
  (register-llm-heuristics)
  (llm-status))

(initialize-llm-heuristics)
