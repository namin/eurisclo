(in-package "EURISCLO")

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
                (let* ((prompt (format nil "The concept ~A has these properties: ~A. What are some analogous mathematical or computational concepts that share similar structural properties? List 2-3 concepts with brief explanations."
                                     f
                                     (describe-concept-properties f)))
                       (response (llm-query prompt))
                       (analogies (parse-llm-analogies response)))
                  (setf *suggested-concepts* analogies)))
  then-define-new-concepts (lambda (f)
                            (dolist (analog *suggested-concepts*)
                              (when (not (unitp (analog-name analog)))
                                (let ((new-unit (create-unit (analog-name analog))))
                                  ;; Copy structural properties from original
                                  (put new-unit 'isa (copy (isa f)))
                                  (put new-unit 'worth (floor (* 0.8 (worth f))))
                                  (put new-unit 'english (analog-description analog))
                                  (put new-unit 'creditors '(h-llm-analogize))
                                  (addprop f 'analogies new-unit)))))
  then-add-to-agenda (lambda (f)
                      (add-to-agenda 
                        (mapcar (lambda (new-concept)
                                  `(,(average-worths new-concept 'h-llm-analogize)
                                    ,new-concept
                                    examples
                                    (("Exploring LLM-suggested analog of" ,f))
                                    ((credit-to h-llm-analogize))))
                                *suggested-concepts*))))

(defheuristic h-llm-pattern-find
  isa (heuristic op anything)
  english "IF a concept has many interesting examples, THEN ask LLM to identify patterns that might suggest new predicates or operations"
  if-potentially-relevant (lambda (f)
                          (and (>= (length (int-examples f)) 5)
                               (memb 'category (isa f))))
  worth 700
  abbrev "LLM identifies patterns in interesting examples"
  then-compute (lambda (f)
                (let* ((examples-str (format nil "~{~A~^, ~}" 
                                           (mapcar #'prin1-to-string 
                                                  (take 10 (int-examples f)))))
                       (prompt (format nil "Analyze these examples: ~A. What mathematical patterns or properties do they share? Suggest 2-3 predicates that might characterize them."
                                     examples-str))
                       (response (llm-query prompt)))
                  (setf *pattern-predicates* (parse-llm-predicates response))))
  then-define-new-concepts (lambda (f)
                            (dolist (pred-spec *pattern-predicates*)
                              (let ((new-pred (create-unit (gensym "llm-pred-"))))
                                (put new-pred 'isa '(pred unary-pred math-pred anything))
                                (put new-pred 'worth 500)
                                (put new-pred 'arity 1)
                                (put new-pred 'domain (list f))
                                (put new-pred 'range '(bit))
                                (put new-pred 'english (pred-spec-description pred-spec))
                                ;; Generate a test function based on the pattern
                                (put new-pred 'fast-defn 
                                     (compile-pattern-test pred-spec))
                                (put new-pred 'creditors '(h-llm-pattern-find))
                                (addprop f 'pattern-predicates new-pred)))))

(defheuristic h-llm-conjecture
  isa (heuristic op anything)
  english "IF interesting relationships exist between concepts, THEN ask LLM to suggest mathematical conjectures about them"
  if-potentially-relevant (lambda (f)
                          (and (conjectures f)
                               (> (worth f) 700)))
  worth 600
  abbrev "LLM suggests mathematical conjectures"
  then-compute (lambda (f)
                (let* ((related-concepts (find-related-high-worth-concepts f))
                       (prompt (format nil "Given these mathematical concepts and their relationships: ~A. Suggest 2-3 plausible mathematical conjectures that might hold."
                                     (describe-concept-network f related-concepts)))
                       (response (llm-query prompt)))
                  (setf *llm-conjectures* (parse-llm-conjectures response))))
  then-conjecture (lambda (f)
                   (dolist (conj-spec *llm-conjectures*)
                     (let ((conjec (new-name 'conjec)))
                       (create-unit conjec 'proto-conjec)
                       (put conjec 'english (conj-spec-text conj-spec))
                       (put conjec 'worth (floor (* 0.7 (worth f))))
                       (put conjec 'conjecture-about (conj-spec-concepts conj-spec))
                       (push conjec *conjectures*)))))

(defheuristic h-llm-specialize-guided
  isa (heuristic op anything)
  english "IF specializing a concept, THEN ask LLM for mathematically interesting ways to constrain it"
  if-potentially-relevant null
  worth 750
  if-working-on-task (lambda (task)
                      (and (is-a-kind-of *cur-slot* 'specializations)
                           (null (assoc 'slot-to-change *cur-sup*))))
  then-compute (lambda (task)
                (let* ((prompt (format nil "The concept ~A with properties ~A needs specialization. Suggest 2-3 mathematically interesting ways to add constraints or restrictions."
                                     *cur-unit*
                                     (get-key-properties *cur-unit*)))
                       (response (llm-query prompt))
                       (suggestions (parse-specialization-suggestions response)))
                  (setf *llm-specializations* suggestions)))
  then-modify-slots (lambda (task)
                     (dolist (spec *llm-specializations*)
                       (let ((slot-name (spec-slot spec))
                             (modification (spec-modification spec)))
                         (when (and (slot-exists-p slot-name)
                                   (can-modify-p slot-name modification))
                           (apply-specialization *cur-unit* slot-name modification))))))

(defun describe-concept-properties (unit)
  "Generate a concise description of a unit's key properties"
  (format nil "Domain: ~A, Range: ~A, Arity: ~A, Type: ~A" 
          (domain unit)
          (range unit) 
          (arity unit)
          (car (isa unit))))

(defun parse-llm-analogies (response)
  "Parse LLM response for analog concepts. Expects format: CONCEPT-NAME: description"
  (let ((analogies nil))
    (dolist (line (split-string response #\Newline))
      (when (and (> (length line) 3)
                 (find #\: line))
        (let* ((colon-pos (position #\: line))
               (name-part (string-trim " " (subseq line 0 colon-pos)))
               (desc-part (string-trim " " (subseq line (1+ colon-pos))))
               (cleaned-name (clean-concept-name name-part)))
          (when (and cleaned-name (> (length desc-part) 10))
            (push (list :name cleaned-name 
                        :description desc-part
                        :original-name name-part)
                  analogies)))))
    (reverse analogies)))

(defun clean-concept-name (name)
  "Convert a natural language concept name into a valid Eurisko unit name"
  ;; Remove special characters, convert to uppercase, replace spaces with hyphens
  (let ((cleaned (string-upcase 
                  (substitute #\- #\Space 
                              (remove-if-not (lambda (c) 
                                              (or (alphanumericp c) 
                                                  (char= c #\Space)))
                                            name)))))
    (when (> (length cleaned) 0)
      (intern cleaned))))

(defun analog-name (analog-spec)
  (getf analog-spec :name))

(defun analog-description (analog-spec)
  (getf analog-spec :description))

(defun h-llm-pattern-find-prompt (examples)
  "Generate a structured prompt for pattern finding"
  (format nil "Analyze these mathematical examples: ~A

Please identify patterns in the format:
PATTERN: [name of pattern]
TEST: [simple description of how to test if something has this pattern]
PROPERTY: [mathematical property being tested]

Example format:
PATTERN: palindromic-number
TEST: number reads the same forwards and backwards
PROPERTY: symmetric digit sequence

Provide 2-3 patterns:" 
          (format nil "~{~A~^, ~}" examples)))

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
      ;; but records what it was asked to test
      (t 
       (lambda (x)
         (cprin1 50 "LLM pattern test for ~A on ~A~%" property x)
         nil)))))

(defun pred-spec-description (pred-spec)
  (format nil "Tests for ~A: ~A" 
          (getf pred-spec :property)
          (getf pred-spec :test)))

(defun describe-concept-network (unit related-units)
  "Describe a concept and its relationships for conjecture generation"
  (format nil "~A (worth ~A) with properties: ~A. Related concepts: ~{~A~^, ~}"
          unit
          (worth unit)
          (get-key-properties unit)
          (mapcar (lambda (u) (format nil "~A(~A)" u (car (isa u))))
                  related-units)))

(defun llm-conjecture-prompt (concept-description)
  (format nil "~A

Suggest mathematical conjectures in this format:
CONJECTURE: [name]
STATEMENT: [precise mathematical statement]
INVOLVES: [comma-separated list of concepts involved]

Example:
CONJECTURE: distributivity-over-union
STATEMENT: For any operation f and sets A,B: f(A∪B) = f(A)∪f(B) when f preserves structure
INVOLVES: operation, set-union, structure-preservation

Provide 2-3 conjectures:"
          concept-description))

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

(defun llm-specialization-prompt (unit properties)
  (format nil "Concept ~A has ~A.

Suggest specializations in this format:
SPECIALIZE: [slot-name]
CONSTRAINT: [how to constrain it]
RATIONALE: [why this makes mathematical sense]

Valid slots: domain, range, fast-defn, examples
Example:
SPECIALIZE: domain
CONSTRAINT: restrict first argument to prime-numbers only  
RATIONALE: prime numbers have unique factorization properties

Provide 2-3 specializations:"
          unit properties))

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

(defun find-related-high-worth-concepts (unit)
  "Find concepts related to unit with high worth"
  (remove-duplicates
   (append (generalizations unit)
           (specializations unit)
           (remove-if-not (lambda (u) (> (worth u) 600))
                         (examples (car (isa unit)))))))


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
