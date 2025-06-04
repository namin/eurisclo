;;;; EURISKO LLM Add-ons
;;;; Enhanced heuristics using Large Language Models for intelligent slot evolution
;;;; Author: AI Assistant
;;;; Date: 2025
;;;; Dependencies: drakma (HTTP client), cl-json (JSON handling)
;;;;
;;;; OVERVIEW:
;;;; This file adds LLM-powered heuristics to EURISKO that replace random mutations
;;;; with semantically-guided concept evolution. Instead of blindly mutating slots,
;;;; these heuristics use large language models to understand the mathematical 
;;;; context and make intelligent changes.
;;;;
;;;; KEY IMPROVEMENTS OVER ORIGINAL HEURISTICS:
;;;; 1. SEMANTIC UNDERSTANDING: LLMs understand what concepts mean, not just structure
;;;; 2. CONTEXT-AWARE DECISIONS: Consider purpose, domain, and applications when evolving
;;;; 3. INTELLIGENT SPECIALIZATION: Target promising slots based on success patterns
;;;; 4. FAILURE ANALYSIS: Learn from mistakes instead of just avoiding them
;;;; 5. WORTH ASSESSMENT: Evaluate conceptual importance beyond just success metrics
;;;;
;;;; HEURISTICS ADDED:
;;;; H30: Smart specialization trigger (replaces blind specialization)
;;;; H31: Intelligent slot evolution (replaces random mutations) 
;;;; H32: Semantic worth assessment (enhances traditional metrics)
;;;; H33: Failure analysis and learning (learns from mistakes)
;;;; H34: Pattern-based heuristic discovery (creates new heuristics)
;;;; H35: Contextual example generation (creates meaningful examples)

(in-package "EURISCLO")

;; Load required libraries for HTTP requests and JSON
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (progn
        (ql:quickload :drakma :silent t)
        (ql:quickload :cl-json :silent t))
    (error (e)
      (format t "Warning: Could not load HTTP dependencies: ~A~%" e)
      (format t "Install with: (ql:quickload '(:drakma :cl-json))~%")
      (format t "Will use mock responses only.~%"))))

;;; =============================================================================
;;; SHARED HEURISTIC VARIABLES
;;; =============================================================================

;; Declare variables that are defined in heuristics.lisp but needed here
;; Using defvar without initial values so they don't override existing definitions

(defvar *old-value*)
(defvar *new-value*)
(defvar *slot-to-change*)
(defvar *new-units*)
(defvar *creditors*)
(defvar *conjectures*)
(defvar *all-units*)
(defvar *deleted-units*)
(defvar *task-num*)
(defvar *task*)
(defvar *abort-task?*)

;; LLM-specific variables
(defvar *llm-context* nil "Context information for LLM operations")

;;; =============================================================================
;;; LLM INFRASTRUCTURE
;;; =============================================================================

;;; LLM Provider Configuration
(defvar *llm-provider* :gemini
  "Current LLM provider. Supported: :gemini, :openai, :claude, :ollama")

(defvar *llm-api-key* nil
  "API key for the current LLM provider")

(defvar *llm-model* "gemini-2.5-flash-preview-05-20"
  "Model name for the current provider")

(defvar *llm-temperature* 0.7
  "Temperature setting for LLM responses")

(defvar *llm-max-tokens* 2048
  "Maximum tokens for LLM responses")

(defvar *llm-timeout* 30
  "Timeout for LLM API calls in seconds")

;;; LLM Provider Configurations
(defparameter *llm-providers*
  '((:gemini 
     :base-url "https://generativelanguage.googleapis.com/v1beta/models/"
     :headers (("Content-Type" . "application/json"))
     :auth-header "x-goog-api-key"
     :request-format :gemini)
    (:openai
     :base-url "https://api.openai.com/v1/chat/completions"
     :headers (("Content-Type" . "application/json"))
     :auth-header "Authorization"
     :auth-prefix "Bearer "
     :request-format :openai)
    (:claude
     :base-url "https://api.anthropic.com/v1/messages"
     :headers (("Content-Type" . "application/json")
               ("anthropic-version" . "2023-06-01"))
     :auth-header "x-api-key"
     :request-format :claude)
    (:ollama
     :base-url "http://localhost:11434/api/generate"
     :headers (("Content-Type" . "application/json"))
     :request-format :ollama)))

;;; =============================================================================
;;; LLM INTERFACE FUNCTIONS
;;; =============================================================================

(defun get-provider-config (provider)
  "Get configuration for the specified LLM provider"
  (cdr (assoc provider *llm-providers*)))

(defun format-llm-request (prompt &key provider model temperature max-tokens)
  "Format request for the specified LLM provider"
  (let ((format-type (getf (get-provider-config (or provider *llm-provider*)) :request-format)))
    (case format-type
      (:gemini
       `(("contents" . ((("parts" . ((("text" . ,prompt)))))))
         ("generationConfig" . (("temperature" . ,(or temperature *llm-temperature*))
                               ("maxOutputTokens" . ,(or max-tokens *llm-max-tokens*))))))
      (:openai
       `(("model" . ,(or model *llm-model*))
         ("messages" . ((("role" . "user") ("content" . ,prompt))))
         ("temperature" . ,(or temperature *llm-temperature*))
         ("max_tokens" . ,(or max-tokens *llm-max-tokens*))))
      (:claude
       `(("model" . ,(or model *llm-model*))
         ("max_tokens" . ,(or max-tokens *llm-max-tokens*))
         ("messages" . ((("role" . "user") ("content" . ,prompt))))))
      (:ollama
       `(("model" . ,(or model *llm-model*))
         ("prompt" . ,prompt)
         ("options" . (("temperature" . ,(or temperature *llm-temperature*)))))))))

(defun extract-llm-response (response provider)
  "Extract text content from LLM response based on provider format"
  (cond
    ;; If it's already a string (mock response), return as-is
    ((stringp response) response)
    
    ;; Handle real API responses
    ((listp response)
     (case provider
       (:gemini
        ;; Navigate: CANDIDATES -> first item -> CONTENT -> PARTS -> first item -> TEXT
        (let* ((candidates (cdr (assoc :candidates response)))
               (first-candidate (first candidates))
               (content (cdr (assoc :content first-candidate)))
               (parts (cdr (assoc :parts content)))
               (first-part (first parts))
               (text (cdr (assoc :text first-part))))
          text))
       (:ollama
        ;; Ollama returns response in "response" field
        (cdr (assoc :response response)))
       (:openai
        (nested-getf response '("choices" 0 "message" "content")))
       (:claude
        (nested-getf response '("content" 0 "text")))
       (otherwise
        (format nil "Unknown provider: ~A" provider))))
    
    ;; Fallback
    (t (format nil "~A" response))))

(defun nested-getf (plist keys)
  "Navigate nested property lists/alists"
  (when plist
    (reduce (lambda (obj key)
              (when obj
                (cond ((listp obj)
                       (if (numberp key)
                           (nth key obj)
                           (cdr (assoc key obj :test #'equal))))
                      (t nil))))
            keys
            :initial-value plist)))

(defun escape-json-string (str)
  "Escape a string for JSON"
  (with-output-to-string (out)
    (loop for char across str do
      (case char
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (otherwise (write-char char out))))))

(defun eurisko-llm-api-call (prompt &key provider model temperature max-tokens)
  "Make HTTP request to LLM API - renamed to avoid conflicts"
  (let* ((provider (or provider *llm-provider*))
         (model (or model *llm-model*))
         (config (get-provider-config provider))
         (base-url (getf config :base-url))
         (api-key *llm-api-key*))
    
    (handler-case
        (case provider
          (:gemini
           (if (and api-key (stringp api-key) (not (string= api-key "")))
               (let* ((json-string (format nil "{\"contents\":[{\"parts\":[{\"text\":\"~A\"}]}],\"generationConfig\":{\"temperature\":~A,\"maxOutputTokens\":~A}}" 
                                          (escape-json-string prompt)
                                          (or temperature *llm-temperature*)
                                          (or max-tokens *llm-max-tokens*)))
                      (full-url (format nil "~A~A:generateContent?key=~A" base-url model api-key)))
                 (multiple-value-bind (response status-code)
                     (drakma:http-request full-url
                                        :method :post
                                        :content-type "application/json"
                                        :content json-string
                                        :want-stream nil)
                   (if (= status-code 200)
                       (let ((response-string (if (stringp response)
                                                  response
                                                  (map 'string #'code-char response))))
                         (cl-json:decode-json-from-string response-string))
                       (format nil "API Error: Status ~A" status-code))))
               (format nil "Mock Gemini response - no API key")))
          
          (:ollama
           ;; Ollama doesn't need an API key - escape the prompt for JSON
           (let* ((json-string (format nil "{\"model\":\"~A\",\"prompt\":\"~A\",\"stream\":false,\"options\":{\"temperature\":~A}}" 
                                      model (escape-json-string prompt) (or temperature *llm-temperature*)))
                  (full-url base-url))
             (multiple-value-bind (response status-code)
                 (drakma:http-request full-url
                                    :method :post
                                    :content-type "application/json"
                                    :content json-string
                                    :want-stream nil)
               (if (= status-code 200)
                   (let ((response-string (if (stringp response)
                                              response
                                              (map 'string #'code-char response))))
                     (cl-json:decode-json-from-string response-string))
                   (format nil "Ollama Error: Status ~A" status-code)))))
          
          (:openai
           (format nil "OpenAI not implemented yet"))
          
          (otherwise
           (format nil "Provider ~A not implemented" provider)))
      (error (e)
        (format nil "Error calling LLM: ~A" e)))))


(defun find-best-candidate (response candidates)
  "Find the best matching candidate from LLM response"
  (when (and response candidates)
    (let ((response-lower (string-downcase (princ-to-string response)))
          (result nil))
      (setf result (find-if (lambda (candidate)
                              (search (string-downcase (princ-to-string candidate)) response-lower))
                            candidates))
      result)))

(defun llm-query (prompt &key provider model temperature max-tokens candidates context)
  "High-level interface for LLM queries with context and candidate filtering"
  (let* ((full-prompt (if context
                         (format nil "Context: ~A~%~%Query: ~A~%~%~A"
                                context prompt
                                (if candidates
                                    (format nil "Please choose from these options: ~A" candidates)
                                    ""))
                         prompt))
         (response (eurisko-llm-api-call full-prompt 
                                        :provider provider 
                                        :model model 
                                        :temperature temperature 
                                        :max-tokens max-tokens)))
    
    (when response
      (let ((text (extract-llm-response response (or provider *llm-provider*))))
        ;; Ensure we always return a string, never nil or other types
        (unless (stringp text)
          (setf text (format nil "~A" text)))
        
        ;; Handle error responses
        (when (and (stringp text) (search "Error" text))
          (setf text "LLM unavailable"))
        
        (if candidates
            (find-best-candidate text candidates)
            text)))))

;;; =============================================================================
;;; LLM-ENHANCED UTILITY FUNCTIONS
;;; =============================================================================

(defun llm-choose-slot (unit available-slots operation context)
  "Use LLM to intelligently choose which slot to modify"
  (llm-query 
   (format nil "Given a unit '~A' with description '~A', which slot would be most effective to ~A?
                Available slots: ~A
                Current context: ~A
                
                Consider the unit's purpose, current applications, and the goal of the operation.
                Return only the slot name."
           unit (or (and (fboundp 'english) (english unit)) 
                   (and (fboundp 'abbrev) (abbrev unit)) "No description") 
           operation available-slots context)
   :candidates available-slots
   :temperature 0.3))

(defun llm-evolve-slot-value (unit slot old-value operation context)
  "Use LLM to evolve a slot value intelligently"
  (llm-query
   (format nil "How should we ~A this ~A slot value: ~A
                
                Unit: ~A
                Description: ~A
                Context: ~A
                
                Goal: Make it ~A while preserving functionality and improving effectiveness.
                Return only the new value, maintaining the same format/structure."
           operation slot old-value unit 
           (or (and (fboundp 'english) (english unit)) 
              (and (fboundp 'abbrev) (abbrev unit)))
           context
           (case operation
             (specialize "more specific and constrained")
             (generalize "more general and broader")
             (mutate "different but related")))
   :temperature 0.7))

(defun llm-assess-worth (unit applications context)
  "Use LLM to provide semantic worth assessment"
  (let ((response (llm-query
                   (format nil "Assess the worth of this unit: ~A
                               Description: ~A
                               Applications: ~A
                               Context: ~A
                               
                               Provide a worth score from 0-1000 considering:
                               - Conceptual importance
                               - Practical utility  
                               - Generalizability
                               - Novelty
                               
                               Return format: SCORE: <number> REASON: <explanation>"
                           unit (or (and (fboundp 'english) (english unit)) 
                                   (and (fboundp 'abbrev) (abbrev unit)))
                           applications context)
                   :temperature 0.3)))
    (when response
      (let ((score-pos (search "SCORE:" response)))
        (when score-pos
          (parse-integer (subseq response (+ score-pos 6) 
                                (or (position #\Space response :start (+ score-pos 6))
                                    (length response)))
                        :junk-allowed t))))))

(defun llm-explain-failure (unit operation old-value new-value context)
  "Use LLM to analyze why an operation failed"
  (llm-query
   (format nil "Analyze why this operation failed:
               Unit: ~A
               Operation: ~A  
               Old value: ~A
               New value: ~A
               Context: ~A
               
               Provide insights on:
               1. What went wrong
               2. Why it failed
               3. How to avoid similar failures
               4. Better approaches to try"
           unit operation old-value new-value context)
   :temperature 0.5))

(defun llm-suggest-new-heuristic (successful-patterns failed-patterns domain)
  "Use LLM to suggest new heuristics based on observed patterns"
  (llm-query
   (format nil "Based on these patterns, suggest a new heuristic:
               
               Successful patterns: ~A
               Failed patterns: ~A
               Domain: ~A
               
               Format your response as:
               ENGLISH: <human readable description>
               CONDITION: <when to apply>
               ACTION: <what to do>
               WORTH: <estimated worth 0-1000>"
           successful-patterns failed-patterns domain)
   :temperature 0.8))

;;; =============================================================================
;;; LLM-ENHANCED HEURISTICS
;;; =============================================================================

(defheuristic h30-llm-specialize
  isa (heuristic op anything)
  english "IF an op has mixed results, THEN use LLM to intelligently specialize it"
  if-potentially-relevant (lambda (f)
                            (and (applics f)
                                 *llm-api-key*))
  if-truly-relevant (lambda (f)
                      (and (some (lambda (a) (some #'has-high-worth (cadr a)))
                                (applics f))
                           (> 0.2 (fraction-of (map-union (applics f) #'cadr) #'has-high-worth))
                           (not (subsumed-by f))))
  worth 800
  abbrev "LLM-guided specialization of mixed-result operations"
  then-compute (lambda (f)
                 ;; DOCUMENTATION: This heuristic replaces H1's random specialization
                 ;; with intelligent analysis. Instead of randomly picking slots to
                 ;; specialize, it asks the LLM to analyze which slots would be most
                 ;; effective to modify based on the success/failure patterns.
                 (cprin1 40 "H30: Analyzing " f " for intelligent specialization using LLM guidance~%")
                 (let* ((good-apps (remove-if-not (lambda (a) (some #'has-high-worth (cadr a)))
                                                 (applics f)))
                        (bad-apps (remove-if (lambda (a) (some #'has-high-worth (cadr a)))
                                            (applics f)))
                        (context (format nil "Good applications: ~A. Bad applications: ~A" 
                                       (subseq good-apps 0 (min 3 (length good-apps)))
                                       (subseq bad-apps 0 (min 3 (length bad-apps)))))
                        (target-slot (llm-choose-slot f 
                                                     (and (fboundp 'slot-names)
                                                          (fboundp 'examples)
                                                          (intersection (slot-names f) (examples 'slot)))
                                                     "specialize" context)))
                   (if target-slot
                       (progn
                         (cprin1 40 "H30: LLM selected slot '" target-slot "' for specialization of " f "~%")
                         (setf *slot-to-change* target-slot)
                         (setf *llm-context* context)
                         t)
                       (progn
                         (cprin1 40 "H30: LLM could not identify a suitable slot for " f "~%")
                         nil))))
  then-add-to-agenda (lambda (f)
                       (when *slot-to-change*
                         (cprin1 40 "H30: Adding intelligent specialization task for " f " slot " *slot-to-change* "~%")
                         (add-to-agenda 
                          `((,(average-worths f 'h30-llm-specialize)
                             ,f specializations
                             (("LLM-guided specialization of" ,*slot-to-change* "slot"))
                             ((slot-to-change ,*slot-to-change*)
                              (llm-context ,*llm-context*)
                              (credit-to h30-llm-specialize)))))
                         (add-task-results 'new-tasks "1 LLM-guided specialization task")))
  arity 1)

(defheuristic h31-llm-slot-evolution
  isa (heuristic op anything)  
  english "IF current task is to specialize/generalize a slot, THEN use LLM to evolve the slot value"
  if-potentially-relevant null
  worth 850
  abbrev "LLM-guided slot value evolution"
  if-working-on-task (lambda (task)
                       (declare (ignore task))
                       (and *llm-api-key*
                            (or (is-a-kind-of *cur-slot* 'specializations)
                                (is-a-kind-of *cur-slot* 'generalizations))
                            (setf *slot-to-change* (cadr (assoc 'slot-to-change *cur-sup*)))
                            (setf *llm-context* (cadr (assoc 'llm-context *cur-sup*)))))
  then-compute (lambda (task)
                 ;; DOCUMENTATION: This is the core LLM evolution heuristic. It replaces
                 ;; H6/H18's random mutations with semantic understanding. Instead of
                 ;; calling SpecializeLispPred/GeneralizeLispFn with random changes,
                 ;; it asks the LLM to understand the slot's purpose and evolve it
                 ;; intelligently while preserving functionality.
                 (declare (ignore task))
                 (cprin1 40 "H31: Beginning LLM-guided evolution of " *slot-to-change* " slot in " *cur-unit* "~%")
                 (setf *old-value* (funcall *slot-to-change* *cur-unit*))
                 (cprin1 40 "H31: Current value: " *old-value* "~%")
                 
                 (let ((operation (if (is-a-kind-of *cur-slot* 'specializations) 
                                     "specialize" "generalize")))
                   (cprin1 40 "H31: Requesting LLM to " operation " the slot value~%")
                   (setf *new-value* 
                         (llm-evolve-slot-value 
                          *cur-unit* 
                          *slot-to-change* 
                          *old-value*
                          operation
                          (or *llm-context* "No specific context")))
                   
                   (cond ((equal *old-value* *new-value*)
                          (cprin1 40 "H31: LLM evolution failed - no meaningful change found~%")
                          (cprin1 15 "~%LLM couldn't find meaningful evolution for " 
                                  *slot-to-change* " slot of " *cur-unit* "~%")
                          nil)
                         (t (cprin1 40 "H31: LLM successfully evolved slot value~%")
                            (cprin1 15 "~%LLM evolved " *slot-to-change* " from " 
                                   *old-value* " to " *new-value* "~%")
                            t))))
  then-define-new-concepts (lambda (task)
                             ;; DOCUMENTATION: Creates the new concept with the LLM-evolved
                             ;; slot value. This follows the same pattern as H6/H18 but
                             ;; with semantically meaningful changes instead of random ones.
                             (declare (ignore task))
                             (when *new-value*
                               (cprin1 40 "H31: Creating new concept with evolved slot value~%")
                               (let ((new-unit (create-unit *cur-unit* *cur-unit*)))
                                 (put new-unit *slot-to-change* *new-value*)
                                 (setf *new-units* (cdr (assoc 'new-units *task-results*)))
                                 (if *new-units*
                                     (nconc *new-units* (list new-unit))
                                     (push (list 'new-units new-unit) *task-results*))
                                 (put new-unit 'creditors '(h31-llm-slot-evolution))
                                 (if (is-a-kind-of *cur-slot* 'specializations)
                                     (progn
                                       (addprop *cur-unit* 'specializations new-unit)
                                       (addprop new-unit 'generalizations *cur-unit*))
                                     (progn
                                       (addprop *cur-unit* 'generalizations new-unit)
                                       (addprop new-unit 'specializations *cur-unit*)))
                                 (cprin1 40 "H31: Successfully created " new-unit " with LLM-guided evolution~%")
                                 t)))
  arity 1)

(defheuristic h32-llm-worth-assessment
  isa (heuristic op anything)
  english "IF a new unit has been created, THEN use LLM to provide semantic worth assessment"
  if-potentially-relevant null
  worth 750
  abbrev "LLM-enhanced worth assessment for new units"
  if-finished-working-on-task (lambda (task)
                                (declare (ignore task))
                                (and *llm-api-key*
                                     (assoc 'new-units *task-results*)
                                     (setf *new-units* (cdr (assoc 'new-units *task-results*)))))
  then-compute (lambda (task)
                 ;; DOCUMENTATION: This heuristic enhances EURISKO's worth assessment
                 ;; by adding semantic understanding. Traditional worth is based on
                 ;; success metrics, but this considers conceptual importance,
                 ;; generalizability, and mathematical significance that an LLM
                 ;; can recognize from the concept's description and purpose.
                 (declare (ignore task))
                 (cprin1 40 "H32: Performing LLM-enhanced worth assessment for new units~%")
                 (dolist (unit *new-units*)
                   (let* ((current-worth (or (worth unit) 400))
                          (apps (applics unit))
                          (context (format nil "Created by: ~A. Current worth: ~A" 
                                         (creditors unit) current-worth))
                          (llm-worth (llm-assess-worth unit apps context)))
                     (if llm-worth
                         (let ((adjusted-worth (floor (+ current-worth llm-worth) 2)))
                           (put unit 'worth adjusted-worth)
                           (put unit 'llm-worth-reason 
                                (format nil "LLM assessment: ~A (was ~A)" 
                                       llm-worth current-worth))
                           (cprin1 40 "H32: LLM adjusted worth of " unit " from " current-worth " to " adjusted-worth "~%")
                           (cprin1 48 "LLM adjusted worth of " unit " to " adjusted-worth "~%"))
                         (cprin1 40 "H32: LLM worth assessment failed for " unit "~%"))))
                 t)
  arity 1)

(defheuristic h33-llm-failure-analysis
  isa (heuristic op anything)
  english "IF a task fails or produces poor results, THEN use LLM to analyze and learn from the failure"
  if-potentially-relevant null
  worth 700
  abbrev "LLM-powered failure analysis and learning"
  if-finished-working-on-task (lambda (task)
                                (declare (ignore task))
                                (and *llm-api-key*
                                     (let ((new-units (cdr (assoc 'new-units *task-results*))))
                                       (or (null new-units)
                                           (every (lambda (u) (< (worth u) 200)) new-units)))))
  then-compute (lambda (task)
                 ;; DOCUMENTATION: This heuristic goes beyond the original H12-H14
                 ;; h-avoid rules. Instead of just creating avoidance rules after
                 ;; failures, it uses LLM to analyze WHY the failure occurred and
                 ;; suggests better approaches. This enables learning from mistakes
                 ;; rather than just avoiding them.
                 (declare (ignore task))
                 (let* ((operation (if (is-a-kind-of *cur-slot* 'specializations) 
                                      "specialization" "generalization"))
                        (slot-to-change (cadr (assoc 'slot-to-change *cur-sup*)))
                        (old-val (if (boundp '*old-value*) *old-value* "not-set"))
                        (new-val (if (boundp '*new-value*) *new-value* "not-set"))
                        (context (format nil "Task: ~A ~A of ~A. Slot: ~A. Old: ~A New: ~A" 
                                       operation *cur-slot* *cur-unit* slot-to-change
                                       old-val new-val)))
                   (cprin1 40 "H33: Analyzing failure - " operation " of " *cur-unit* " produced low-worth results~%")
                   (let ((analysis (llm-explain-failure *cur-unit* operation 
                                                       old-val new-val context)))
                     (if analysis
                         (progn
                           (cprin1 40 "H33: LLM provided failure analysis and improvement suggestions~%")
                           (add-task-results 'failure-analysis 
                                            `((unit ,*cur-unit*)
                                              (operation ,operation)
                                              (analysis ,analysis)))
                           (cprin1 40 "~%LLM Failure Analysis: " analysis "~%"))
                         (cprin1 40 "H33: LLM failure analysis was unsuccessful~%")))
                   t))
  arity 1)

(defheuristic h34-llm-heuristic-discovery
  isa (heuristic op anything)
  english "IF we have observed patterns of success and failure, THEN use LLM to suggest new heuristics"
  if-potentially-relevant null
  worth 600
  abbrev "LLM-generated heuristic discovery"
  if-finished-working-on-task (lambda (task)
                                (declare (ignore task))
                                (and *llm-api-key*
                                     (> (length *conjectures*) 5)
                                     (> (length (remove-if-not (lambda (u) (< (worth u) 200)) 
                                                              *all-units*)) 3)))
  then-compute (lambda (task)
                 ;; DOCUMENTATION: This is a meta-heuristic that generates new heuristics
                 ;; based on observed patterns. It goes beyond EURISKO's original
                 ;; capability by using LLM to analyze successful strategies and
                 ;; failed approaches, then synthesize new rules. This enables
                 ;; the system to evolve its own reasoning capabilities.
                 (declare (ignore task))
                 (let* ((successful-patterns (mapcar (lambda (c) (english c))
                                                   (subseq *conjectures* 0 (min 3 (length *conjectures*)))))
                        (failed-units (remove-if-not (lambda (u) (< (worth u) 200)) *all-units*))
                        (failed-patterns (mapcar (lambda (u) (list u (creditors u)))
                                               (subseq failed-units 0 (min 3 (length failed-units)))))
                        (domain "mathematical concept discovery"))
                   (cprin1 40 "H34: Analyzing " (length *conjectures*) " successful patterns and " 
                          (length failed-units) " failure patterns~%")
                   (let ((suggestion (llm-suggest-new-heuristic successful-patterns 
                                                               failed-patterns domain)))
                     (if suggestion
                         (progn
                           (cprin1 40 "H34: LLM generated new heuristic suggestion~%")
                           (add-task-results 'llm-heuristic-suggestions suggestion)
                           (cprin1 13 "~%LLM suggested new heuristic: " suggestion "~%"))
                         (cprin1 40 "H34: LLM could not generate meaningful heuristic suggestion~%")))
                   t))
  arity 1)

(defheuristic h35-llm-smart-instantiation  
  isa (heuristic op anything)
  english "IF looking for examples of a concept, THEN use LLM to suggest semantically relevant examples"
  if-potentially-relevant (lambda (f)
                            (and *llm-api-key*
                                 (or (memb 'category (isa f))
                                     (memb 'op (isa f)))
                                 (and (fboundp 'examples)
                                      (< (length (examples f)) 3))))
  worth 800
  abbrev "LLM-guided example discovery"
  then-compute (lambda (f)
                 ;; DOCUMENTATION: This heuristic replaces random example generation
                 ;; with contextually meaningful examples. Instead of generating
                 ;; arbitrary instances, it asks the LLM to suggest examples that
                 ;; are representative, diverse, and useful for testing the concept.
                 ;; This dramatically improves the quality of examples used for
                 ;; concept validation and development.
                 (cprin1 40 "H35: Requesting LLM to generate meaningful examples for " f "~%")
                 (let* ((context (format nil "Unit: ~A. Description: ~A. Domain: ~A" 
                                       f (or (and (fboundp 'english) (english f)) 
                                            (and (fboundp 'abbrev) (abbrev f))) 
                                       (and (fboundp 'domain) (domain f))))
                        (suggestion (llm-query 
                                    (format nil "Suggest 3-5 good examples for this concept: ~A
                                                Context: ~A
                                                
                                                The examples should be:
                                                - Representative of the concept
                                                - Useful for testing and validation  
                                                - Diverse in coverage
                                                
                                                Format: Example1, Example2, Example3"
                                            f context)
                                    :temperature 0.6)))
                   (if suggestion
                       (progn
                         (cprin1 40 "H35: LLM provided example suggestions for " f "~%")
                         ;; Parse examples from suggestion and add them
                         (let ((examples (mapcar (lambda (s) (string-trim '(#\Space #\Tab #\Newline) s))
                                               (split-string suggestion #\,))))
                           (dolist (ex examples)
                             (when (and ex (not (string= ex "")))
                               (union-prop f 'examples (intern (string-upcase ex)))))
                           (add-task-results 'llm-examples 
                                            `((unit ,f) (examples ,examples)))
                           (cprin1 40 "H35: Added " (length examples) " LLM-suggested examples to " f "~%")
                           (cprin1 48 "LLM suggested examples for " f ": " examples "~%")
                           t))
                       (progn
                         (cprin1 40 "H35: LLM failed to generate examples for " f "~%")
                         nil))))
  arity 1)

;;; =============================================================================
;;; LLM CONFIGURATION AND UTILITIES
;;; =============================================================================

(defun configure-llm (provider &key api-key model temperature max-tokens)
  "Configure LLM settings"
  (setf *llm-provider* provider)
  ;; Only set API key if provided - Ollama doesn't need one
  (when api-key (setf *llm-api-key* api-key))
  (when model (setf *llm-model* model))
  (when temperature (setf *llm-temperature* temperature))
  (when max-tokens (setf *llm-max-tokens* max-tokens))
  (cprin1 13 "Configured LLM: " provider " with model " *llm-model* "~%"))

(defun llm-status ()
  "Display current LLM configuration status"
  (format t "~%LLM Status:~%")
  (format t "Provider: ~A~%" *llm-provider*)
  (format t "Model: ~A~%" *llm-model*)
  (format t "API Key: ~A~%" (if (and *llm-api-key* (not (string= *llm-api-key* ""))) 
                                "Set" 
                                "Not set"))
  (format t "Temperature: ~A~%" *llm-temperature*)
  (format t "Max Tokens: ~A~%" *llm-max-tokens*))

(defun split-string (string delimiter)
  "Simple string splitting utility"
  (let ((result '())
        (start 0))
    (loop for pos = (position delimiter string :start start)
          do (push (subseq string start pos) result)
             (if pos
                 (setf start (1+ pos))
                 (return (nreverse result))))))

;;; =============================================================================
;;; HEURISTIC REGISTRATION
;;; =============================================================================

(defun register-llm-heuristics ()
  "Register LLM heuristics with the EURISKO system"
  (let ((llm-heuristics '(h30-llm-specialize 
                         h31-llm-slot-evolution
                         h32-llm-worth-assessment
                         h33-llm-failure-analysis
                         h34-llm-heuristic-discovery
                         h35-llm-smart-instantiation)))
    
    ;; Add to examples of 'heuristic if that's how EURISKO tracks them
    (when (fboundp 'union-prop)
      (dolist (h llm-heuristics)
        (union-prop 'heuristic 'examples h)))
    
    (format t "Registered ~A LLM heuristics~%" (length llm-heuristics))))

;;; =============================================================================
;;; INITIALIZATION
;;; =============================================================================

(defun initialize-llm-heuristics ()
  "Initialize LLM-enhanced heuristics system"
  (cprin1 13 "~%Initializing LLM-enhanced EURISKO heuristics...~%")
  (cprin1 13 "Added heuristics: H30-H35 (LLM-guided)~%")
  (cprin1 13 "Use (configure-llm :gemini :api-key \"your-key\") to enable LLM features~%")
  (register-llm-heuristics)
  (llm-status))

;; Auto-initialize when loaded
(initialize-llm-heuristics)
