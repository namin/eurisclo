;;;; EURISKO LLM Add-ons
;;;; Enhanced heuristics using Large Language Models for intelligent slot evolution
;;;; Author: AI Assistant
;;;; Date: 2025

(in-package "EURISCLO")

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
     :request-format gemini)
    (:openai
     :base-url "https://api.openai.com/v1/chat/completions"
     :headers (("Content-Type" . "application/json"))
     :auth-header "Authorization"
     :auth-prefix "Bearer "
     :request-format openai)
    (:claude
     :base-url "https://api.anthropic.com/v1/messages"
     :headers (("Content-Type" . "application/json")
               ("anthropic-version" . "2023-06-01"))
     :auth-header "x-api-key"
     :request-format claude)
    (:ollama
     :base-url "http://localhost:11434/api/generate"
     :headers (("Content-Type" . "application/json"))
     :request-format ollama)))

;;; =============================================================================
;;; LLM INTERFACE FUNCTIONS
;;; =============================================================================

(defun get-provider-config (provider)
  "Get configuration for the specified LLM provider"
  (getf *llm-providers* provider))

(defun format-llm-request (prompt &key provider model temperature max-tokens)
  "Format request for the specified LLM provider"
  (let ((format-type (getf (get-provider-config (or provider *llm-provider*)) :request-format)))
    (case format-type
      (gemini
       `(("contents" . ((("parts" . ((("text" . ,prompt)))))))
         ("generationConfig" . (("temperature" . ,(or temperature *llm-temperature*))
                               ("maxOutputTokens" . ,(or max-tokens *llm-max-tokens*))))))
      (openai
       `(("model" . ,(or model *llm-model*))
         ("messages" . ((("role" . "user") ("content" . ,prompt))))
         ("temperature" . ,(or temperature *llm-temperature*))
         ("max_tokens" . ,(or max-tokens *llm-max-tokens*))))
      (claude
       `(("model" . ,(or model *llm-model*))
         ("max_tokens" . ,(or max-tokens *llm-max-tokens*))
         ("messages" . ((("role" . "user") ("content" . ,prompt))))))
      (ollama
       `(("model" . ,(or model *llm-model*))
         ("prompt" . ,prompt)
         ("options" . (("temperature" . ,(or temperature *llm-temperature*)))))))))

(defun extract-llm-response (response provider)
  "Extract text content from LLM response based on provider format"
  (case provider
    (:gemini
     (nested-getf response '("candidates" 0 "content" "parts" 0 "text")))
    (:openai
     (nested-getf response '("choices" 0 "message" "content")))
    (:claude
     (nested-getf response '("content" 0 "text")))
    (:ollama
     (getf response "response"))))

(defun nested-getf (plist keys)
  "Navigate nested property lists/alists"
  (reduce (lambda (obj key)
            (cond ((listp obj)
                   (if (numberp key)
                       (nth key obj)
                       (cdr (assoc key obj :test #'equal))))
                  (t nil)))
          keys
          :initial-value plist))

(defun call-llm-api (prompt &key provider model temperature max-tokens)
  "Make HTTP request to LLM API"
  (let* ((provider (or provider *llm-provider*))
         (config (get-provider-config provider))
         (base-url (getf config :base-url))
         (headers (getf config :headers))
         (auth-header (getf config :auth-header))
         (auth-prefix (getf config :auth-prefix ""))
         (request-data (format-llm-request prompt 
                                          :provider provider 
                                          :model model 
                                          :temperature temperature 
                                          :max-tokens max-tokens)))
    
    ;; Add authentication header
    (when (and *llm-api-key* auth-header)
      (push (cons auth-header (concatenate 'string auth-prefix *llm-api-key*)) headers))
    
    ;; Build full URL for Gemini
    (when (eq provider :gemini)
      (setf base-url (concatenate 'string base-url (or model *llm-model*) ":generateContent")))
    
    ;; Make HTTP request (this would need an actual HTTP client library)
    ;; For now, we'll return a mock response
    (handler-case
        (progn
          (cprin1 99 "LLM API Call: " provider " - " prompt "~%")
          ;; TODO: Replace with actual HTTP client call
          ;; (http-request base-url :method :post :headers headers :content (json:encode-json request-data))
          
          ;; Mock response for testing
          (case provider
            (:gemini '(("candidates" . ((("content" . (("parts" . ((("text" . "Mock Gemini response")))))))))
            (:openai '(("choices" . ((("message" . (("content" . "Mock OpenAI response")))))))))
            (:claude '(("content" . ((("text" . "Mock Claude response"))))))
            (:ollama '(("response" . "Mock Ollama response")))))
      (error (e)
        (cprin1 39 "LLM API Error: " e "~%")
        nil)))))

(defun llm-query (prompt &key provider model temperature max-tokens candidates context)
  "High-level interface for LLM queries with context and candidate filtering"
  (let* ((full-prompt (if context
                         (format nil "Context: ~A~%~%Query: ~A~%~%~A"
                                context prompt
                                (if candidates
                                    (format nil "Please choose from these options: ~A" candidates)
                                    ""))
                         prompt))
         (response (call-llm-api full-prompt 
                                :provider provider 
                                :model model 
                                :temperature temperature 
                                :max-tokens max-tokens)))
    
    (when response
      (let ((text (extract-llm-response response (or provider *llm-provider*))))
        (if candidates
            (find-best-candidate text candidates)
            text)))))

(defun find-best-candidate (response candidates)
  "Find the best matching candidate from LLM response"
  (let ((response-lower (string-downcase (string response))))
    (find-if (lambda (candidate)
               (search (string-downcase (string candidate)) response-lower))
             candidates)))

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
           unit (or (english unit) (abbrev unit) "No description") 
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
           (or (english unit) (abbrev unit))
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
                           unit (or (english unit) (abbrev unit))
                           applications context)
                   :temperature 0.3)))
    (when response
      (let ((score-pos (search "SCORE:" response)))
        (when score-pos
          (parse-integer (subseq response (+ score-pos 6) 
                                (position #\Space response :start (+ score-pos 6)))
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
                 (let* ((good-apps (remove-if-not (lambda (a) (some #'has-high-worth (cadr a)))
                                                 (applics f)))
                        (bad-apps (remove-if (lambda (a) (some #'has-high-worth (cadr a)))
                                            (applics f)))
                        (context (format nil "Good applications: ~A. Bad applications: ~A" 
                                       (subseq good-apps 0 (min 3 (length good-apps)))
                                       (subseq bad-apps 0 (min 3 (length bad-apps)))))
                        (target-slot (llm-choose-slot f 
                                                     (intersection (slot-names f) (examples 'slot))
                                                     "specialize" context)))
                   (when target-slot
                     (setf *slot-to-change* target-slot)
                     (setf *llm-context* context)
                     t)))
  then-add-to-agenda (lambda (f)
                       (when *slot-to-change*
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
                 (declare (ignore task))
                 (setf *old-value* (funcall *slot-to-change* *cur-unit*))
                 (setf *new-value* 
                       (llm-evolve-slot-value 
                        *cur-unit* 
                        *slot-to-change* 
                        *old-value*
                        (if (is-a-kind-of *cur-slot* 'specializations) 
                            "specialize" "generalize")
                        (or *llm-context* "No specific context")))
                 
                 (cond ((equal *old-value* *new-value*)
                        (cprin1 15 "~%LLM couldn't find meaningful evolution for " 
                                *slot-to-change* " slot of " *cur-unit* "~%")
                        nil)
                       (t (cprin1 15 "~%LLM evolved " *slot-to-change* " from " 
                                 *old-value* " to " *new-value* "~%")
                          t)))
  then-define-new-concepts (lambda (task)
                             (declare (ignore task))
                             (when *new-value*
                               (let ((new-unit (create-unit *cur-unit* *cur-unit*)))
                                 (put new-unit *slot-to-change* *new-value*)
                                 (setf *new-units* (list new-unit))
                                 (push (list 'new-units new-unit) *task-results*)
                                 (put new-unit 'creditors '(h31-llm-slot-evolution))
                                 (if (is-a-kind-of *cur-slot* 'specializations)
                                     (progn
                                       (addprop *cur-unit* 'specializations new-unit)
                                       (addprop new-unit 'generalizations *cur-unit*))
                                     (progn
                                       (addprop *cur-unit* 'generalizations new-unit)
                                       (addprop new-unit 'specializations *cur-unit*)))
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
                 (declare (ignore task))
                 (dolist (unit *new-units*)
                   (let* ((current-worth (or (worth unit) 400))
                          (apps (applics unit))
                          (context (format nil "Created by: ~A. Current worth: ~A" 
                                         (creditors unit) current-worth))
                          (llm-worth (llm-assess-worth unit apps context)))
                     (when llm-worth
                       (let ((adjusted-worth (floor (+ current-worth llm-worth) 2)))
                         (put unit 'worth adjusted-worth)
                         (put unit 'llm-worth-reason 
                              (format nil "LLM assessment: ~A (was ~A)" 
                                     llm-worth current-worth))
                         (cprin1 48 "LLM adjusted worth of " unit " to " adjusted-worth "~%")))))
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
                                     (or (null *new-units*)
                                         (every (lambda (u) (< (worth u) 200)) *new-units*))))
  then-compute (lambda (task)
                 (declare (ignore task))
                 (let* ((operation (if (is-a-kind-of *cur-slot* 'specializations) 
                                      "specialization" "generalization"))
                        (context (format nil "Task: ~A ~A of ~A. Slot: ~A" 
                                       operation *cur-slot* *cur-unit* 
                                       (cadr (assoc 'slot-to-change *cur-sup*))))
                        (analysis (llm-explain-failure *cur-unit* operation 
                                                      *old-value* *new-value* context)))
                   (when analysis
                     (add-task-results 'failure-analysis 
                                      `((unit ,*cur-unit*)
                                        (operation ,operation)
                                        (analysis ,analysis)))
                     (cprin1 40 "~%LLM Failure Analysis: " analysis "~%"))
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
                 (declare (ignore task))
                 (let* ((successful-patterns (mapcar (lambda (c) (english c))
                                                   (subseq *conjectures* 0 (min 3 (length *conjectures*)))))
                        (failed-units (remove-if-not (lambda (u) (< (worth u) 200)) *all-units*))
                        (failed-patterns (mapcar (lambda (u) (list u (creditors u)))
                                               (subseq failed-units 0 (min 3 (length failed-units)))))
                        (domain "mathematical concept discovery")
                        (suggestion (llm-suggest-new-heuristic successful-patterns 
                                                              failed-patterns domain)))
                   (when suggestion
                     (add-task-results 'llm-heuristic-suggestions suggestion)
                     (cprin1 13 "~%LLM suggested new heuristic: " suggestion "~%"))
                   t))
  arity 1)

(defheuristic h35-llm-smart-instantiation  
  isa (heuristic op anything)
  english "IF looking for examples of a concept, THEN use LLM to suggest semantically relevant examples"
  if-potentially-relevant (lambda (f)
                            (and *llm-api-key*
                                 (or (memb 'category (isa f))
                                     (memb 'op (isa f)))
                                 (< (length (examples f)) 3)))
  worth 800
  abbrev "LLM-guided example discovery"
  then-compute (lambda (f)
                 (let* ((context (format nil "Unit: ~A. Description: ~A. Domain: ~A" 
                                       f (or (english f) (abbrev f)) (domain f)))
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
                   (when suggestion
                     ;; Parse examples from suggestion and add them
                     (let ((examples (mapcar #'string-trim 
                                           (split-string suggestion #\,))))
                       (dolist (ex examples)
                         (when (and ex (not (string= ex "")))
                           (union-prop f 'examples (intern (string-upcase ex)))))
                       (add-task-results 'llm-examples 
                                        `((unit ,f) (examples ,examples)))
                       (cprin1 48 "LLM suggested examples for " f ": " examples "~%")
                       t))))
  arity 1)

;;; =============================================================================
;;; LLM CONFIGURATION AND UTILITIES
;;; =============================================================================

(defun configure-llm (provider &key api-key model temperature max-tokens)
  "Configure LLM settings"
  (setf *llm-provider* provider)
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
  (format t "API Key: ~A~%" (if *llm-api-key* "Configured" "Not set"))
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
;;; INITIALIZATION
;;; =============================================================================

(defun initialize-llm-heuristics ()
  "Initialize LLM-enhanced heuristics system"
  (cprin1 13 "~%Initializing LLM-enhanced EURISKO heuristics...~%")
  (cprin1 13 "Added heuristics: H30-H35 (LLM-guided)~%")
  (cprin1 13 "Use (configure-llm :gemini :api-key \"your-key\") to enable LLM features~%")
  (llm-status))

;; Auto-initialize when loaded
(initialize-llm-heuristics)
