;;;; EURISKO LLM Integration
;;;; Dependencies: drakma (HTTP client), cl-json (JSON handling)

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
;;; LLM INFRASTRUCTURE
;;; =============================================================================

;;; LLM Provider Configuration
(defvar *llm-provider* :ollama
  "Current LLM provider. Supported: :gemini, :openai, :claude, :ollama")

(defvar *llm-api-key* nil
  "API key for the current LLM provider")

(defvar *llm-model* "qwen2.5:14b"
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
  "Make HTTP request to LLM API - fixed version with proper JSON escaping"
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
        ;; Ollama returns response in :RESPONSE field (keyword)
        (cdr (assoc :response response)))
       (:openai
        (let* ((choices (cdr (assoc :choices response)))
               (first-choice (first choices))
               (message (cdr (assoc :message first-choice)))
               (content (cdr (assoc :content message))))
          content))
       (:claude
        (let* ((content (cdr (assoc :content response)))
               (first-content (first content))
               (text (cdr (assoc :text first-content))))
          text))
       (otherwise
        (format nil "Unknown provider: ~A" provider))))
    
    ;; Fallback
    (t (format nil "~A" response))))

(defun llm-query (prompt &key provider model temperature max-tokens context)
  "High-level interface for LLM queries"
  (let* ((full-prompt (if context
                         (format nil "Context: ~A~%~%Query: ~A" context prompt)
                         prompt))
         (_ (cprin1 40 "Prompt: " full-prompt "~%"))
         (response (eurisko-llm-api-call full-prompt 
                                        :provider provider 
                                        :model model 
                                        :temperature temperature 
                                        :max-tokens max-tokens)))
    
    (when response
      (let ((text (extract-llm-response response (or provider *llm-provider*))))
        ;; Ensure we always return a string
        (unless (stringp text)
          (setf text (format nil "~A" text)))
        (cprin1 40 "Response: " text)
        text))))

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
