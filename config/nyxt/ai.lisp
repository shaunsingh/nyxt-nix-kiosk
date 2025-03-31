(in-package #:nyxt-user)

;;; SET OPENAI KEY on Env OPENAI_KEY

(defvar *model-host* "https://api.openai.com/v1/chat/completions")
(defvar *model* "gpt-4o-mini")
(defvar *available-functions* (make-hash-table :test 'equal))

(defstruct ai-function
  name
  description
  parameters
  func)

(defun register-function (name description parameters fn)
  (format t "Registering ~A ~A~%" name fn)
  (setf (gethash name *available-functions*)
        (make-ai-function
         :name name
         :description description
         :parameters parameters
	 :func fn)))

(defun lisp-to-json-string (data)
  (with-output-to-string (s)
    (json:encode-json data s)))

(defun substitute-subseq (string old new &key (test #'eql))
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))

(defun escape-json (str)
  (with-output-to-string (out)
    (loop for ch across str do
         (if (char= ch #\")
             (write-string "\\\"" out)
             (write-char ch out)))))

(defun handle-function-call (function-call)
  (format t "~% ** handle-function-call (DUMMY) fucntion-call: ~A~%" function-call)
  (let* ((name (cdr (assoc :name function-call)))
         (args-string (cdr (assoc :arguments function-call)))
         (args (and args-string (cl-json:decode-json-from-string args-string)))
         (func (ai-function-func (gethash name *available-functions*))))
    (format t "~% handle-function-call name: ~A" name)
    (format t "~% handle-function-call args-string: ~A" args-string)
    (format t "~% handle-function-call args: ~A" args)
    (format t "~% handle-function-call func: ~A" func)
    (if (not (null func))
	(let ()
          (format t "~%Calling function ~a called with args: ~a~%" name args)
	  (let ((f-val (apply func (mapcar #'cdr args))))
	    (format t "~%Return value from func ~A is ~A~%" name f-val)
	    f-val))
        (error "Unknown function: ~a" name))))

(defun ai-helper (curl-command)
  (terpri)
  (princ curl-command)
  (terpri)
  (let ((response (uiop:run-program curl-command
                                    :output :string
                                    :error-output :string)))
    (terpri)
    (princ response)
    (terpri)
    (with-input-from-string (s response)
      (let* ((json-as-list (json:decode-json s))
             (choices (cdr (assoc :choices json-as-list)))
             (first-choice (car choices))
             (message (cdr (assoc :message first-choice)))
             (function-call (cdr (assoc :function--call message)))
             (content (cdr (assoc :content message))))
	(format t "~% json-as-list: ~A~%" json-as-list)
	(format t "~% choices: ~A~%" choices)
	(format t "~% first-choice: ~A~%" first-choice)
	(format t "~% message: ~A~%" message)
	(format t "~% function-call: ~A~%" function-call)
	(format t "~% content: ~A~%" content)
        (if function-call
            (handle-function-call function-call)
            (or content "No response content"))))))

(defun completions (starter-text max-tokens &optional functions)
  (unless (numberp max-tokens)
    (error "max-tokens must be a number, got: ~a" max-tokens))
  (let* ((function-defs (when functions
                          (mapcar (lambda (f)
                                    (let ((func (gethash f *available-functions*)))
                                      (list (cons :name (ai-function-name func))
                                            (cons :description (ai-function-description func))
                                            (cons :parameters (ai-function-parameters func)))))
                                  functions)))
         (message (list (cons :role "user")
                        (cons :content starter-text)))
         (base-data `((model . ,*model*)
                      (messages . ,(list message))
                      (max_tokens . ,max-tokens)))
         (data (if function-defs
                   (append base-data (list (cons :functions function-defs)))
                   base-data))
         (request-body (cl-json:encode-json-to-string data))
         (fixed-json-data (substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (escape-json fixed-json-data))
         (curl-command
          (format nil "curl ~A -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d \"~A\""
                  *model-host*
                  (uiop:getenv "OPENAI_KEY")
                  escaped-json)))
    (ai-helper curl-command)))

;; util

(defun summarize (some-text max-tokens)
  (let ((curl-command
         (concatenate 'string
                      "curl " *model-host*
                      " -H \"Content-Type: application/json\""
                      " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" "
                      " -d '{\"messages\": [{\"role\": \"user\", \"content\": \"Summarize: " some-text
                      "\"}], \"model\": " *model* ", \"max_tokens\": " (write-to-string max-tokens) "}'")))
    (ai-helper curl-command)))

(defun answer-question (question-text max-tokens)
  (completions question-text max-tokens))

(defun embeddings (text)
  "Get embeddings using text-embedding-3-small model (1536 dimensions)"
  (let* ((curl-command
          (concatenate 'string
                       "curl https://api.openai.com/v1/embeddings "
                       " -H \"Content-Type: application/json\""
                       " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" "
                       " -d '{\"input\": \"" text
                       "\", \"model\": \"text-embedding-3-small\"}'"))
         (response (uiop:run-program curl-command :output :string)))
    (with-input-from-string (s response)
      (let ((json-as-list (json:decode-json s)))
        (cdr (nth 2 (cadr (cadr json-as-list))))))))

;;; Sample registrations for functions used in tool calling
(defun get_weather (location)
  (if (equal location "New York")
      77.0
      65.0))

(register-function
 "get_weather"
 "Get current weather for a location"
 (list (cons :type "object")
       (cons :properties (list (cons :location (list (cons :type "string")
                                                     (cons :description "The city name")))))
       (cons :required '("location")))
 #'nyxt-user::get_weather)

;; function-call looks like: ((:name . "get_weather") (:arguments . "{\"location\":\"New York\"}"))
;; ex calls to cumpetions/summarize/question:
;; (print (completions "The President went to Congress" 20))
;; (print (summarize "Jupiter is the fifth planet from the Sun..." 30))
;; (print (answer-question "Where were the 1992 Olympics held?" 60))
;; (print (answer-question "Where is the Valley of Kings?" 60))
;; (print (answer-question "Mary is 30 years old and Bob is 25. Who is older?" 60))
;; (print (completions "Use function calling for: What's the weather like in New York?" 100 '("get_weather" "calculate")))

;;; COMMANDS

;; (defun extract-content-from-response (response-string)
;;   "Extract the content field from the OpenAI API JSON response.
;;    Handles both JSON responses and plain text responses."
;;   (handler-case
;;       ;; First try to parse as JSON
;;       (with-input-from-string (s response-string)
;;         (let* ((json-as-list (json:decode-json s))
;;                (choices (cdr (assoc :choices json-as-list)))
;;                (first-choice (car choices))
;;                (message (cdr (assoc :message first-choice)))
;;                (content (cdr (assoc :content message))))
;;           (or content "No response content found in JSON")))
;;     ;; If JSON parsing fails, check if it's plain text
;;     (error (e)
;;       (format t "~%JSON parsing error: ~A~%" e)
;;       (format t "~%Response might be plain text: ~A~%" response-string)
;;       ;; If response starts with expected plain text, return it directly
;;       (if (and (stringp response-string)
;;                (> (length response-string) 0)
;;                (or (char= (char response-string 0) #\I)  ; "I'm..."
;;                    (char= (char response-string 0) #\T)  ; "The..."
;;                    (alpha-char-p (char response-string 0))))
;;           response-string
;;           ;; Otherwise return error info
;;           (format nil "Error extracting content: ~A. Raw response: ~A" 
;;                   e (if (> (length response-string) 100)
;;                         (concatenate 'string (subseq response-string 0 100) "...")
;;                         response-string))))))

(define-command-global ai-ask-question ()
  "Ask a question to the AI assistant and display the answer."
  (let* ((question (first (prompt
                          :prompt "Ask AI: "
                          :sources (make-instance 'prompter:raw-source))))
         (response (answer-question question 200)))
    (echo "~A" response)))

;; TODO
(define-panel-command-global ai-summarize-selection (&key (selection (nyxt-user::ffi-buffer-copy (current-buffer))))
    (panel "*AI Summary*" :right)
  "Summarize the selected text in a panel."
  (when (string= selection "")
    (error "No text selected"))
  (run-thread "AI Summary"
    (let* ((summary-text (summarize selection 100))
           (html (format nil "<h2>AI Summary</h2><div>~A</div>" summary-text)))
      (nyxt-user::ffi-buffer-evaluate-javascript
       panel
       (ps:ps
         (setf (ps:@ document body |innerHTML|) (ps:lisp html))))))
  "Summarizing...")

(define-panel-command-global ai-expand-selection (&key (selection (nyxt-user::ffi-buffer-copy (current-buffer))))
    (panel "*AI Expanded*" :right)
  "Expand the selected text with AI."
  (when (string= selection "")
    (error "No text selected"))
  (run-thread "AI Expansion"
    (let* ((prompt (format nil "Expand on this text: ~A" selection))
           (expanded-text (completions prompt 50))
           (html (format nil "<h2>AI Expanded</h2><div>~A</div>" expanded-text)))
      (nyxt-user::ffi-buffer-evaluate-javascript
       panel
       (ps:ps
         (setf (ps:@ document body |innerHTML|) (ps:lisp html))))))
  "Expanding...")

(define-panel-command-global ai-generate-content ()
    (panel "*AI Content*" :right)
  "Generate content based on a prompt."
  (let ((prompt (first (prompt
                        :prompt "Generate: "
                        :sources (make-instance 'prompter:raw-source)))))
    (run-thread "AI Gen"
      (let* ((generated-text (completions prompt 300))
             (html (format nil "<h2>AI Content</h2><div>~A</div>" generated-text)))
        (nyxt-user::ffi-buffer-evaluate-javascript
         panel
         (ps:ps
           (setf (ps:@ document body |innerHTML|) (ps:lisp html))))))
    "Generating..."))

;; TODO
(define-panel-command-global ai-analyze-selection (&key (selection (nyxt-user::ffi-buffer-copy (current-buffer))))
    (panel "*AI Analysis*" :bottom)
  "Analyze the selected text."
  (when (string= selection "")
    (error "No text selected"))
  (run-thread "AI Analysis"
    (let* ((prompt (format nil "Analyze this text: ~A" selection))
           (analysis (completions prompt 150))
           (html (format nil "<h2>AI Analysis</h2><div>~A</div>" analysis)))
      (nyxt-user::ffi-buffer-evaluate-javascript
       panel
       (ps:ps
         (setf (ps:@ document body |innerHTML|) (ps:lisp html))))))
  "Analyzing...")

(define-panel-command-global ai-translate-selection (&key (selection (nyxt-user::ffi-buffer-copy (current-buffer))))
    (panel "*AI Translation*" :right)
  "Translate selected text using AI."
  (when (string= selection "")
    (error "No text selected"))
  (let* ((target-language (first (prompt
                                  :prompt "Translate to:"
                                  :sources (make-instance
                                           'prompter:source
                                           :name "Languages"
                                           :constructor '("English" "Spanish" "French" "German" "Chinese" "Japanese"))))))
    (run-thread "AI Translation"
      (let* ((prompt (format nil "Translate to ~A: ~A" target-language selection))
             (translation (completions prompt 150))
             (html (format nil "<h2>~A Translation</h2><div>~A</div>" target-language translation)))
        (nyxt-user::ffi-buffer-evaluate-javascript
         panel
         (ps:ps
           (setf (ps:@ document body |innerHTML|) (ps:lisp html)))))))
  "Translating...")

;; TODO
(define-panel-command-global ai-explain-code-selection (&key (selection (nyxt-user::ffi-buffer-copy (current-buffer))))
    (panel "*Code Explanation*" :bottom)
  "Explain selected code using AI."
  (when (string= selection "")
    (error "No code selected"))
  (run-thread "Code Explanation"
    (let* ((prompt (format nil "Explain this code: ~A" selection))
           (explanation (completions prompt 200))
           (html (format nil "<h2>Code Explanation</h2><div>~A</div>" explanation)))
      (nyxt-user::ffi-buffer-evaluate-javascript
       panel
       (ps:ps
         (setf (ps:@ document body |innerHTML|) (ps:lisp html))))))
  "Explaining code...")
