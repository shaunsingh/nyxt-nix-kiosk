(in-package #:nyxt-user)

;;; SET OPENAI KEY on Env OPENAI_KEY

(defvar *model-host* "https://api.openai.com/v1/chat/completions")
(defvar *model* "gpt-4o-mini")
(defvar *available-functions* (make-hash-table :test 'equal))

(defun dot-product-recursive (a b)
  "Calculate dot product recursively"
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product-recursive (rest a) (rest b)))))

(defun dot-product (list1 list2)
  "Calculate dot product iteratively"
  (let ((sum 0))
    (loop for x in list1
          for y in list2
          do (setf sum (+ sum (* x y))))
    sum))

(defun get-api-key ()
  "Retrieve the OpenAI API key from the environment variable."
  (or (uiop:getenv "OPENAI_KEY")
      (error "API key not found. Please set OPENAI_KEY in your environment.")))

(defun lisp-to-json-string (data)
  "Convert Lisp data to a JSON string."
  (with-output-to-string (s)
    (json:encode-json data s)))

(defun substitute-subseq (string old new &key (test #'eql))
  "Substitute the first occurrence of OLD in STRING with NEW using TEST."
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))

(defun escape-json (str)
  "Escape quotes in a JSON string."
  (with-output-to-string (out)
    (loop for ch across str do
         (if (char= ch #\")
             (write-string "\\\"" out)
             (write-char ch out)))))

(defstruct ai-function
  name
  description
  parameters
  func)

(defun register-function (name description parameters fn)
  "Register a function for AI function calling."
  (format t "Registering ~A ~A~%" name fn)
  (setf (gethash name *available-functions*)
        (make-ai-function
         :name name
         :description description
         :parameters parameters
         :func fn)))

(defun handle-function-call (function-call)
  "Process a function call from the API response."
  (format t "~% ** handle-function-call (DUMMY) function-call: ~A~%" function-call)
  (let* ((name (cdr (assoc :name function-call)))
         (args-string (cdr (assoc :arguments function-call)))
         (args (and args-string (cl-json:decode-json-from-string args-string)))
         (func (ai-function-func (gethash name *available-functions*))))
    (format t "~% handle-function-call name: ~A" name)
    (format t "~% handle-function-call args-string: ~A" args-string)
    (format t "~% handle-function-call args: ~A" args)
    (format t "~% handle-function-call func: ~A" func)
    (if func
        (let ((result (apply func (mapcar #'cdr args))))
          (format t "~%Return value from func ~A is ~A~%" name result)
          result)
        (error "Unknown function: ~a" name))))

(defun build-curl-command (json-data)
  "Build a curl command string from JSON-DATA."
  (let* ((request-body (cl-json:encode-json-to-string json-data))
         (fixed-json (substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (escape-json fixed-json)))
    (format nil "curl ~A -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d \"~A\""
            *model-host* (get-api-key) escaped-json)))

(defun ai-helper (curl-command)
  "Run the curl command and process the API response."
  (format t "~%Executing: ~A~%" curl-command)
  (let ((response (uiop:run-program curl-command :output :string :error-output :string)))
    (format t "~%Response: ~A~%" response)
    (with-input-from-string (s response)
      (let* ((json-as-list (json:decode-json s))
             (choices (cdr (assoc :choices json-as-list)))
             (first-choice (car choices))
             (message (cdr (assoc :message first-choice)))
             (function-call (cdr (assoc :function--call message)))
             (content (cdr (assoc :content message))))
        (format t "~%Parsed JSON: ~A~%" json-as-list)
        (if function-call
            (handle-function-call function-call)
            (or content "No response content"))))))

(defun completions (starter-text max-tokens &optional functions)
  "Send a completion request with STARTER-TEXT and optional FUNCTIONS.
MAX-TOKENS must be a number."
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
         (curl-command (build-curl-command data)))
    (ai-helper curl-command)))

(defun summarize (some-text max-tokens)
  "Summarize SOME-TEXT with a given MAX-TOKENS limit."
  (let* ((json-data `((model . ,*model*)
                      (messages . ,(list `((role . "user")
                                            (content . ,(format nil "Summarize: ~A" some-text)))))
                      (max_tokens . ,max-tokens)))
         (curl-command (build-curl-command json-data)))
    (ai-helper curl-command)))

(defun embeddings (text)
  "Get embeddings using the text-embedding-3-small model."
  (let* ((curl-command
          (format nil "curl https://api.openai.com/v1/embeddings -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d '{\"input\": \"~A\", \"model\": \"text-embedding-3-small\"}'"
                  (get-api-key) text))
         (response (uiop:run-program curl-command :output :string)))
    (with-input-from-string (s response)
      (let ((json-as-list (json:decode-json s)))
        (cdr (nth 2 (cadr (cadr json-as-list))))))))

(defun get_weather (location)
  "Return a dummy weather value for the given LOCATION."
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

;; GROQ

(defun groq-completion (content)
  (let* ((url "https://api.groq.com/openai/v1/chat/completions")
         (groq-api-key (uiop:getenv "GROQ_API_KEY"))
         (headers `(("Authorization" . ,(concatenate 'string "Bearer " groq-api-key))
                    ("Content-Type" . "application/json")))
         (data `(("model" . "meta-llama/llama-4-scout-17b-16e-instruct")
		 ("messages" . ((("role" . "system")
				 ("content" . "content"))
                                (("role" . "user")
				 ("content" . ,content))))))
         (json-data (cl-json:encode-json-to-string data)))
    (cl-json:decode-json-from-string
     (flexi-streams:octets-to-string
      (drakma:http-request url
                           :method :post
                           :content-type "application/json"
                           :additional-headers headers
                           :content json-data)))))

(defun groq-extract-content (resp)
  (cdr (nth 2 (cadr (cadr (assoc :choices resp))))))

;; IMPLEMENT

(define-command-global ai-ask-question ()
  "Ask a question to the AI assistant and display the answer."
  (let* ((question (first (prompt :prompt "Ask AI: " :sources (make-instance 'prompter:raw-source))))
         (response (completions question 200)))
    (echo "~A" response)))

(define-panel-command-global ai-summarize-selection (&key (selection (nyxt-user::ffi-buffer-copy (current-buffer))))
    (panel "*AI Summary*" :right)
  "Summarize the selected text in a panel."
  (when (string= selection "")
    (error "No text selected"))
  (run-thread "AI Summary"
    (let ((summary-text (summarize selection 100))
          (html (format nil "<h2>AI Summary</h2><div>~A</div>" summary-text)))
      (nyxt-user::ffi-buffer-evaluate-javascript panel
         (ps:ps (setf (ps:@ document body |innerHTML|) (ps:lisp html))))))
  "Summarizing...")

(define-panel-command-global ai-expand-selection (&key (selection (nyxt-user::ffi-buffer-copy (current-buffer))))
    (panel "*AI Expanded*" :right)
  "Expand the selected text with AI."
  (when (string= selection "")
    (error "No text selected"))
  (run-thread "AI Expansion"
    (let* ((prompt (format nil "Expand on this text: ~A" selection))
           (expanded-text (completions prompt 200))
           (html (format nil "<h2>AI Expanded</h2><div>~A</div>" expanded-text)))
      (nyxt-user::ffi-buffer-evaluate-javascript panel
        (ps:ps (setf (ps:@ document body |innerHTML|) (ps:lisp html))))))
  "Expanding...")

(define-panel-command-global ai-generate-content ()
    (panel "*AI Content*" :right)
  "Generate content based on a prompt."
  (let ((prompt (first (prompt :prompt "Generate: " :sources (make-instance 'prompter:raw-source)))))
    (run-thread "AI Gen"
      (let* ((generated-text (completions prompt 300))
             (html (format nil "<h2>AI Content</h2><div>~A</div>" generated-text)))
        (nyxt-user::ffi-buffer-evaluate-javascript panel
          (ps:ps (setf (ps:@ document body |innerHTML|) (ps:lisp html))))))
    "Generating..."))

(define-panel-command-global ai-analyze-selection (&key (selection (nyxt-user::ffi-buffer-copy (current-buffer))))
    (panel "*AI Analysis*" :bottom)
  "Analyze the selected text."
  (when (string= selection "")
    (error "No text selected"))
  (run-thread "AI Analysis"
    (let* ((prompt (format nil "Analyze this text: ~A" selection))
           (analysis (completions prompt 150))
           (html (format nil "<h2>AI Analysis</h2><div>~A</div>" analysis)))
      (nyxt-user::ffi-buffer-evaluate-javascript panel
         (ps:ps (setf (ps:@ document body |innerHTML|) (ps:lisp html))))))
  "Analyzing...")

(define-panel-command-global ai-translate-selection (&key (selection (nyxt-user::ffi-buffer-copy (current-buffer))))
    (panel "*AI Translation*" :right)
  "Translate selected text using AI."
  (when (string= selection "")
    (error "No text selected"))
  (let ((target-language (first (prompt :prompt "Translate to:" 
                                        :sources (make-instance 'prompter:source
                                                                 :name "Languages"
                                                                 :constructor '("English" "Spanish" "French" "German" "Chinese" "Japanese"))))))
    (run-thread "AI Translation"
      (let* ((prompt (format nil "Translate to ~A: ~A" target-language selection))
             (translation (completions prompt 150))
             (html (format nil "<h2>~A Translation</h2><div>~A</div>" target-language translation)))
        (nyxt-user::ffi-buffer-evaluate-javascript panel
          (ps:ps (setf (ps:@ document body |innerHTML|) (ps:lisp html)))))))
  "Translating...")

(define-panel-command-global ai-explain-code-selection (&key (selection (nyxt-user::ffi-buffer-copy (current-buffer))))
    (panel "*Code Explanation*" :bottom)
  "Explain selected code using AI."
  (when (string= selection "")
    (error "No code selected"))
  (run-thread "Code Explanation"
    (let* ((prompt (format nil "Explain this code: ~A" selection))
           (explanation (completions prompt 200))
           (html (format nil "<h2>Code Explanation</h2><div>~A</div>" explanation)))
      (nyxt-user::ffi-buffer-evaluate-javascript panel
         (ps:ps (setf (ps:@ document body |innerHTML|) (ps:lisp html))))))
  "Explaining code...")
