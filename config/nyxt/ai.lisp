(in-package #:nyxt-user)

(defvar *openai-model-host* "https://api.openai.com/v1/chat/completions")
(defvar *openai-model* "gpt-4o-mini")

(defvar *groq-model* "meta-llama/llama-4-scout-17b-16e-instruct") ;; yappatron 2000
;; (defvar *groq-model* "llama-3.1-8b-instant")
;; (defvar *groq-model* "qwen-2.5-32b")

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

(defstruct openai-function
  name
  description
  parameters
  func)

(defun openai-register-function (name description parameters fn)
  "Register a function for AI function calling."
  (format t "Registering ~A ~A~%" name fn)
  (setf (gethash name *available-functions*)
        (make-openai-function
         :name name
         :description description
         :parameters parameters
         :func fn)))

(defun openai-handle-function-call (function-call)
  "Process a function call from the API response."
  (format t "~% ** openai-handle-function-call (DUMMY) function-call: ~A~%" function-call)
  (let* ((name (cdr (assoc :name function-call)))
         (args-string (cdr (assoc :arguments function-call)))
         (args (and args-string (cl-json:decode-json-from-string args-string)))
         (func (openai-function-func (gethash name *available-functions*))))
    (format t "~% openai-handle-function-call name: ~A" name)
    (format t "~% openai-handle-function-call args-string: ~A" args-string)
    (format t "~% openai-handle-function-call args: ~A" args)
    (format t "~% openai-handle-function-call func: ~A" func)
    (if func
        (let ((result (apply func (mapcar #'cdr args))))
          (format t "~%Return value from func ~A is ~A~%" name result)
          result)
        (error "Unknown function: ~a" name))))

(defun openai-build-curl-command (json-data)
  "Build a curl command string from JSON-DATA."
  (let* ((request-body (cl-json:encode-json-to-string json-data))
         (fixed-json (substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (escape-json fixed-json)))
    (format nil "curl ~A -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d \"~A\""
            *openai-model-host* *openai-api-key* escaped-json)))

(defun openai-helper (curl-command)
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
            (openai-handle-function-call function-call)
            (or content "No response content"))))))

(defun openai-completions (starter-text max-tokens &optional functions)
  "Send a completion request with STARTER-TEXT and optional FUNCTIONS.
MAX-TOKENS must be a number."
  (unless (numberp max-tokens)
    (error "max-tokens must be a number, got: ~a" max-tokens))
  (let* ((function-defs (when functions
                          (mapcar (lambda (f)
                                    (let ((func (gethash f *available-functions*)))
                                      (list (cons :name (openai-function-name func))
                                            (cons :description (openai-function-description func))
                                            (cons :parameters (openai-function-parameters func)))))
                                  functions)))
         (message (list (cons :role "user")
                        (cons :content starter-text)))
         (base-data `((model . ,*openai-model*)
                      (messages . ,(list message))
                      (max_tokens . ,max-tokens)))
         (data (if function-defs
                   (append base-data (list (cons :functions function-defs)))
                   base-data))
         (curl-command (openai-build-curl-command data)))
    (openai-helper curl-command)))

(defun openai-summarize (some-text max-tokens)
  "Summarize SOME-TEXT with a given MAX-TOKENS limit."
  (let* ((json-data `((model . ,*openai-model*)
                      (messages . ,(list `((role . "user")
                                            (content . ,(format nil "Summarize: ~A" some-text)))))
                      (max_tokens . ,max-tokens)))
         (curl-command (openai-build-curl-command json-data)))
    (openai-helper curl-command)))

(defun openai-embeddings (text)
  "Get embeddings using the text-embedding-3-small model."
  (let* ((curl-command
          (format nil "curl https://api.openai.com/v1/embeddings -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d '{\"input\": \"~A\", \"model\": \"text-embedding-3-small\"}'"
                  *openai-api-key* text))
         (response (uiop:run-program curl-command :output :string)))
    (with-input-from-string (s response)
      (let ((json-as-list (json:decode-json s)))
        (cdr (nth 2 (cadr (cadr json-as-list))))))))

(defun get_weather (location)
  "Return a dummy weather value for the given LOCATION."
  (if (equal location "New York")
      77.0
      65.0))

(openai-register-function
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
         (headers `(("Authorization" . ,(concatenate 'string "Bearer " *groq-api-key*))
                    ("Content-Type" . "application/json")))
         (data `(("model" . ,*groq-model*)
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

(defun choose-ai-api ()
  "Prompt the user to choose which AI API to use: OpenAI or Groq."
  (first (prompt :prompt "Choose AI API (OpenAI/Groq): "
                 :sources (make-instance 'prompter:source
                                         :name "AI API"
                                         :constructor '("OpenAI" "Groq")))))

(define-command-global ai-ask-question ()
  "Ask a question to the AI assistant and display the answer."
  (let* ((question (first (prompt :prompt "Ask AI: " 
				  :sources (make-instance 'prompter:raw-source))))
         (api (choose-ai-api))
         (result (if (string= api "Groq")
                     (groq-extract-content (groq-completion question))
                     (openai-completions question 100))))
    (echo "~A" result)))

(define-internal-page ai-summarize-buffer (&key (id (id (current-buffer))))
    (:title "*AI Summary*")
  "Summarize the current buffer by creating a new summary buffer with AI. 
ID is a buffer `id'."
  (let ((buffer (nyxt::buffers-get id)))
    (let* ((api (choose-ai-api))
	   (contents
              (serapeum:string-join
               (map 'list (lambda (e) (plump:text e))
                    (clss:select "p" (document-model buffer)))
               " "))
           (result (if (string= api "Groq")
                       (groq-extract-content (groq-completion (format nil "Do not include 
any extra text or markdown formatting. Summarize: ~A" contents)))
                       (openai-summarize contents 200))))
      (spinneret:with-html-string
        (:h1 "Summary for: " (title buffer))
        (:p (:raw result))))))
 
(define-command-global ai-summarize-buffer (&key (buffer (current-buffer)))
  "Summarize the current buffer with AI by creating a new summary buffer."
  (buffer-load-internal-page-focus 'ai-summarize-buffer :id (id buffer)))
