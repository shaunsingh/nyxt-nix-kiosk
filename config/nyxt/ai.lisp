(in-package #:nyxt-user)

;; keys

(defvar *openai-model-host* "https://api.openai.com/v1/chat/completions")
(defvar *openai-model* "gpt-4o-mini")

(defvar *groq-model* "meta-llama/llama-4-scout-17b-16e-instruct") ;; yappatron 2000

(defvar *available-functions* (make-hash-table :test 'equal))

;; util

(defun dot-product-recursive (a b)
  "Calculate dot product recursively."
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product-recursive (rest a) (rest b)))))

(defun dot-product (list1 list2)
  "Calculate dot product iteratively."
  (let ((sum 0))
    (loop for x in list1
          for y in list2
          do (setf sum (+ sum (* x y))))
    sum))

;; json

(defun substitute-subseq (string old new &key (test #'eql))
  "Substitute the first occurrence of OLD in STRING with NEW using TEST."
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))

(defun safe-parse-json (json-string)
  "Safely parse a JSON string into Lisp data. Return NIL if parsing fails."
  (handler-case
      (cl-json:decode-json-from-string json-string)
    (error (err)
      (format *error-output* "~%JSON parse error: ~A~%" err)
      nil)))

(defun json-to-html (json)
  "Convert structured JSON summary into an inner HTML string using spinneret."
  (let ((title (cdr (assoc :title json)))
        (sections (cdr (assoc :sections json)))
        (html-parts '()))
    (push (spinneret:with-html-string (:h1 title)) html-parts)
    (dolist (section sections)
      (let ((heading (cdr (assoc :heading section)))
            (content (cdr (assoc :content section)))
            (items (cdr (assoc :list section))))
        (when heading
          (push (spinneret:with-html-string (:h2 heading)) html-parts))
        (when content
          (push (spinneret:with-html-string (:p content)) html-parts))
        (when items
          (let ((list-html
                  (spinneret:with-html-string
                    (:ul
                      (dolist (item items)
                        (:li item))))))
            (push list-html html-parts)))))
    (apply #'concatenate 'string (nreverse html-parts))))

(defun escape-json (str)
  "Escape quotes in a JSON string."
  (with-output-to-string (out)
    (loop for ch across str do
          (if (char= ch #\")
              (write-string "\\\"" out)
              (write-char ch out)))))

(define-internal-page-command-global parse-json-test-page ()
    (buffer "*JSON TEST*")
  "Internal page to test json schema parsing & rendering"
  (let* ((json-string "
{
  \"title\": \"test1\",
  \"sections\": [
    {\"heading\": \"test1 heading\", \"content\": \"abasdflkjh\"},
    {\"heading\": \"test2 list\", \"list\": [\"asdkfljh0\", \"asdlfkjh1\"] }
  ]
}
    ")
         (parsed-json (safe-parse-json json-string))
         (inner-html (json-to-html parsed-json)))
    (spinneret:with-html-string
      (:div (:raw inner-html)))))

;; openai

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
  "Build a curl command string from JSON-DATA for OpenAI requests."
  (let* ((request-body (cl-json:encode-json-to-string json-data))
         (fixed-json (substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (escape-json fixed-json)))
    (format nil "curl ~A -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d \"~A\""
            *openai-model-host* *openai-api-key* escaped-json)))

(defun openai-helper (curl-command)
  "Run the curl command and process the API response from OpenAI."
  (format t "~%Executing: ~A~%" curl-command)
  (let ((response (uiop:run-program curl-command :output :string :error-output :string)))
    (format t "~%Response: ~A~%" response)
    (with-input-from-string (s response)
      (let* ((json-as-list (json:decode-json s))
             (choices (cdr (assoc :choices json-as-list)))
             (first-choice (first choices))
             (message (cdr (assoc :message first-choice)))
             (function-call (cdr (assoc :function--call message)))
             (content (cdr (assoc :content message))))
        (format t "~%Parsed JSON: ~A~%" json-as-list)
        (if function-call
            (openai-handle-function-call function-call)
            (or content "No response content"))))))

(defun openai-completions (starter-text max-tokens &optional functions)
  "Send a completion request with STARTER-TEXT and optional FUNCTIONS using OpenAI.
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
  "Summarize SOME-TEXT using OpenAI with a given MAX-TOKENS limit."
  (let* ((json-data `((model . ,*openai-model*)
                      (messages . ,(list `((role . "user")
                                            (content . ,(format nil "Summarize: ~A" some-text)))))
                      (max_tokens . ,max-tokens)))
         (curl-command (openai-build-curl-command json-data)))
    (openai-helper curl-command)))

(defun openai-embeddings (text)
  "Get embeddings using the text-embedding-3-small model from OpenAI."
  (let* ((curl-command
          (format nil "curl https://api.openai.com/v1/embeddings -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d '{\"input\": \"~A\", \"model\": \"text-embedding-3-small\"}'"
                  *openai-api-key* text))
         (response (uiop:run-program curl-command :output :string)))
    (with-input-from-string (s response)
      (let ((json-as-list (json:decode-json s)))
        (cdr (nth 2 (cadr (cadr json-as-list))))))))

;; groq

(defun groq-send-json-request (json-data)
  "Send a JSON request to Groq using the provided JSON-DATA.
Returns the decoded JSON response."
  (let* ((url "https://api.groq.com/openai/v1/chat/completions")
         (headers `(("Authorization" . ,(concatenate 'string "Bearer " *groq-api-key*))
                    ("Content-Type" . "application/json")))
         (content json-data))
    (cl-json:decode-json-from-string
     (flexi-streams:octets-to-string
      (drakma:http-request url
                           :method :post
                           :content-type "application/json"
                           :additional-headers headers
                           :content content)))))

(defun groq-extract-content (resp)
  "Extract the text content from Groq's API response.
Assumes RESP is a parsed JSON association list."
  (let ((choices (cdr (assoc :choices resp))))
    (when choices
      (let ((first-choice (first choices)))
        (let ((message (cdr (assoc :message first-choice))))
          (when message
            (cdr (assoc :content message))))))))

(defun groq-completion-internal (content extra-params)
  "Internal Groq completion function.
CONTENT is the prompt.
EXTRA-PARAMS is an alist of additional parameters to merge into the request."
  (let* ((base-data `(("model" . ,*groq-model*)
                      ("messages" .
                       ((("role" . "system")
                         ("content" . "content"))
                        (("role" . "user")
                         ("content" . ,content))))))
         (data (if extra-params
                   (append base-data extra-params)
                   base-data))
         (json-data (cl-json:encode-json-to-string data)))
    (groq-send-json-request json-data)))

(defun groq-completion (content)
  "Send a Groq completion request using the default mode."
  (groq-completion-internal content nil))

(defun groq-json-completion (content)
  "Send a Groq completion request that forces valid JSON output.
This function forces the use of JSON mode via response_format."
  (groq-completion-internal content
    `(("response_format" . (("type" . "json_object"))))))

;; impl

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
  "Summarize the current buffer by creating a new summary buffer with AI using Groq."
  (let ((buffer (nyxt::buffers-get id)))
    (let* ((contents (serapeum:string-join
                      (map 'list (lambda (e) (plump:text e))
                           (clss:select "p" (document-model buffer)))
                      " "))
           (prompt (format nil
"Summarize the following content. Summary should be concise, formal, and objective. 
Use as many headings & lists as needed. The JSON must conform to schema:
{
  \"title\": \"...\",
  \"sections\": [
    {\"heading\": \"...\", \"content\": \"...\"},
    {\"heading\": \"...\", \"list\": [\"...\", \"...\"] }
  ]
}
Content:\n~A" contents))
           (raw-response (groq-json-completion prompt))
           (json-string (groq-extract-content raw-response))
           (parsed-json (safe-parse-json json-string))
           (inner-html (json-to-html parsed-json)))
      (spinneret:with-html-string
        (:div (:raw inner-html))))))

(define-command-global ai-summarize-buffer (&key (buffer (current-buffer)))
  "Summarize the current buffer with AI by creating a new summary buffer."
  (buffer-load-internal-page-focus 'ai-summarize-buffer :id (id buffer)))
