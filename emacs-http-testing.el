;; Remember to M-x package-install request

(request-response-data
 (request "http://127.0.0.1:5000"
   :params '(("key" . "value") ("key2" . "value2"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "I sent: %S" (assoc-default 'args data))))))
