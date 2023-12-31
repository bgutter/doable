;;
;; doable.lisp
;;

;; load tiny-routes

(in-package :cl-user)

(defpackage :doable
  (:use :common-lisp)
  (:export
   #:api-start-or-restart
   #:api-stop))

(in-package :doable)

;; Instance IDing

(defclass id-mixin ()
  ((id
    :initform (next-id)
    :accessor id))
  (:documentation "Unique ID for an instance within Doable."))

(defvar id-counter 0)
(defun next-id ()
  (let
      ((ret id-counter))
    (setf id-counter (+ id-counter 1))
    ret))

;; Task Status Representation

(defclass status ()
  ((status-symbol
    :initarg :symbol
    :accessor status-symbol)
   (is-final
    :initarg :final
    :initform nil))
  (:documentation "Represents the status of a task."))

(defvar *task-status-registry* (list)
  "Index of all legal task status property objects.")

(defun find-status-properties (status)
  "Look up the properties for a task status symbol."
  (find status
        *task-status-registry*
        :key #'status-symbol))

(defun status-finalp (status)
  "Does this status symbol represent a final status?"
  (slot-value (find-status-properties status) 'is-final))

(defmacro define-status (symbol is-final)
  "Define an individual task status."
  `(let
       ((status (make-instance 'status
                               :symbol ',symbol
                               :final ,is-final)))
     (push status *task-status-registry*)
     status))

(defmacro define-statuses (nonfinal-statuses final-statuses)
  "Batch-define many task status symbols."
  `(progn
     ,@(loop for status in nonfinal-statuses
             collect `(define-status ,status nil))
     ,@(loop for status in final-statuses
             collect `(define-status ,status t))))

(define-statuses
    (unstarted waiting in-progress hiatus)
    (done cancelled))

(defmethod print-object ((obj status) out)
  "Print method for task statuses."
  (print-unreadable-object (obj out)
    (format out "STATUS-PROPS:~A" (status-symbol obj))))

;; Task Representation

(defclass task (id-mixin)
  ((summary
    :documentation "Single line summary."
    :accessor task-summary
    :initarg :summary
    :initform "New Task")
   (created-date
    :initform (local-time:now))
   (due-date
    :accessor when-due
    :initarg :due
    :initform nil)
   (status
    :accessor task-status
    :type 'status
    :initform 'unstarted)
   (prerequisites
    :accessor task-prerequisites
    :initform (list)))
  (:documentation "Represents a single actionable task."))

(defmethod print-object ((task task) out)
  (print-unreadable-object (task out :identity t)
    (format out "~A" (task-summary task))))

(defun task-has-prereqsp (task)
  "Does this task have any prerequisites?"
  (plusp (length (task-prerequisites task))))

(defun task-openp (task)
  "Does this task have a non-final status?"
  (declare (type task task))
  (not (status-finalp (task-status task))))

(defun task-closedp (task)
  "Does this task have a final status?"
  (not (task-openp task)))

(defun task-unblockedp (task)
  "Are all of this task's prerequisites in final states?"
  (or (not (task-has-prereqsp task))
      (loop for before-task in (task-prerequisites task)
            always (and
                    (task-closedp before-task)
                    (task-unblockedp before-task)))))

(defun task-blockedp (task)
  (not (task-unblockedp task)))

;; Task Indexing

(defvar *task-list* (list)
  "In-memory unordered record of all tasks.")

(defun index-task (task)
  "Index a new task"
  (push task *task-list*)
  (persist-tasks)) ;; place this behind some logic down the road

(defun reset-tasks ()
  "Testing and development only. Clear task list."
  (setf *task-list* nil))

(defun persist-tasks (&key (path nil))
  )
  ;; (let
  ;;     ((*print-readably* t)
  ;;      (path (or path "~/.doable/tasks.lisp")))
  ;;   (ensure-directories-exist path)
  ;;   (with-open-file (fout path :direction :output)
  ;;     (loop for task in *task-list*
  ;;           collect
  ;;           (with-slots () task
  ;;             ()))
  ;;     (format fout "~A" *task-list*))))

;; Task Mutation

(defun new-task (&rest initargs)
  "Create a new task."
  (let
      ((task (apply #'make-instance 'task initargs)))
    (index-task task)
    task))

(defun make-prerequisite (blocked-task needed-task)
  "Register `needed-task' as a prerequisite of `blocked-task'."
  (with-slots (prerequisites) blocked-task
    (when (not (member needed-task prerequisites))
      (push needed-task prerequisites))))

;; Task Discovery

(defun sort-tasks-by-due-date (task-list)
  "Given a list of tasks, return them sorted by due date."
  (sort
   (copy-list task-list)
   (lambda (t1 t2)
     (if (and t1 t2)
         (local-time:timestamp<= t1 t2)
         (not (not t1))))
   :key #'when-due))

(defvar *backlog-sort-fn* #'sort-tasks-by-due-date
  "Function used to sort the return list of the `backlog' function.")

(defvar *next-sort-fn* #'sort-tasks-by-due-date
  "Function used to sort the return list of the `next' function.")

(defun backlog ()
  "Get a list of all open tasks, sorted by *backlog-sort-fn*."
  (funcall *backlog-sort-fn*
           (remove-if-not #'task-openp
                          *task-list*)))

(defun next ()
  "Get a list of all open and unblocked tasks, sorted by *next-sort-fn*."
  (funcall *next-sort-fn*
           (remove-if-not (lambda (task)
                            (and
                             (task-openp task)
                             (task-unblockedp task)))
                          *task-list*)))

;; CLI (development use only?)

(defun cli-print-task (task &key (indent 0))
  "Print a single task to a single line."
  (format t "~vA[~A] ~A~%"
          indent
          ""
          (task-status task)
          (task-summary task)))

(defun cli-list-tasks (task-list &key (describe-blockers t) title)
  "Write a task list to STDOUT."
  (terpri)
  (when title
      (format t "~A~%----------~%" title))
  (loop for task in task-list
        do (progn
             (cli-print-task task)
             (when (and describe-blockers (task-blockedp task))
               (format t "  -> Blocked by the following tasks:~%")
               (loop for blocking-task in (task-prerequisites task)
                     when (task-openp blocking-task)
                       do (cli-print-task blocking-task :indent 2))))))

;; !!!
;; DEBUGGING
;; !!!

;; (setq *backlog-sort-fn* #'sort-tasks-by-due-date)

;; (macroexpand-1 (define-statuses (open waiting in-progress hiatus) (done cancelled)))

;; (setf *task-status-registry* (list))

;; (macroexpand '(define-status 'cat t))

;; (status-finalp 'open)

;; (new-task :summary "First task")

;; (backlog)

;; (slot-value (first *task-list*) 'status)

;; *task-status-registry*

;; (slot-value (find-status-properties (slot-value (first *task-list*) 'status)) 'is-final)

;; (task-openp (first *task-list*))

;; (first *task-list*)
;; (task-status (first *task-list*))
;; (find-status-properties (task-status (first *task-list*)))

;; (new-task :summary "Second one, prereq for first")
;; (task-unblockedp (first *task-list*))

;; (setq *next-sort-fn* #'sort-tasks-by-due-date)

;; (new-task :summary "third one!")

;; (make-prerequisite (first *task-list*) (second *task-list*))
;; (make-prerequisite (first *task-list*) (third *task-list*))

;; (cli-list-tasks (backlog) :title "Backlog")
;; (cli-list-tasks (next))

;; (let
;;     ((t1 (new-task :summary "Finish the basement"))
;;      (t2 (new-task :summary "Stop water leaks"))
;;      (t3 (new-task :summary "Re-grade yard"))
;;      (t4 (new-task :summary "Patch concrete blocks in basement")))
;;   (make-prerequisite t1 t2)
;;   (make-prerequisite t2 t3)
;;   (make-prerequisite t2 t4)
;;   (cli-list-tasks (backlog) :title "Backlog")
;;   (cli-list-tasks (next) :title "Next")
;;   (reset-tasks))

(defvar *app* (make-instance 'ningle:<app>))
;; (setf *app* (make-instance 'ningle:<app>))

(defun respond-with-json (thing &key (encode-func #'json:encode-json))

  ;; Set response content-type
  (setf (lack.response:response-headers ningle:*response*)
        (append (lack.response:response-headers ningle:*response*)
                (list :content-type "application/json")))

  ;; Return JSON encoded string
  (let
      ((out-string (make-string-output-stream)))
    (funcall encode-func thing out-string)
    (get-output-stream-string out-string)))

(setq cl-who:*attribute-quote-char* #\") ;; better way to do this?

(defun api-add-route-definitions ()

  (let
      ((cl-who:*attribute-quote-char* #\")) ;; dynamic binding? Need to ,in lambdas below perhaps

    (setf (ningle:route *app* "/")
          #'(lambda (parameters)
              "Just serve the landing page."
              (declare (ignore parameters))
              (generate-spwa)))

  ;; Get task by ID
  (setf (ningle:route *app* "/task/:task-id" :method :GET)
        #'(lambda (params)
            "Retrieve a task by task ID."
            (let ((task (find (parse-integer (cdr (assoc :task-id params)))
                              *task-list*
                              :key #'id)))
              (respond-with-json task))))

  ;; Create task by ID
  (setf (ningle:route *app* "/task" :method :POST)
        #'(lambda (params)
            "Create a task by task ID."
            (let
                ((task (apply #'new-task
                              (alexandria:alist-plist
                               (mapcar #'(lambda (pair)
                                           (cons
                                            (intern (string-upcase (car pair)) "KEYWORD")
                                            (cdr pair)))
                                       (car params))))))
              (respond-with-json (list :id (id task))
               :encode-func #'json:encode-json-plist))))

  ;; Default case
  (setf (ningle:route *app* "/*")
        #'(lambda (params)
            "Handle unknown paths."
            (declare (ignore params))
            (setf (lack.response:response-status ningle:*response*) 404)
            "Object not found!"))))

(defvar *clack-instance* nil)

(defun api-stop ()
  "Stop the REST server, if it is running."
  (when *clack-instance*
    (clack:stop *clack-instance*)
    (setf *clack-instance* nil))
  (values))

(defun api-start-or-restart (&key (reset-routes nil))
  "Start the REST server. Restart it if running."
  (api-stop)
  (when reset-routes
    (ningle:clear-routing-rules *app*)
    (api-add-route-definitions))
  (setf *clack-instance*
        (clack:clackup *app* :server :hunchentoot)))

(defun generate-spwa ()
  (let
      ((x (make-string-output-stream)))
    (who:with-html-output (x nil :prologue t)
      (:html
       (:head
        (:script :type "text/javascript"
         (who:str (generate-js)))
        (:style
         (who:str (generate-style))))
       (:body
        (:div :id "container"
              :style "width: 100%;height: 100%;"
              (:div :id "elem"
                    :onmousedown "beginDivDragging(this,'container',event);"
                    :onmouseup "endDivDragging('container');"
                    :style "width: 200px;height: 100px;"
                    (:div :style "width:100%;height:100%;padding:10px"
                          ))))))
        ;; (:h1 "Doable")
        ;; (:div :id "task-pane")
        ;; (:p "This is some text from Lisp.")
        ;; (loop for task in *task-list*
        ;;       do (who:htm
        ;;           (:li
        ;;            (:button (who:str " X "))
        ;;            (:p (who:str (task-summary task)))
        ;;            (:p (who:str "test")))))
        ;; (:a :href "#"
        ;;     :onclick
        ;;     (ps:ps
        ;;       (ps:chain document
        ;;              (get-element-by-id "task-pane")
        ;;              (append-child
        ;;               (ps:chain document (create-element "li"))))
        ;;       (alert "Hello World"))
        ;;     "Hello World"))))
    (get-output-stream-string x)))

(defun generate-js ()
  (ps:ps

    (defun refresh-tasks ()
      "Development: Fetch a single task"
      (ps:chain
       (fetch "http://127.0.0.1:5000/task/1")
       (then (lambda (response)
               (ps:chain response (json))))
       (then (lambda (json)
               (ps:chain console (log json))))))

    (defun move-div (div new-xpos new-ypos)
      (ps:chain console (log div new-xpos new-ypos))
      (setf (ps:chain div style left) (concatenate 'string (ps:chain new-xpos (to-string)) "px"))
      (setf (ps:chain div style top) (concatenate 'string (ps:chain new-ypos (to-string)) "px"))
      (values))

    (defun begin-div-dragging (divid container-id &optional evt)
      "Modified from https://stackoverflow.com/questions/9334084/moveable-draggable-div"
      (let*
          ((evt (or evt (ps:chain window event)))
           (pos-x (ps:chain evt client-x))
           (pos-y (ps:chain evt client-y))
           (div-top (ps:chain (ps:chain divid style top) (replace "px" "")))
           (div-left (ps:chain (ps:chain divid style left) (replace "px" "")))
           (diff-x (- pos-x div-left))
           (diff-y (- pos-y div-top))
           (e-wi (parse-int (ps:chain divid style width)))
           (e-he (parse-int (ps:chain divid style height)))
           (c-wi (parse-int (ps:chain document (get-element-by-id container-id) offset-width)))
           (c-he (parse-int (ps:chain document (get-element-by-id container-id) offset-height))))
        (ps:chain console (log "About to set drag event."))
        (ps:chain console (log "Element w,h:" e-wi e-he))
        (ps:chain console (log "Container w,h:" c-wi c-he))
        (ps:chain console (log "Diffs: " diff-x diff-y))
        (setf (ps:chain document (get-element-by-id container-id) style cursor) "move"
              (ps:chain document onmousemove)
              (lambda (evt)
                (let*
                    ((evt (or evt (ps:chain window event)))
                     (pos-x (ps:chain evt client-x))
                     (pos-y (ps:chain evt client-y))
                     (a-x ( max 0 (- pos-x diff-x)))
                     (a-y (max 0 (- pos-y diff-y))))
                  (ps:chain console (log "Move Vars: " pos-x pos-y a-x a-y))
                  (move-div divid
                            (if (> (+ a-x e-wi) c-wi)
                                (- c-wi e-wi)
                                a-x)
                            (if (> (+ a-y e-he) c-he)
                                (- c-he e-he)
                                a-y)))))))

    (defun end-div-dragging (container-id)
      (let
          ((a (ps:chain document (create-element "script"))))
        (setf (ps:chain document (get-element-by-id container-id) style cursor) "default"
              (ps:chain document onmousemove) (lambda ()))))

    (defun spawn-new-task-window ()
      "Create a draggable div for making new tasks."
      )

    ;; (refresh-tasks)
    ))

(defun generate-style ()
  (cl-css:css
   '(("*"
      :margin 0
      :padding 0)
     ("li > p"
      :display inline)
     ("#container"
      :position "absolute"
      :background-color "LightGray")
     ("#elem"
      :position "absolute"
      :background-color "gray"
      :user-select "none"))))

;; (api-start-or-restart :reset-routes t)
;; (api-stop)
