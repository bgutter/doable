;;
;; doable.lisp
;;

;; load tiny-routes

(defpackage :doable
  (:use :common-lisp :local-time :closer-mop :clack)
  (:export
   #:new-task
   #:backlog
   #:*task-list*))

(in-package :doable)

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

(defclass task ()
  ((summary
    :documentation "Single line summary."
    :accessor task-summary
    :initarg :summary
    :initform "New Task")
   (created-date
    :initform (local-time:now))
   (due-date
    :accessor when-due
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
  (push task *task-list*))

(defun reset-tasks ()
  "Testing and development only. Clear task list."
  (setf *task-list* nil))

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

;; server

;; (defvar *api-handler*
;;   (lambda (env)
;;     '(200 (:content-type "application/json") ("{clack:\"up\"}"))))

(tiny:define-routes *app*
  (tiny:define-get "/" ()
    (tiny:ok "aliv2e"))
  ;;(tiny:define-get "/accounts/:account-id" (request)
    ;;(let ((account-id (tiny:path-param request :account-id)))
    ;;  (tiny:ok (format nil "Your account id: ~a." account-id))))
  (tiny:define-get "/home" ()
    (tiny:ok "Home"))
  (tiny:define-get "/*" ()
    (tiny:not-found "not-found")))

(defvar *clack-instance* nil)

(defun api-stop ()
  "Stop the REST server, if it is running."
  (when *clack-instance*
    (clack:stop *clack-instance*)
    (setf *clack-instance* nil))
  (values))

(defun api-start-or-restart ()
  "Start the REST server. Restart it if running."
  (api-stop)
  (setf *clack-instance*
        (clack:clackup *app* :server :hunchentoot)))

(api-start-or-restart)
