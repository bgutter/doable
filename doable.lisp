;;
;; doable.lisp
;;

(defpackage :doable
  (:use :common-lisp :local-time)
  (:export
   #:new-task
   #:backlog
   #:*task-list*))

(in-package :doable)

;; Task Representation

(defclass task ()
  ((summary
    :documentation "Single line summary."
    :initarg :summary
    :initform "New Task")
   (created-date
    :initform (local-time:now))
   (due-date
    :accessor when-due
    :initform nil)
   (status
    :accessor task-status
    :initform 'unstarted))
  (:documentation "Represents a single actionable task."))

(defclass status ()
  ((status-symbol
    :initarg :symbol
    :accessor status-symbol)
   (is-final
    :initarg :final
    :initform nil))
  (:documentation "Represents the status of a task."))

(defvar *task-status-registry* (list))

(defun status-props (status)
  (find status
        *task-status-registry*
        :key #'status-symbol))

(defun status-finalp (status)
  (slot-value (status-props status) 'is-final))

(defmacro define-status (symbol is-final)
  `(let
       ((status (make-instance 'status
                               :symbol ,symbol
                               :final ,is-final)))
     (push status *task-status-registry*)
     status))

(define-status 'open nil)
(define-status 'closed t)

;; (macroexpand '(define-status 'cat t))

;; (status-finalp 'open)

(defun task-openp (task)
  "Is this task in an incomplete state?"
  (not (status-finalp (status-props (task-status task)))))

;; Task Indexing

(defvar *task-list* (list)
  "In-memory unordered record of all tasks.")

(defun index-task (task)
  "Index a new task"
  (push task *task-list*))

;; Task Discovery

(defun sort-tasks-by-due-date (task-list)
  (sort
   (copy-list task-list)
   #'local-time:timestamp<=
   :key #'when-due))

(defvar *backlog-sort-fn* #'sort-tasks-by-due-date)

(defun backlog ()
  (funcall *backlog-sort-fn*
           (remove-if-not #'task-openp
                          *task-list*)))

;; Task Mutation

(defun new-task (&rest initargs)
  "Create a new task."
  (let
      ((task (apply #'make-instance 'task initargs)))
    (index-task task)
    task))
