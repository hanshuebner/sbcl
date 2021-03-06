;;;; bootstrapping fundamental machinery (e.g. DEFUN, DEFCONSTANT,
;;;; DEFVAR) from special forms and primitive functions
;;;;
;;;; KLUDGE: The bootstrapping aspect of this is now obsolete. It was
;;;; originally intended that this file file would be loaded into a
;;;; Lisp image which had Common Lisp primitives defined, and DEFMACRO
;;;; defined, and little else. Since then that approach has been
;;;; dropped and this file has been modified somewhat to make it work
;;;; more cleanly when used to predefine macros at
;;;; build-the-cross-compiler time.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")


;;;; IN-PACKAGE

(defmacro-mundanely in-package (string-designator)
  (let ((string (string string-designator)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setq *package* (find-undeleted-package-or-lose ,string)))))

;;;; MULTIPLE-VALUE-FOO

(defun list-of-symbols-p (x)
  (and (listp x)
       (every #'symbolp x)))

(defmacro-mundanely multiple-value-bind (vars value-form &body body)
  (if (list-of-symbols-p vars)
    ;; It's unclear why it would be important to special-case the LENGTH=1 case
    ;; at this level, but the CMU CL code did it, so.. -- WHN 19990411
    (if (= (length vars) 1)
      `(let ((,(car vars) ,value-form))
         ,@body)
      (let ((ignore (sb!xc:gensym)))
        `(multiple-value-call #'(lambda (&optional ,@(mapcar #'list vars)
                                         &rest ,ignore)
                                  (declare (ignore ,ignore))
                                  ,@body)
                              ,value-form)))
    (error "Vars is not a list of symbols: ~S" vars)))

(defmacro-mundanely multiple-value-setq (vars value-form)
  (unless (list-of-symbols-p vars)
    (error "Vars is not a list of symbols: ~S" vars))
  ;; MULTIPLE-VALUE-SETQ is required to always return just the primary
  ;; value of the value-from, even if there are no vars. (SETF VALUES)
  ;; in turn is required to return as many values as there are
  ;; value-places, hence this:
  (if vars
      `(values (setf (values ,@vars) ,value-form))
      `(values ,value-form)))

(defmacro-mundanely multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))

;;;; various conditional constructs

;;; COND defined in terms of IF
(defmacro-mundanely cond (&rest clauses)
  (if (endp clauses)
      nil
      (let ((clause (first clauses))
            (more (rest clauses)))
        (if (atom clause)
            (error 'simple-type-error
                   :format-control "COND clause is not a ~S: ~S"
                   :format-arguments (list 'cons clause)
                   :expected-type 'cons
                   :datum clause)
            (let ((test (first clause))
                  (forms (rest clause)))
              (if (endp forms)
                  (let ((n-result (gensym)))
                    `(let ((,n-result ,test))
                       (if ,n-result
                           ,n-result
                           (cond ,@more))))
                  (if (eq t test)
                      ;; THE to preserve non-toplevelness for FOO in
                      ;;   (COND (T (FOO)))
                      ;; FIXME: this hides all other possible stylistic issues,
                      ;; not the least of which is a code deletion note,
                      ;; if there are forms following the one whose head is T.
                      ;; This is not usually the SBCL preferred way.
                      `(the t (progn ,@forms))
                      `(if ,test
                           (progn ,@forms)
                           ,(when more `(cond ,@more))))))))))

(defmacro-mundanely when (test &body forms)
  #!+sb-doc
  "If the first argument is true, the rest of the forms are
evaluated as a PROGN."
  `(if ,test (progn ,@forms) nil))

(defmacro-mundanely unless (test &body forms)
  #!+sb-doc
  "If the first argument is not true, the rest of the forms are
evaluated as a PROGN."
  `(if ,test nil (progn ,@forms)))

(defmacro-mundanely and (&rest forms)
  (cond ((endp forms) t)
        ((endp (rest forms))
         ;; Preserve non-toplevelness of the form!
         `(the t ,(first forms)))
        (t
         `(if ,(first forms)
              (and ,@(rest forms))
              nil))))

(defmacro-mundanely or (&rest forms)
  (cond ((endp forms) nil)
        ((endp (rest forms))
         ;; Preserve non-toplevelness of the form!
         `(the t ,(first forms)))
        (t
         (let ((n-result (gensym)))
           `(let ((,n-result ,(first forms)))
              (if ,n-result
                  ,n-result
                  (or ,@(rest forms))))))))

;;;; various sequencing constructs

(flet ((prog-expansion-from-let (varlist body-decls let)
         (multiple-value-bind (body decls)
             (parse-body body-decls :doc-string-allowed nil)
           `(block nil
              (,let ,varlist
                ,@decls
                (tagbody ,@body))))))
  (defmacro-mundanely prog (varlist &body body-decls)
    (prog-expansion-from-let varlist body-decls 'let))
  (defmacro-mundanely prog* (varlist &body body-decls)
    (prog-expansion-from-let varlist body-decls 'let*)))

(defmacro-mundanely prog1 (result &body body)
  (let ((n-result (gensym)))
    `(let ((,n-result ,result))
       ,@body
       ,n-result)))

(defmacro-mundanely prog2 (form1 result &body body)
  `(prog1 (progn ,form1 ,result) ,@body))

;;;; DEFUN

;;; Should we save the inline expansion of the function named NAME?
(defun inline-fun-name-p (name)
  (or
   ;; the normal reason for saving the inline expansion
   (let ((inlinep (info :function :inlinep name)))
     (member inlinep '(:inline :maybe-inline)))
   ;; another reason for saving the inline expansion: If the
   ;; ANSI-recommended idiom
   ;;   (DECLAIM (INLINE FOO))
   ;;   (DEFUN FOO ..)
   ;;   (DECLAIM (NOTINLINE FOO))
   ;; has been used, and then we later do another
   ;;   (DEFUN FOO ..)
   ;; without a preceding
   ;;   (DECLAIM (INLINE FOO))
   ;; what should we do with the old inline expansion when we see the
   ;; new DEFUN? Overwriting it with the new definition seems like
   ;; the only unsurprising choice.
   (info :function :inline-expansion-designator name)))

(defmacro-mundanely defun (&environment env name args &body body)
  #!+sb-doc
  "Define a function at top level."
  #+sb-xc-host
  (unless (symbol-package (fun-name-block-name name))
    (warn "DEFUN of uninterned function name ~S (tricky for GENESIS)" name))
  (multiple-value-bind (forms decls doc) (parse-body body)
    (let* (;; stuff shared between LAMBDA and INLINE-LAMBDA and NAMED-LAMBDA
           (lambda-guts `(,args
                          ,@decls
                          (block ,(fun-name-block-name name)
                            ,@forms)))
           (lambda `(lambda ,@lambda-guts))
           #-sb-xc-host
           (named-lambda `(named-lambda ,name ,@lambda-guts))
           (inline-lambda
            (when (inline-fun-name-p name)
              ;; we want to attempt to inline, so complain if we can't
              (or (sb!c:maybe-inline-syntactic-closure lambda env)
                  (progn
                    (#+sb-xc-host warn
                     #-sb-xc-host sb!c:maybe-compiler-notify
                     "lexical environment too hairy, can't inline DEFUN ~S"
                     name)
                    nil)))))
      `(progn
         ;; In cross-compilation of toplevel DEFUNs, we arrange for
         ;; the LAMBDA to be statically linked by GENESIS.
         ;;
         ;; It may seem strangely inconsistent not to use NAMED-LAMBDA
         ;; here instead of LAMBDA. The reason is historical:
         ;; COLD-FSET was written before NAMED-LAMBDA, and has special
         ;; logic of its own to notify the compiler about NAME.
         #+sb-xc-host
         (cold-fset ,name ,lambda)

         (eval-when (:compile-toplevel)
           (sb!c:%compiler-defun ',name ',inline-lambda t))
         (%defun ',name
                 ;; In normal compilation (not for cold load) this is
                 ;; where the compiled LAMBDA first appears. In
                 ;; cross-compilation, we manipulate the
                 ;; previously-statically-linked LAMBDA here.
                 #-sb-xc-host ,named-lambda
                 #+sb-xc-host (fdefinition ',name)
                 ,doc
                 ',inline-lambda
                 (sb!c:source-location))))))

;; Approximately 20% of the output from #+sb-show is from lines associated with
;; printing "redefining NAME in %DEFUN" and lines about how it is not possible
;; to actually invoke WARN at that point.
;; So FBOUNDP etc is useless because the warning is ignored during cold-init.
#-sb-xc-host
(macrolet
    ((def-defun (name fboundp-check)
       `(defun ,name (name def doc inline-lambda source-location)
          (declare (type function def))
          (declare (type (or null simple-string) doc))
          ,@(unless fboundp-check
              '((declare (ignore source-location))))
          ;; should've been checked by DEFMACRO DEFUN
          (aver (legal-fun-name-p name))
          (sb!c:%compiler-defun name inline-lambda nil)
          ,@(when fboundp-check
              `((when (fboundp name)
                  (/show0 "redefining NAME in %DEFUN")
                  (warn 'redefinition-with-defun
                        :name name
                        :new-function def
                        :new-location source-location))))
          (setf (sb!xc:fdefinition name) def)
  ;; %COMPILER-DEFUN doesn't do this except at compile-time, when it
  ;; also checks package locks. By doing this here we let (SETF
  ;; FDEFINITION) do the load-time package lock checking before
  ;; we frob any existing inline expansions.
          (sb!c::%set-inline-expansion name nil inline-lambda)

          (sb!c::note-name-defined name :function)

          (when doc
            (setf (%fun-doc def) doc))

          name)))
  (def-defun %defun t)
  (def-defun !%quietly-defun nil))

;;;; DEFVAR and DEFPARAMETER

(defmacro-mundanely defvar (var &optional (val nil valp) (doc nil docp))
  #!+sb-doc
  "Define a special variable at top level. Declare the variable
  SPECIAL and, optionally, initialize it. If the variable already has a
  value, the old value is not clobbered. The third argument is an optional
  documentation string for the variable."
  `(progn
     (eval-when (:compile-toplevel)
       (%compiler-defvar ',var))
     (%defvar ',var (unless (boundp ',var) ,val)
              ',valp ,doc ',docp
              (sb!c:source-location))))

(defmacro-mundanely defparameter (var val &optional (doc nil docp))
  #!+sb-doc
  "Define a parameter that is not normally changed by the program,
  but that may be changed without causing an error. Declare the
  variable special and sets its value to VAL, overwriting any
  previous value. The third argument is an optional documentation
  string for the parameter."
  `(progn
     (eval-when (:compile-toplevel)
       (%compiler-defvar ',var))
     (%defparameter ',var ,val ,doc ',docp (sb!c:source-location))))

(defun %compiler-defvar (var)
  (sb!xc:proclaim `(special ,var)))

#-sb-xc-host
(defun %defvar (var val valp doc docp source-location)
  (%compiler-defvar var)
  (when valp
    (unless (boundp var)
      (set var val)))
  (when docp
    (setf (fdocumentation var 'variable) doc))
  (sb!c:with-source-location (source-location)
    (setf (info :source-location :variable var) source-location))
  var)

#-sb-xc-host
(defun %defparameter (var val doc docp source-location)
  (%compiler-defvar var)
  (set var val)
  (when docp
    (setf (fdocumentation var 'variable) doc))
  (sb!c:with-source-location (source-location)
    (setf (info :source-location :variable var) source-location))
  var)

;;;; iteration constructs

;;; (These macros are defined in terms of a function FROB-DO-BODY which
;;; is also used by SB!INT:DO-ANONYMOUS. Since these macros should not
;;; be loaded on the cross-compilation host, but SB!INT:DO-ANONYMOUS
;;; and FROB-DO-BODY should be, these macros can't conveniently be in
;;; the same file as FROB-DO-BODY.)
(defmacro-mundanely do (varlist endlist &body body)
  #!+sb-doc
  "DO ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct. Each Var is initialized in parallel to the value of the
  specified Init form. On subsequent iterations, the Vars are assigned the
  value of the Step form (if any) in parallel. The Test is evaluated before
  each evaluation of the body Forms. When the Test is true, the Exit-Forms
  are evaluated as a PROGN, with the result being the value of the DO. A block
  named NIL is established around the entire expansion, allowing RETURN to be
  used as an alternate exit mechanism."
  (frob-do-body varlist endlist body 'let 'psetq 'do nil))
(defmacro-mundanely do* (varlist endlist &body body)
  #!+sb-doc
  "DO* ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct. Each Var is initialized sequentially (like LET*) to the
  value of the specified Init form. On subsequent iterations, the Vars are
  sequentially assigned the value of the Step form (if any). The Test is
  evaluated before each evaluation of the body Forms. When the Test is true,
  the Exit-Forms are evaluated as a PROGN, with the result being the value
  of the DO. A block named NIL is established around the entire expansion,
  allowing RETURN to be used as an alternate exit mechanism."
  (frob-do-body varlist endlist body 'let* 'setq 'do* nil))

;;; DOTIMES and DOLIST could be defined more concisely using
;;; destructuring macro lambda lists or DESTRUCTURING-BIND, but then
;;; it'd be tricky to use them before those things were defined.
;;; They're used enough times before destructuring mechanisms are
;;; defined that it looks as though it's worth just implementing them
;;; ASAP, at the cost of being unable to use the standard
;;; destructuring mechanisms.
(defmacro-mundanely dotimes ((var count &optional (result nil)) &body body)
  (cond ((integerp count)
        `(do ((,var 0 (1+ ,var)))
             ((>= ,var ,count) ,result)
           (declare (type unsigned-byte ,var))
           ,@body))
        (t
         (let ((c (gensym "COUNT")))
           `(do ((,var 0 (1+ ,var))
                 (,c ,count))
                ((>= ,var ,c) ,result)
              (declare (type unsigned-byte ,var)
                       (type integer ,c))
              ,@body)))))

(defmacro-mundanely dolist ((var list &optional (result nil)) &body body &environment env)
  ;; We repeatedly bind the var instead of setting it so that we never
  ;; have to give the var an arbitrary value such as NIL (which might
  ;; conflict with a declaration). If there is a result form, we
  ;; introduce a gratuitous binding of the variable to NIL without the
  ;; declarations, then evaluate the result form in that
  ;; environment. We spuriously reference the gratuitous variable,
  ;; since we don't want to use IGNORABLE on what might be a special
  ;; var.
  (multiple-value-bind (forms decls) (parse-body body :doc-string-allowed nil)
    (let* ((n-list (gensym "N-LIST"))
           (start (gensym "START")))
      (multiple-value-bind (clist members clist-ok)
          (cond ((sb!xc:constantp list env)
                 (let ((value (constant-form-value list env)))
                   (multiple-value-bind (all dot) (list-members value :max-length 20)
                     (when (eql dot t)
                       ;; Full warning is too much: the user may terminate the loop
                       ;; early enough. Contents are still right, though.
                       (style-warn "Dotted list ~S in DOLIST." value))
                     (if (eql dot :maybe)
                         (values value nil nil)
                         (values value all t)))))
                ((and (consp list) (eq 'list (car list))
                      (every (lambda (arg) (sb!xc:constantp arg env)) (cdr list)))
                 (let ((values (mapcar (lambda (arg) (constant-form-value arg env)) (cdr list))))
                   (values values values t)))
                (t
                 (values nil nil nil)))
        `(block nil
           (let ((,n-list ,(if clist-ok (list 'quote clist) list)))
             (tagbody
                ,start
                (unless (endp ,n-list)
                  (let ((,var ,(if clist-ok
                                   `(truly-the (member ,@members) (car ,n-list))
                                   `(car ,n-list))))
                    ,@decls
                    (setq ,n-list (cdr ,n-list))
                    (tagbody ,@forms))
                  (go ,start))))
           ,(if result
                `(let ((,var nil))
                   ;; Filter out TYPE declarations (VAR gets bound to NIL,
                   ;; and might have a conflicting type declaration) and
                   ;; IGNORE (VAR might be ignored in the loop body, but
                   ;; it's used in the result form).
                   ,@(filter-dolist-declarations decls)
                   ,var
                   ,result)
                nil))))))

;;;; conditions, handlers, restarts

;;; KLUDGE: we PROCLAIM these special here so that we can use restart
;;; macros in the compiler before the DEFVARs are compiled.
;;;
;;; For an explanation of these data structures, see DEFVARs in
;;; target-error.lisp.
(sb!xc:proclaim '(special *handler-clusters* *restart-clusters*))

;;; Generated code need not check for unbound-marker in *HANDLER-CLUSTERS*
;;; (resp *RESTART-). To elicit this we must poke at the info db.
;;; SB!XC:PROCLAIM SPECIAL doesn't advise the host Lisp that *HANDLER-CLUSTERS*
;;; is special and so it rightfully complains about a SETQ of the variable.
;;; But I must SETQ if proclaming ALWAYS-BOUND because the xc asks the host
;;; whether it's currently bound.
;;; But the DEFVARs are in target-error. So it's one hack or another.
(setf (info :variable :always-bound '*handler-clusters*)
      #+sb-xc :always-bound #-sb-xc :eventually)
(setf (info :variable :always-bound '*restart-clusters*)
      #+sb-xc :always-bound #-sb-xc :eventually)

(defmacro-mundanely with-condition-restarts
    (condition-form restarts-form &body body)
  #!+sb-doc
  "Evaluates the BODY in a dynamic environment where the restarts in the list
   RESTARTS-FORM are associated with the condition returned by CONDITION-FORM.
   This allows FIND-RESTART, etc., to recognize restarts that are not related
   to the error currently being debugged. See also RESTART-CASE."
  (once-only ((restarts restarts-form))
    (with-unique-names (restart)
      ;; FIXME: check the need for interrupt-safety.
      `(unwind-protect
           (progn
             (dolist (,restart ,restarts)
               (push ,condition-form
                     (restart-associated-conditions ,restart)))
             ,@body)
         (dolist (,restart ,restarts)
           (pop (restart-associated-conditions ,restart)))))))

(defmacro-mundanely restart-bind (bindings &body forms)
  #!+sb-doc
  "(RESTART-BIND ({(case-name function {keyword value}*)}*) forms)
   Executes forms in a dynamic context where the given bindings are in
   effect. Users probably want to use RESTART-CASE. A case-name of NIL
   indicates an anonymous restart. When bindings contain the same
   restart name, FIND-RESTART will find the first such binding."
  (flet ((parse-binding (binding)
           (unless (>= (length binding) 2)
             (error "ill-formed restart binding: ~S" binding))
           (destructuring-bind (name function
                                &key interactive-function
                                     test-function
                                     report-function)
               binding
             (unless (or name report-function)
               (warn "Unnamed restart does not have a report function: ~
                      ~S" binding))
             `(make-restart ',name ,function
                            ,report-function
                            ,interactive-function
                            ,@(and test-function
                                   `(,test-function))))))
    `(let ((*restart-clusters*
             (cons (list ,@(mapcar #'parse-binding bindings))
                   *restart-clusters*)))
       ,@forms)))

;;; Wrap the RESTART-CASE expression in a WITH-CONDITION-RESTARTS if
;;; appropriate. Gross, but it's what the book seems to say...
(defun munge-restart-case-expression (expression env)
  (let ((exp (%macroexpand expression env)))
    (if (consp exp)
        (let* ((name (car exp))
               (args (if (eq name 'cerror) (cddr exp) (cdr exp))))
          (if (member name '(signal error cerror warn))
              (once-only ((n-cond `(coerce-to-condition
                                    ,(first args)
                                    (list ,@(rest args))
                                    ',(case name
                                        (warn 'simple-warning)
                                        (signal 'simple-condition)
                                        (t 'simple-error))
                                    ',name)))
                `(with-condition-restarts
                     ,n-cond
                     (car *restart-clusters*)
                   ,(if (eq name 'cerror)
                        `(cerror ,(second exp) ,n-cond)
                        `(,name ,n-cond))))
              expression))
        expression)))

(defmacro-mundanely restart-case (expression &body clauses &environment env)
  #!+sb-doc
  "(RESTART-CASE form {(case-name arg-list {keyword value}* body)}*)
   The form is evaluated in a dynamic context where the clauses have
   special meanings as points to which control may be transferred (see
   INVOKE-RESTART).  When clauses contain the same case-name,
   FIND-RESTART will find the first such clause. If form is a call to
   SIGNAL, ERROR, CERROR or WARN (or macroexpands into such) then the
   signalled condition will be associated with the new restarts."
  ;; PARSE-CLAUSE (which uses PARSE-KEYWORDS-AND-BODY) is used to
  ;; parse all clauses into lists of the form
  ;;
  ;;  (NAME TAG KEYWORDS LAMBDA-LIST BODY)
  ;;
  ;; where KEYWORDS are suitable keywords for use in HANDLER-BIND
  ;; bindings. These lists are then passed to
  ;; * MAKE-BINDING which generates bindings for the respective NAME
  ;;   for HANDLER-BIND
  ;; * MAKE-APPLY-AND-RETURN which generates TAGBODY entries executing
  ;;   the respective BODY.
  (let ((block-tag (sb!xc:gensym "BLOCK"))
        (temp-var (gensym)))
    (labels ((parse-keywords-and-body (keywords-and-body)
               (do ((form keywords-and-body (cddr form))
                    (result '())) (nil)
                 (destructuring-bind (&optional key (arg nil argp) &rest rest)
                     form
                   (declare (ignore rest))
                   (setq result
                         (append
                          (cond
                            ((and (eq key :report) argp)
                             (list :report-function
                                   (if (stringp arg)
                                       `#'(lambda (stream)
                                            (write-string ,arg stream))
                                       `#',arg)))
                            ((and (eq key :interactive) argp)
                             (list :interactive-function `#',arg))
                            ((and (eq key :test) argp)
                             (list :test-function `#',arg))
                            (t
                             (return (values result form))))
                          result)))))
             (parse-clause (clause)
               (unless (and (listp clause) (>= (length clause) 2)
                            (listp (second clause)))
                 (error "ill-formed ~S clause, no lambda-list:~%  ~S"
                        'restart-case clause))
               (destructuring-bind (name lambda-list &body body) clause
                 (multiple-value-bind (keywords body)
                     (parse-keywords-and-body body)
                   (list name (sb!xc:gensym "TAG") keywords lambda-list body))))
             (make-binding (clause-data)
               (destructuring-bind (name tag keywords lambda-list body) clause-data
                 (declare (ignore body))
                 `(,name
                   (lambda ,(cond ((null lambda-list)
                                   ())
                                  ((and (null (cdr lambda-list))
                                        (not (member (car lambda-list)
                                                     '(&optional &key &aux))))
                                   '(temp))
                                  (t
                                   '(&rest temp)))
                     ,@(when lambda-list `((setq ,temp-var temp)))
                     (locally (declare (optimize (safety 0)))
                       (go ,tag)))
                   ,@keywords)))
             (make-apply-and-return (clause-data)
               (destructuring-bind (name tag keywords lambda-list body) clause-data
                 (declare (ignore name keywords))
                 `(,tag (return-from ,block-tag
                          ,(cond ((null lambda-list)
                                  `(progn ,@body))
                                 ((and (null (cdr lambda-list))
                                       (not (member (car lambda-list)
                                                    '(&optional &key &aux))))
                                  `(funcall (lambda ,lambda-list ,@body) ,temp-var))
                                 (t
                                  `(apply (lambda ,lambda-list ,@body) ,temp-var))))))))
      (let ((clauses-data (mapcar #'parse-clause clauses)))
        `(block ,block-tag
           (let ((,temp-var nil))
             (declare (ignorable ,temp-var))
             (tagbody
                (restart-bind
                    ,(mapcar #'make-binding clauses-data)
                  (return-from ,block-tag
                    ,(munge-restart-case-expression expression env)))
                ,@(mapcan #'make-apply-and-return clauses-data))))))))

(defmacro-mundanely with-simple-restart ((restart-name format-string
                                                       &rest format-arguments)
                                         &body forms)
  #!+sb-doc
  "(WITH-SIMPLE-RESTART (restart-name format-string format-arguments)
   body)
   If restart-name is not invoked, then all values returned by forms are
   returned. If control is transferred to this restart, it immediately
   returns the values NIL and T."
  (let ((stream (gensym "STREAM")))
   `(restart-case
        ;; If there's just one body form, then don't use PROGN. This allows
        ;; RESTART-CASE to "see" calls to ERROR, etc.
        ,(if (= (length forms) 1) (car forms) `(progn ,@forms))
      (,restart-name ()
        :report (lambda (,stream)
                  (format ,stream ,format-string ,@format-arguments))
        (values nil t)))))

(defmacro-mundanely %handler-bind (bindings form)
  ;; As an optimization, this looks at the handler parts of BINDINGS
  ;; and turns handlers of the forms (lambda ...) and (function
  ;; (lambda ...)) into local, dynamic-extent functions.
  (let ((local-functions '())
        (cluster-entries '()))
    (labels ((cons-form (type handler)
               `(cons ',type ,handler))
             (local-function (type lambda-form)
               (let ((name (sb!xc:gensym "HANDLER")))
                 (push `(,name ,@(rest lambda-form)) local-functions)
                 (push (cons-form type `(function ,name)) cluster-entries)))
             (process-binding (binding)
               (unless (proper-list-of-length-p binding 2)
                 (error "ill-formed handler binding: ~S" binding))
               (destructuring-bind (type handler) binding
                 (typecase handler
                   ((cons (eql lambda) t)
                    (local-function type handler))
                   ((cons (eql function)
                          (cons (cons (eql lambda) t) t))
                    (local-function type (second handler)))
                   (t
                    (push (apply #'cons-form binding) cluster-entries))))))
      (mapc #'process-binding bindings)
      `(dx-flet (,@(reverse local-functions))
         (let ((*handler-clusters*
                (list* (list ,@(nreverse cluster-entries)) *handler-clusters*)))
           #!+stack-allocatable-fixed-objects
           (declare (truly-dynamic-extent *handler-clusters*))
           (progn ,form))))))

(defmacro-mundanely handler-bind (bindings &body forms)
  #!+sb-doc
  "(HANDLER-BIND ( {(type handler)}* ) body)

Executes body in a dynamic context where the given handler bindings are in
effect. Each handler must take the condition being signalled as an argument.
The bindings are searched first to last in the event of a signalled
condition."
  `(%handler-bind ,bindings
                  #!-x86 (progn ,@forms)
                  ;; Need to catch FP errors here!
                  #!+x86 (multiple-value-prog1 (progn ,@forms) (float-wait))))

(defmacro-mundanely handler-case (form &rest cases)
  #!+sb-doc
  "(HANDLER-CASE form { (type ([var]) body) }* )

Execute FORM in a context with handlers established for the condition types. A
peculiar property allows type to be :NO-ERROR. If such a clause occurs, and
form returns normally, all its values are passed to this clause as if by
MULTIPLE-VALUE-CALL. The :NO-ERROR clause accepts more than one var
specification."
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
        (let ((normal-return (make-symbol "normal-return"))
              (error-return  (make-symbol "error-return")))
          `(block ,error-return
             (multiple-value-call (lambda ,@(cdr no-error-clause))
               (block ,normal-return
                 (return-from ,error-return
                   (handler-case (return-from ,normal-return ,form)
                     ,@(remove no-error-clause cases)))))))
        (let* ((local-funs nil)
               (annotated-cases
                (mapcar (lambda (case)
                          (with-unique-names (tag fun)
                            (destructuring-bind (type ll &body body) case
                              (push `(,fun ,ll ,@body) local-funs)
                              (list tag type ll fun))))
                        cases)))
          (with-unique-names (block cell form-fun)
            `(dx-flet ((,form-fun ()
                         #!-x86 ,form
                         ;; Need to catch FP errors here!
                         #!+x86 (multiple-value-prog1 ,form (float-wait)))
                       ,@(reverse local-funs))
               (declare (optimize (sb!c::check-tag-existence 0)))
               (block ,block
                 ;; KLUDGE: We use a dx CONS cell instead of just assigning to
                 ;; the variable directly, so that we can stack allocate
                 ;; robustly: dx value cells don't work quite right, and it is
                 ;; possible to construct user code that should loop
                 ;; indefinitely, but instead eats up some stack each time
                 ;; around.
                 (dx-let ((,cell (cons :condition nil)))
                   (declare (ignorable ,cell))
                   (tagbody
                      (%handler-bind
                       ,(mapcar (lambda (annotated-case)
                                  (destructuring-bind (tag type ll fun-name) annotated-case
                                    (declare (ignore fun-name))
                                    (list type
                                          `(lambda (temp)
                                             ,(if ll
                                                  `(setf (cdr ,cell) temp)
                                                  '(declare (ignore temp)))
                                             (go ,tag)))))
                                annotated-cases)
                       (return-from ,block (,form-fun)))
                      ,@(mapcan
                         (lambda (annotated-case)
                           (destructuring-bind (tag type ll fun-name) annotated-case
                             (declare (ignore type))
                             (list tag
                                   `(return-from ,block
                                      ,(if ll
                                           `(,fun-name (cdr ,cell))
                                           `(,fun-name))))))
                         annotated-cases))))))))))

;;;; miscellaneous

(defmacro-mundanely return (&optional (value nil))
  `(return-from nil ,value))

(defmacro-mundanely psetq (&rest pairs)
  #!+sb-doc
  "PSETQ {var value}*
   Set the variables to the values, like SETQ, except that assignments
   happen in parallel, i.e. no assignments take place until all the
   forms have been evaluated."
  ;; Given the possibility of symbol-macros, we delegate to PSETF
  ;; which knows how to deal with them, after checking that syntax is
  ;; compatible with PSETQ.
  (do ((pair pairs (cddr pair)))
      ((endp pair) `(psetf ,@pairs))
    (unless (symbolp (car pair))
      (error 'simple-program-error
             :format-control "variable ~S in PSETQ is not a SYMBOL"
             :format-arguments (list (car pair))))))

(defmacro-mundanely lambda (&whole whole args &body body)
  (declare (ignore args body))
  `#',whole)

(defmacro-mundanely named-lambda (&whole whole name args &body body)
  (declare (ignore name args body))
  `#',whole)

(defmacro-mundanely lambda-with-lexenv (&whole whole
                                        declarations macros symbol-macros
                                        &body body)
  (declare (ignore declarations macros symbol-macros body))
  `#',whole)

;;; this eliminates a whole bundle of unknown function STYLE-WARNINGs
;;; when cross-compiling.  It's not critical for behaviour, but is
;;; aesthetically pleasing, except inasmuch as there's this list of
;;; magic functions here.  -- CSR, 2003-04-01
#+sb-xc-host
(sb!xc:proclaim '(ftype (function * *)
                        ;; functions appearing in fundamental defining
                        ;; macro expansions:
                        %compiler-deftype
                        %compiler-defvar
                        %defun
                        %defsetf
                        %defparameter
                        %defvar
                        sb!c:%compiler-defun
                        sb!c::%define-symbol-macro
                        sb!c::%defconstant
                        sb!c::%define-compiler-macro
                        sb!c::%defmacro
                        sb!kernel::%compiler-defstruct
                        sb!kernel::%compiler-define-condition
                        sb!kernel::%defstruct
                        sb!kernel::%define-condition
                        ;; miscellaneous functions commonly appearing
                        ;; as a result of macro expansions or compiler
                        ;; transformations:
                        sb!kernel::arg-count-error ; PARSE-DEFMACRO
                        ))
