(setq rcs-common0-
   "$Header: record.l,v 1.3 84/02/29 19:33:50 jkf Exp $")

;;					-[Mon Feb 20 15:00:52 1984 by jkf]-
;; simple record package
;;

(eval-when (compile)
   (or (get 'record 'version) (load 'record)))

(defvar record-pkg-indicator 'record-package-dr-record)

(declare (macros nil))

;; internal macro
(defmacro dr-error (message &rest args)
   ;; print an error preceeded by 'defrecord'
   ;; internal use only
   `(error ',(concat "defrecord: " message) ,@args))


;(defrecord dr-record
;   	name	; name of record
;	storage ; 'list' or 'vector'
;	options	; subset of 'named', 'access-check'
;   	fields  ; list of dr-field records
;)

(eval-when (compile eval)
   (putprop 'dr-record
	    '(dr-record list nil ((fields 3 nil)
				  (options 2 nil)
				  (storage 1 nil)
				  (name 0 nil)))
	    record-pkg-indicator))
(defmacro make-dr-record (&rest args) (record-pkg-construct 'dr-record args))
(defmacro dr-record-storage (arg) `(nth 1 ,arg))
(defmacro dr-record-options (arg) `(nth 2 ,arg))
(defmacro dr-record-fields (arg) `(nth 3 ,arg))

;(defrecord dr-field
;   ;; internal structure used to store info on fields
;   name
;   offset
;   defaultvalue)

(eval-when (compile eval)
   (putprop 'dr-field
	    '(dr-field list nil ((defaultvalue 2 nil)
				 (offset 1 nil)
				 (name 0 nil)))
	    record-pkg-indicator))
(defmacro make-dr-field (&rest args) (record-pkg-construct 'dr-field args))

(defmacro dr-field-name (arg) `(nth 0 ,arg))
(defmacro dr-field-offset (arg) `(nth 1 ,arg))
(defmacro dr-field-defaultvalue (arg) `(nth 2 ,arg))

;; internal functions (called by macros)

(defun record-pkg-construct (recname args)
   ;; called to expand a make- form.
   ;; recname is the name of a record

   ; convert to an assq list, verifing field names
   (let* ((dr-record (get recname record-pkg-indicator))
	  (fields (dr-record-fields dr-record))
	  (given))
      (do ((xx args (cddr xx)))
	  ((null xx))
	  (if (assq (car xx) fields)
	     then (push (cons (car xx) (cadr xx)) given)
	     else (dr-error " for record " recname
			   ", this field doesn't exist " (car xx))))
      ;; now build a list of values.
      ;; use the fact that the fields list is in the reverse order
      (do ((xx fields (cdr xx))
	   (got)
	   (res))
	  ((null xx)
	   ;; now we have a list of values to compute to build this
	   ;; form.  
	   (caseq (dr-record-storage dr-record)
	      (list `(list ,@res))
	      (vector `(vector ,@res))
	      (t (error "record package is confused about storage type "))))
	  (if (setq got (assq (dr-field-name (car xx)) given))
	     then (push (cdr got) res)	; given value
	     else (push (dr-field-defaultvalue (car xx))
			res)))))



(defun record-pkg-access (recname fieldname arg)
   ;; return code access the given field in the given record
   (let ((dr-record (get recname record-pkg-indicator))
	 (recnamefield)
	 (fieldinfo)
	 (options)
	 (storage))
      (setq fieldinfo (assq fieldname (dr-record-fields dr-record)))
      (setq options (dr-record-options dr-record))
      (setq storage (dr-record-storage dr-record))
      (if (null fieldinfo)
	 then (dr-error "internal error: can't find field " fieldname
			" in record " recname))
      (if (memq 'access-check options)
	 then (setq recnamefield (assq '-record-field-name-
				       (dr-record-fields dr-record)))
	      `((lambda (defrecord-acma)
		   (cond ((not (eq ',recname
				   ,(dr-accessor storage
						 (dr-field-offset
						    recnamefield)
						 'defrecord-acma)))
			  (record-pkg-illegal-access ',recname ',fieldname
						     defrecord-acma))
			 (t ,(dr-accessor storage
					  (dr-field-offset fieldinfo)
					  'defrecord-acma))))
		,arg)
	 else (dr-accessor storage (dr-field-offset fieldinfo) arg))))


(defun dr-accessor (class index obj)
   ;; determine the correct field accessor to get the index'th element
   ;; from obj, give the storage type class (either list or vector).
   ;;
   (caseq class
      (list `(nth ,index ,obj))
      (vector `(vref ,obj ,index))
      (t (error "record package: illegal storage class " class))))

(defun record-pkg-illegal-access (recname fieldname value)
   (error "Unable to access field " fieldname " of record " recname
	  " because this is not an instance of that record: "
	  value))
		    
      
	  
	  
(defun defrecord-name (form)
   ;; user callable function to return the record name of
   ;; a record
   (if (defrecord-namedp form)
      then (if (dtpr form) then (cadr form)
	    elseif (vectorp form)
	      then (vref form 1))
      else (error "record-name: this record doesn't have a name " form)))

(defun defrecord-namedp (form)
   ;; return t iff form is a named record
   (let (name)
      (and (or (and (dtpr form)
		    (cdr form)
		    (progn (setq name (cadr form)) t)
		    (symbolp name))
	       (and (vectorp form)
		    (>& (vsize form) 1)
		    (progn (setq name (vref form 1)) t)
		    (symbolp name)))
	   (get name record-pkg-indicator)
	   t)))

;; external functions
;; The following functions are user callable


(declare (macros t))

(defvar defrecord-default-flags nil)  ; what is assumed in the flag field

(defmacro defrecord (&rest form)
   ;; user callable function
   (if (null form)
      then (error "defrecord: missing record name in " form))

   (let ((name (car form))
	 (args (cdr form))
	 (fields)
	 (nameargs)
	 (givenoptions defrecord-default-flags)
	 (savedoptions)
	 ;;options
	 (namedp)(access-checkp) (vectorp))
      (if (dtpr name)
	 then (setq givenoptions (append givenoptions (cdr name))
		    name (car name)))

      (if (not (symbolp name))
	 then (dr-error "non symbol record name " name))

      ;; process given options
      (do ((xx  givenoptions (cdr xx)))
	  ((null xx))
	  (caseq (car xx)
	     (named (setq namedp t))
	     (access-check (setq access-checkp t))
	     (vector (setq vectorp t))
	     (t ; ignore
	     )))
      ;; look for conflicting options
      (if (and access-checkp (not namedp))
	 then (error "defrecord: Can't specify access-check without also specifying named " form))

      (if namedp then (push 'named savedoptions))
      (if access-checkp then (push 'access-check savedoptions))

      (if namedp
	 then (let ((namefield `(-record-field-name- ',name)))
		 (if args
		    then (setq args (cons (car args)
					  (cons namefield
						(cdr args))))
			 else (setq args (list namefield)))))

      (do ((xx args (cdr xx))
	   (off 0 (1+ off)))
	  ((null xx))
	  (if (dtpr (car xx))
	     then (push (make-dr-field
			   name (caar xx)
			   offset off
			   defaultvalue (cadar xx))
			fields)
	     else (push (make-dr-field
			   name (car xx)
			   offset off)
			fields)))

      
      ; return a progn compile of an accessor and a collection
      ; of accessors
      `(progn 'compile
	      (eval-when (compile load eval)
			 (putprop ',name ',(make-dr-record
					    name name
					    storage (if vectorp
							then 'vector
							else 'list)
					    options savedoptions
					    fields fields)
				  ',record-pkg-indicator))
	      (defmacro ,(concat 'make- name) (&rest args)
			 (record-pkg-construct ',name args))
	      ,@(mapcar '(lambda (dr-field)
			    `(defmacro ,(concat name
						'-
						(dr-field-name dr-field))
					(arg)
					(record-pkg-access
					   ',name
					   ',(dr-field-name dr-field)
					   arg)))
			fields))))


(putprop 'record t 'version)
