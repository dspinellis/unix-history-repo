(setq |SCCS-loop| "@(#)loop.l	1.1	10/2/80")
;-*- Mode:LISP; Package:System-Internals; Base:8; Lowercase:T -*-

;The master copy of this file is on ML:LSB1;LOOP >
;The current Lisp machine copy is on AI:LISPM2;LOOP >
;The FASL and QFASL should also be accessible from LIBLSP; on all machines.

; Bugs/complaints/suggestions/solicitations-for-documentation to BUG-LOOP
; at any ITS site.

;; the file was franzified by JKF.  
;

;;;; LOOP Iteration Macro


; Hack up the stuff for data-types.  DATA-TYPE? will always be a macro
; so that it will not require the data-type package at run time if
; all uses of the other routines are conditionalized upon that value.
(defmacro data-type? (x) `(get ,x ':data-type))

(declare
    (*lexpr variable-declarations)
    (*expr initial-value form-wrapper))


;Loop macro

(eval-when (eval compile)
  (defun lexpr-funcall macro (x)
	 `(apply ,(cadr x) (list* . ,(cddr x)))))


(defun loop-displace (x y)
  ((lambda (val) (rplaca x (car val)) (rplacd x (cdr val)) x)
   (cond ((atom y) (list 'progn y)) (t y))))


(defmacro loop-finish () 
    '(go end-loop))

(defun neq macro (x) `(not (eq . ,(cdr x))))


(defun loop-make-psetq (frobs)
    (loop-make-setq
       (car frobs)
       (cond ((null (cddr frobs)) (cadr frobs))
	     (t `(prog1 ,(cadr frobs) ,(loop-make-psetq (cddr frobs)))))))

(defmacro loop-psetq (&rest frobs)
    (loop-make-psetq frobs))




(defvar loop-keyword-alist			;clause introducers
     '( (initially loop-do-initially)
	(finally loop-do-finally)
	(do loop-do-do)
	(doing loop-do-do)
	(return loop-do-return)
	(collect loop-do-collect list)
	(collecting loop-do-collect list)
	(append loop-do-collect append)
	(appending loop-do-collect append)
	(nconc loop-do-collect nconc)
	(nconcing loop-do-collect nconc)
	(count loop-do-collect count)
	(counting loop-do-collect count)
	(sum loop-do-collect sum)
	(summing loop-do-collect sum)
	(maximize loop-do-collect max)
	(minimize loop-do-collect min)
	(always loop-do-always t)
	(never loop-do-always nil)
	(thereis loop-do-thereis)
	(while loop-do-while or)
	(until loop-do-while and)
	(when loop-do-when nil)
 	(unless loop-do-when t)
	(with loop-do-with)
	(for loop-do-for)
	(as loop-do-for)))

(defvar loop-for-keyword-alist			;Types of FOR
     '( (= loop-for-equals)
	(in loop-for-in)
	(on loop-for-on)
	(from loop-for-arithmetic nil)
	(downfrom loop-for-arithmetic down)
	(upfrom loop-for-arithmetic up)
	(being loop-for-being)))

(defvar loop-path-keyword-alist nil)		; PATH functions
(defvar loop-variables)				;Variables local to the loop
(defvar loop-declarations)			; Local dcls for above
(defvar loop-variable-stack)
(defvar loop-declaration-stack)
(defvar loop-prologue)				;List of forms in reverse order
(defvar loop-body)				;..
(defvar loop-after-body)			;.. for FOR steppers
(defvar loop-epilogue)				;..
(defvar loop-after-epilogue)			;So COLLECT's RETURN comes after FINALLY
(defvar loop-conditionals)			;If non-NIL, condition for next form in body
  ;The above is actually a list of entries of the form
  ;(condition forms...)
  ;When it is output, each successive condition will get
  ;nested inside the previous one, but it is not built up
  ;that way because you wouldn't be able to tell a WHEN-generated
  ;COND from a user-generated COND.

(defvar loop-when-it-variable)			;See LOOP-DO-WHEN
(defvar loop-collect-cruft)			; for multiple COLLECTs (etc)
(defvar loop-source-code)
(defvar loop-attachment-transformer		; see attachment definition
	(cond ((status feature lms) 'progn) (t nil)))

(defun loop-lookup-keyword macro (x)

     `(assq . ,(cdr x)))


(defun loop-add-keyword (cruft alist-name)
    (let ((val (symeval alist-name)) (known?))
      (and (setq known? (loop-lookup-keyword (car cruft) val))
	   (set alist-name (delq known? val)))
      (set alist-name (cons cruft val))))


(defmacro define-loop-macro (keyword)
    (or (eq keyword 'loop)
	(loop-lookup-keyword keyword loop-keyword-alist)
	(error "lisp: Not a loop keyword -- " keyword))
    `(eval-when (compile load eval)
	 (putd ',keyword '(macro (macroarg) (loop-translate macroarg)))))

(define-loop-macro loop)

(defun loop-translate (x)
     (loop-displace x (loop-translate-1 x)))


(defun loop-translate-1 (loop-source-code)
  (and (eq (car loop-source-code) 'loop)
       (setq loop-source-code (cdr loop-source-code)))
  (do ((loop-variables nil)
       (loop-declarations nil)
       (loop-variable-stack nil)
       (loop-declaration-stack nil)
       (loop-prologue nil)
       (loop-body nil)
       (loop-after-body nil)
       (loop-epilogue nil)
       (loop-after-epilogue nil)
       (loop-conditionals nil)
       (loop-when-it-variable nil)
       (loop-collect-cruft nil)
       (keyword)
       (tem))
      ((null loop-source-code)
       (and loop-conditionals
	    (error "lisp:  hanging conditional in loop macro -- "
			     (caar loop-conditionals)))
       (cond (loop-variables
	        (push loop-variables loop-variable-stack)
		(push loop-declarations loop-declaration-stack)))
       (setq tem `(prog ()
		      ,@(nreverse loop-prologue)
		   next-loop
		      ,@(nreverse loop-body)
		      ,@(nreverse loop-after-body)
		      (go next-loop)
		   end-loop
		      ,@(nreverse loop-epilogue)
		      ,@(nreverse loop-after-epilogue)))
       (do ((vars) (dcls)) ((null loop-variable-stack))
	 (setq vars (pop loop-variable-stack)
	       dcls (pop loop-declaration-stack))
	 (and dcls (setq dcls `((declare . ,(nreverse dcls)))))
	   (setq tem `(,@dcls ,tem))
	   (cond ((do ((l vars (cdr l))) ((null l) nil)
		    (and (not (atom (car l)))
			 (not (atom (caar l)))
			 (return t)))
		    (setq tem `(let ,(nreverse vars) ,.tem)))
		 (t (let ((lambda-vars nil) (lambda-vals nil))
		       (do ((l vars (cdr l)) (v)) ((null l))
			 (cond ((atom (setq v (car l)))
				  (push v lambda-vars)
				  (push nil lambda-vals))
			       (t (push (car v) lambda-vars)
				  (push (cadr v) lambda-vals))))
		       (setq tem `((lambda ,(nreverse lambda-vars) ,.tem)
				   ,.(nreverse lambda-vals))))))
	 )
       tem)
    (if (symbolp (setq keyword (pop loop-source-code)))
	(if (setq tem (loop-lookup-keyword keyword loop-keyword-alist))
	    (apply (cadr tem) (cddr tem))
	    (error "lisp:  unknown keyword in loop macro -- "
		   keyword))
	(error "lisp:  loop found object where keyword expected -- "
	       keyword))))


(defun loop-bind-block ()
   (cond ((not (null loop-variables))
	    (push loop-variables loop-variable-stack)
	    (push loop-declarations loop-declaration-stack)
	    (setq loop-variables nil loop-declarations nil))
	 (loop-declarations (break barf))))


;Get FORM argument to a keyword.  Read up to atom.  PROGNify if necessary.
(defun loop-get-form ()
  (do ((forms (list (pop loop-source-code)) (cons (pop loop-source-code) forms))
       (nextform (car loop-source-code) (car loop-source-code)))
      ((atom nextform)
       (if (null (cdr forms)) (car forms)
	   (cons 'progn (nreverse forms))))))


(defun loop-make-setq (var-or-pattern value)

    (list (if (atom var-or-pattern) 'setq 'desetq) var-or-pattern value))


(defun loop-imply-type (expression type
			&aux (frob (and (data-type? type)
					(form-wrapper type expression))))
    (cond ((not (null frob)) frob)
	  (t expression)))

(defun loop-make-variable (name initialization dtype)
  (cond ((null name)
	   (and initialization
		(push (list  nil
			    initialization)
		      loop-variables)))
	((atom name)
	   (cond ((data-type? dtype)
		    (setq loop-declarations
			  (append (variable-declarations dtype name)
				  loop-declarations))
		    (or initialization
			(setq initialization (initial-value dtype))))
		 ((memq dtype '(fixnum flonum number))
		    (or initialization
			(setq initialization (if (eq dtype 'flonum) 0.0 0)))))
	   (push (if initialization (list name initialization) name)
		 loop-variables))
	(initialization
	   (push (list name initialization) loop-variables)
	   (loop-declare-variable name dtype))
	(t (let ((tcar) (tcdr))
	      (cond ((atom dtype) (setq tcar (setq tcdr dtype)))
		    (t (setq tcar (car dtype) tcdr (cdr dtype))))
	      (loop-make-variable (car name) nil tcar)
	      (loop-make-variable (cdr name) nil tcdr))))
  name)

(defun loop-declare-variable (name dtype)
    (cond ((or (null name) (null dtype)) nil)
	  ((atom name)
	     (cond ((data-type? dtype)
		      (setq loop-declarations
			    (append (variable-declarations dtype name)
				    loop-declarations)))
		 ))
	  ((atom dtype)
	     (loop-declare-variable (car name) dtype)
	     (loop-declare-variable (cdr name) dtype))
	  (t (loop-declare-variable (car name) (car dtype))
	     (loop-declare-variable (cdr name) (cdr dtype)))))


(defun loop-maybe-bind-form (form data-type?)
    (cond ((or (numberp form) (memq form '(t nil))
	       (and (not (atom form)) (eq (car form) 'quote)))
	     form)
	  (t (loop-make-variable (gensym) form data-type?))))


(defun loop-optional-type ()
    (let ((token (car loop-source-code)))
	(and (not (null token))
	     (or (not (atom token))
		 (data-type? token)
		 (memq token '(fixnum flonum number)))
	     (pop loop-source-code))))


;Compare two "tokens".  The first is the frob out of LOOP-SOURCE-CODE,
;the second a string (lispm) or symbol (maclisp) to check against.
(defmacro loop-tequal (x1 x2) `(eq ,x1 ,x2))

;Incorporates conditional if necessary
(defun loop-emit-body (form)
  (cond (loop-conditionals
	   (rplacd (last (car (last loop-conditionals)))
		   (cond ((and (not (atom form))  ;Make into list of forms
			       (eq (car form) 'progn))
			  (append (cdr form) nil))
			 (t (list form))))
	   (cond ((loop-tequal (car loop-source-code) "and")
		    (pop loop-source-code))
		 (t ;Nest up the conditionals and output them
		    (do ((prev (car loop-conditionals) (car l))
			 (l (cdr loop-conditionals) (cdr l)))
			((null l))
		      (rplacd (last prev) `((cond ,(car l)))))
		    (push `(cond ,(car loop-conditionals)) loop-body)
		    (setq loop-conditionals nil))))
	(t (push form loop-body))))

(defun loop-do-initially ()
  (push (loop-get-form) loop-prologue))

(defun loop-do-finally ()
  (push (loop-get-form) loop-epilogue))

(defun loop-do-do ()
  (loop-emit-body (loop-get-form)))

(defun loop-do-return ()
  (loop-emit-body `(return ,(loop-get-form))))


(defun loop-do-collect (type)
  (let ((var) (form) (tem) (tail) (dtype) (cruft) (rvar)
	(ctype (cond ((memq type '(max min)) 'maxmin)
		     ((memq type '(nconc list append)) 'list)
		     ((memq type '(count sum)) 'sum)
		     (t 
			  (error
			     "lisp:  unrecognized loop collecting keyword -- "
			     type)))))
    (setq form (loop-get-form) dtype (loop-optional-type))
    (cond ((loop-tequal (car loop-source-code) 'into)
	     (pop loop-source-code)
	     (setq rvar (setq var (pop loop-source-code)))))
    ; CRUFT will be (varname ctype dtype var tail (optional tem))
    (cond ((setq cruft (assq var loop-collect-cruft))
	     (cond ((not (eq ctype (car (setq cruft (cdr cruft)))))
		        (error "lisp:  incompatible loop collections -- "
			       (list ctype (car cruft))))
		   ((and dtype (not (eq dtype (cadr cruft))))
		        (error
			   "lisp:  loop found unequal types in collector -- "
			   (list type (list dtype (cadr cruft))))))
	     (setq dtype (car (setq cruft (cdr cruft)))
		   var (car (setq cruft (cdr cruft)))
		   tail (car (setq cruft (cdr cruft)))
		   tem (cadr cruft))
	     (and (eq ctype 'maxmin)
		  (not (atom form)) (null tem)
		  (rplaca (cdr cruft) (setq tem (loop-make-variable
						   (gensym) nil dtype)))))
	  (t (and (null dtype)
		  (setq dtype (cond ((eq type 'count) 'fixnum)
				    ((memq type '(min max sum)) 'number))))
	     (or var (push `(return ,(setq var (gensym)))
			   loop-after-epilogue))
	     (loop-make-variable var nil dtype)
	     (setq tail 
		   (cond ((eq ctype 'list)
			    (setq tem (loop-make-variable (gensym) nil nil))
			    (loop-make-variable (gensym) nil nil))
			 ((eq ctype 'maxmin)
			    (or (atom form)
				(setq tem (loop-make-variable
					     (gensym) nil dtype)))
			    (loop-make-variable (gensym) nil nil))))
	     (push (list rvar ctype dtype var tail tem)
		   loop-collect-cruft)))
    (loop-emit-body
	(selectq type
	  (count (setq tem `(setq ,var (1+ ,var)))
		 (cond ((eq form t) tem) (t `(and ,form ,tem))))
	  (sum `(setq ,var (plus ,(loop-imply-type form dtype) ,var)))
	  ((max min)
	     `(setq ,@(and tem (prog1 `(,tem ,form) (setq form tem)))
		    ,var (cond (,tail (,type ,(loop-imply-type form dtype)
					     ,var))
			       (t (setq ,tail t) ,form))))
	  (list `(setq ,tem (ncons ,form)
			  ,tail (cond (,tail (cdr (rplacd ,tail ,tem)))
				      ((setq ,var ,tem))))
		 )
	  (nconc `(setq ,tem ,form
			,tail (last (cond (,tail (rplacd ,tail ,tem))
					  ((setq ,var ,tem))))))
	  (append `(setq ,tem (append ,form nil)
			 ,tail (last (cond (,tail (rplacd ,tail ,tem))
					   ((setq ,var ,tem))))))))))


(defun loop-do-while (cond)
  (loop-emit-body `(,cond ,(loop-get-form) (go end-loop))))

(defun loop-do-when (negate?)
  (let ((form (loop-get-form)) (cond))
    (cond ((loop-tequal (cadr loop-source-code) 'it)
	   ;WHEN foo RETURN IT and the like
	   (or loop-when-it-variable
	       (setq loop-when-it-variable
		     (loop-make-variable (gensym) nil nil)))
	   (setq cond `(setq ,loop-when-it-variable ,form))
	   (setq loop-source-code		;Plug in variable for IT
		 (list* (car loop-source-code)
			loop-when-it-variable
			(cddr loop-source-code))))
	  (t (setq cond form)))
    (and negate? (setq cond `(not ,cond)))
    (setq loop-conditionals (nconc loop-conditionals (ncons (list cond))))))


(defun loop-do-with ()
  (do ((var) (equals) (val) (dtype)) (nil)
    (setq var (pop loop-source-code) equals (car loop-source-code))
    (cond ((loop-tequal equals '=)
	     (pop loop-source-code)
	     (setq val (pop loop-source-code) dtype nil))
	  ((or (loop-tequal equals 'and)
	       (loop-lookup-keyword equals loop-keyword-alist))
	     (setq val nil dtype nil))
	  (t (setq dtype (pop loop-source-code)
		   equals (car loop-source-code))
	     (cond ((loop-tequal equals '=)
		      (pop loop-source-code)
		      (setq val (pop loop-source-code)))
		   ((and (not (null loop-source-code))
			 (not (loop-lookup-keyword equals loop-keyword-alist))
			 (not (loop-tequal equals 'and)))
		      (error "lisp:  loop was expecting = but found "
			     equals))
		   (t (setq val nil)))))
    (loop-make-variable var val dtype)
    (cond ((not (loop-tequal (car loop-source-code) 'and)) (return nil))
	  ((pop loop-source-code))))
  (loop-bind-block))

(defun loop-do-always (true)
  (let ((form (loop-get-form)))
    (or true (setq form `(not ,form)))
    (loop-emit-body `(or ,form (return nil)))
    (push '(return t) loop-after-epilogue)))

;THEREIS expression
;If expression evaluates non-nil, return that value.
(defun loop-do-thereis ()
   (let ((var (loop-make-variable (gensym) nil nil))
	 (expr (loop-get-form)))
      (loop-emit-body `(and (setq ,var ,expr) (return ,var)))))

;FOR variable keyword ..args.. {AND more-clauses}
;For now AND only allowed with the = keyword
(defun loop-do-for ()
  (and loop-conditionals
         (error "lisp:  loop for or as starting inside of conditional"))
  (do ((var) (data-type?) (keyword) (first-arg)
       (tem) (pretests) (posttests) (inits) (steps))
      (nil)
    (setq var (pop loop-source-code) data-type? (loop-optional-type)
	  keyword (pop loop-source-code) first-arg (pop loop-source-code))
    (and (or (not (symbolp keyword))
	     (null (setq tem (loop-lookup-keyword
			        keyword
				loop-for-keyword-alist))))
	 (error "lisp:  unknown keyword in for or as loop clause -- "
		keyword))
    (setq tem (lexpr-funcall (cadr tem) var first-arg data-type? (cddr tem)))
    (and (car tem) (push (car tem) pretests))
    (setq inits (nconc inits (append (car (setq tem (cdr tem))) nil)))
    (and (car (setq tem (cdr tem))) (push (car tem) posttests))
    (setq steps (nconc steps (append (car (setq tem (cdr tem))) nil)))
    (cond ((not (loop-tequal (car loop-source-code) 'and))
	     (cond ((cdr (setq pretests (nreverse pretests)))
		      (push 'or pretests))
		   (t (setq pretests (car pretests))))
	     (cond ((cdr (setq posttests (nreverse posttests)))
		      (push 'or posttests))
		   (t (setq posttests (car posttests))))
	     (and pretests (push `(and ,pretests (go end-loop)) loop-body))
	     (and inits (push (loop-make-psetq inits) loop-body))
	     (and posttests (push `(and ,posttests (go end-loop))
				  loop-after-body))
	     (and steps (push (loop-make-psetq steps) loop-after-body))
	     (loop-bind-block)
	     (return nil))
	  (t (pop loop-source-code)))))

(defun loop-for-equals (var val data-type?)
  (cond ((loop-tequal (car loop-source-code) 'then)
	   ;FOR var = first THEN next
	   (pop loop-source-code)
	   (loop-make-variable var val data-type?)
	   (list nil nil nil `(,var ,(loop-get-form))))
	(t (loop-make-variable var nil data-type?)
	   (list nil `(,var ,val) nil nil))))


(defun loop-for-on (var val data-type?)
  (let ((step (if (loop-tequal (car loop-source-code) 'by)
		  (progn (pop loop-source-code) (pop loop-source-code))
		  '(function cdr)))
	(var1 (cond ((not (atom var))
		       ; Destructuring?  Then we can't use VAR as the
		       ; iteration variable.
		       (loop-make-variable var nil nil)
		       (loop-make-variable (gensym) val nil))
		    (t (loop-make-variable var val nil)
		       var))))
    (setq step (cond ((or (atom step)
			  (not (memq (car step) '(quote function))))
		        `(funcall ,(loop-make-variable (gensym) step nil)
				  ,var1))
		     (t (list (cadr step) var1))))
    (list `(null ,var1) (and (not (eq var var1)) `(,var ,var1))
	  nil `(,var1 ,step))))


(defun loop-for-in (var val data-type?)
  (let ((var1 (gensym))			;VAR1 is list, VAR is element
	(step (if (loop-tequal (car loop-source-code) 'by)
		    (progn (pop loop-source-code) (pop loop-source-code))
		    '(function cdr))))
      (loop-make-variable var1 val nil)
      (loop-make-variable var nil data-type?)
      (setq step (cond ((or (atom step)
			    (not (memq (car step) '(quote function))))
			  `(funcall (loop-make-variable (gensym) step nil)
				    var1))
		       (t (list (cadr step) var1))))
      (list `(null ,var1) `(,var (car ,var1)) nil `(,var1 ,step))))


(defun loop-for-arithmetic (var val data-type? forced-direction)
  (let ((limit) (step 1) (test) (direction) (eval-to-first t) (inclusive)) 
     (do () (nil)
       (cond ((not (symbolp (car loop-source-code))) (return nil))
	     ((loop-tequal (car loop-source-code) 'by)
	      (pop loop-source-code)
	      (setq step (loop-get-form) eval-to-first t))
	     ((loop-tequal (car loop-source-code) 'to)
	      (pop loop-source-code)
	      (setq limit (loop-get-form) inclusive t eval-to-first nil))
	     ((loop-tequal (car loop-source-code) 'downto)
	      (pop loop-source-code)
	      (setq limit (loop-get-form) inclusive t
		    eval-to-first nil direction 'down))
	     ((loop-tequal (car loop-source-code) 'below)
	      (pop loop-source-code)
	      (setq limit (loop-get-form) direction 'up eval-to-first nil))
	     ((loop-tequal (car loop-source-code) 'above)
	      (pop loop-source-code)
	      (setq limit (loop-get-form) direction 'down eval-to-first nil))
	     (t (return nil))))
     (cond ((null direction) (setq direction (or forced-direction 'up)))
	   ((and forced-direction (not (eq forced-direction direction)))
	        (error "lisp:  loop variable stepping lossage with " var)))
     (or data-type? (setq data-type? 'fixnum))
     (and (eq data-type? 'flonum) (fixp step) (setq step (float step)))
     (loop-make-variable var val data-type?)
     (cond ((and limit eval-to-first)
	      (setq limit (loop-maybe-bind-form limit data-type?))))
     (setq step (loop-maybe-bind-form step data-type?))
     (cond ((and limit (not eval-to-first))
	      (setq limit (loop-maybe-bind-form limit data-type?))))
     (cond ((not (null limit))
	      (let ((z (list var limit)))
		 (setq test (cond ((eq direction 'up)
				     (cond (inclusive `(greaterp . ,z))
					   (t `(not (lessp . ,z)))))
				  (t (cond (inclusive `(lessp . ,z))
					   (t `(not (greaterp . ,z))))))))))
     (setq step (cond ((eq direction 'up)
			 (cond ((equal step 1) `(add1 ,var))
			       (t `(plus ,var ,step))))
		      ((equal step 1) `(sub1 ,var))
		      (t `(difference ,var ,step))))
     ;; The object of the following crock is to get the INTERPRETER to
     ;; do error checking.  This is only correct for data-type of FIXNUM,
     ;; since floating-point arithmetic is contagious.
     #M (and (eq data-type? 'fixnum)
	     (rplaca step (cdr (assq (car step) '((sub1 . 1-) (add1 . 1+)
						  (plus . +)
						  (difference . -))))))
     (list test nil nil `(,var ,step))))


(defun loop-for-being (var val data-type?)
   ; FOR var BEING something ... - var = VAR, something = VAL.
   ; If what passes syntactically for a pathname isn't, then
   ; we trap to the ATTACHMENTS path;  the expression which looked like
   ; a path is given as an argument to the IN preposition.  If
   ; LOOP-ATTACHMENT-TRANSFORMER is not NIL, then we call that on the
   ; "form" to get the actual form;  otherwise, we quote it.  Thus,
   ; by default, FOR var BEING EACH expr OF expr-2
   ; ==> FOR var BEING ATTACHMENTS IN 'expr OF expr-2.
   (let ((tem) (inclusive?) (ipps) (each?) (attachment))
     (cond ((loop-tequal val "each")
	      (setq each? t val (car loop-source-code)))
	   (t (push val loop-source-code)))
     (cond ((and (setq tem (loop-lookup-keyword val loop-path-keyword-alist))
		 (or each? (not (loop-tequal (cadr loop-source-code) 'and))))
	      ;; FOR var BEING {each} path {prep expr}..., but NOT
	      ;; FOR var BEING var-which-looks-like-path AND {ITS} ...
	      (pop loop-source-code))
	   (t (setq val (loop-get-form))
	      (cond ((loop-tequal (car loop-source-code) 'and)
		       ;; FOR var BEING value AND ITS path-or-ar
		       (or (null each?)
			     (error "lisp:  malformed being clause in loop of var "
			      var))
		       (setq ipps `((of ,val)) inclusive? t)
		       (pop loop-source-code)
		       (or (loop-tequal (setq tem (pop loop-source-code))
					'its)
			   (loop-tequal tem 'his)
			   (loop-tequal tem 'her)
			   (loop-tequal tem 'their)
			   (loop-tequal tem 'each)
			   (error "lisp:  loop expected its or each but found "
				  tem))
		       (cond ((setq tem (loop-lookup-keyword
					   (car loop-source-code)
					   loop-path-keyword-alist))
				(pop loop-source-code))
			     (t (push (setq attachment `(in ,(loop-get-form)))
				      ipps))))
		    ((not (setq tem (loop-lookup-keyword
				       (car loop-source-code)
				       loop-path-keyword-alist)))
		       ; FOR var BEING {each} a-r ...
		       (setq ipps (list (setq attachment (list 'in val)))))
		    (t ; FOR var BEING {each} pathname ...
		       ; Here, VAL should be just PATHNAME.
		       (pop loop-source-code)))))
     (cond ((not (null tem)))
	   ((not (setq tem (loop-lookup-keyword 'attachments
						loop-path-keyword-alist)))
	        (error "lisp:  loop trapped to attachments path illegally"))
	   (t (or attachment (break barf))
	      (rplaca (cdr attachment)
		      (cond (loop-attachment-transformer
			       (funcall loop-attachment-transformer
					(cadr attachment)))
			    (t (list 'quote (cadr attachment)))))))
     (setq tem (funcall (cadr tem) (car tem) var data-type?
			(nreconc ipps (loop-gather-preps (caddr tem)))
			inclusive? (caddr tem) (cdddr tem)))
     ;; TEM is now (bindings prologue-forms endtest setups steps)
     (mapc '(lambda (x)
	       (let (var val dtype)
		  (cond ((atom x) (setq var x))
			(t (setq var (car x) val (cadr x) dtype (caddr x))))
		  (loop-make-variable var val dtype)))
	   (car tem))
     (setq loop-prologue (nconc (reverse (cadr tem)) loop-prologue))
     (cddr tem)))


(defun loop-gather-preps (preps-allowed)
   (do ((list nil (cons (list (pop loop-source-code) (loop-get-form)) list))
	(token (car loop-source-code) (car loop-source-code)))
       ((not  (memq token preps-allowed))
	(nreverse list))))


(defun loop-add-path (name data)
    (loop-add-keyword (cons name data) 'loop-path-keyword-alist))


(defmacro define-loop-path (names &rest cruft
			    &aux forms)
   (setq forms (mapcar
		 '(lambda (name)
		     `(loop-add-path
			',name ',cruft))
		 (cond ((atom names) (list names))
		       (t names))))
   `(eval-when (eval load compile) ,@forms))


(defun loop-path-carcdr (name var dtype pps inclusive? preps data)
    preps dtype ;Prevent unused arguments error
    (let ((vars) (step) (endtest `(,(cadr data) ,var)) (tem))
       (or (setq tem (loop-lookup-keyword 'of pps))
	     (error "lisp:  loop path has no initialization -- " name))
       (setq vars `((,var ,(cond (inclusive? (cadr tem))
				 (t `(,(car data) ,(cadr tem))))
			  ,dtype)))
       (setq step `(,var (,(car data) ,var)))
       (list vars nil nil nil endtest step)))


(defun loop-interned-symbols-path (path variable data-type prep-phrases
				   inclusive? allowed-preps data)
   path data-type allowed-preps  data		; unused vars
   ; data-type should maybe be error-checked..... 
   (let ((bindings) (presteps) (pretest) (poststeps) (posttest)
	 (prologue) (indexv)  (listv) (ob)  
	 (test) (step))
     (push variable bindings)
     (and (not (null prep-phrases))
	  (or (cdr prep-phrases)
	      (and (not (loop-tequal (caar prep-phrases) 'in))
		   (not (loop-tequal (caar prep-phrases) 'of))))
	   (error
	      "Illegal prep phrase(s) in interned-symbols path --"
	      (list* variable 'being path prep-phrases)))
     (push (list (setq ob (gensym))
		 (cond ((null prep-phrases)  'obarray )
		       (t  (cadar prep-phrases))))
	   bindings)
     ; Multics lisp does not store single-char-obs in the obarray buckets.
     ; Thus, we need to iterate over the portion of the obarray
     ; containing them also.  (511. = (ascii 0))
     (push `(,(setq indexv (gensym))
	     #+Multics 639. #+(and Maclisp (not Multics)) 511. #Q 0
	     fixnum)
	      bindings)
     #M (push `(,(setq listv (gensym)) nil) bindings)
     #Q (push `(setq ,indexv (array-dimension-n 2 ,ob)) prologue)
     (setq test
	    `(and #-Multics (null ,listv)
		    #+Multics (or (> ,indexv 510.) (null ,listv))
		 (prog ()
		  lp (cond ((< (setq ,indexv (1- ,indexv)) 0) (return t))
			   ((setq ,listv (arraycall #+Multics obarray
						    #-Multics t ,ob ,indexv))
			      (return nil))
			   (t (go lp)))))
	    )
     (setq step
	   `(,variable
	       #+Multics (cond ((> ,indexv 510.) ,listv)
			       (t (prog2 nil (car ,listv)
					 (setq ,listv (cdr ,listv)))))
	       #+(and Maclisp (not Multics)) (car ,listv)
	       #+Lispm (ar-2 ,ob 1 ,indexv)))
     (cond (inclusive? (setq posttest test poststeps step
			     prologue `((setq ,variable ,ob))))
	   (t (setq pretest test presteps step)))
     #+(and Maclisp (not Multics))
       (setq poststeps `(,@poststeps ,listv (cdr ,listv)))
     (list bindings prologue pretest presteps posttest poststeps)))


; We don't want these defined in the compilation environment because
; the appropriate environment hasn't been set up.  So, we just bootstrap
; them up.
(mapc '(lambda (x)
	  (mapc '(lambda (y) (loop-add-path y (cdr x))) (car x)))
      '(((car cars) loop-path-carcdr (of) car atom)
	((cdr cdrs) loop-path-carcdr (of) cdr atom)
	((cddr cddrs) loop-path-carcdr (of) cddr null)
	((interned-symbols interned-symbol)
	   loop-interned-symbols-path (in))
	))

(or (status feature loop) (sstatus feature loop))

;Loop macro blathering.
;
;  This doc is totally wrong.  Complete documentation (nice looking
; hardcopy) is available from GSB, or from ML:LSBDOC;LPDOC (which
; needs to be run through BOLIO). 
;
;This is intended to be a cleaned-up version of PSZ's FOR package
;which is a cleaned-up version of the Interlisp CLisp FOR package.
;Note that unlike those crocks, the order of evaluation is the
;same as the textual order of the code, always.
;
;The form is introduced by the word LOOP followed by a series of clauses,
;each of which is introduced by a keyword which however need not be
;in any particular package.  Certain keywords may be made "major"
;which means they are global and macros themselves, so you could put
;them at the front of the form and omit the initial "LOOP".
;
;Each clause can generate:
;
;	Variables local to the loop.
;
;	Prologue Code.
;
;	Main Code.
;
;	Epilogue Code.
;
;Within each of the three code sections, code is always executed strictly
;in the order that the clauses were written by the user.  For parallel assignments
;and such there are special syntaxes within a clause.  The prologue is executed
;once to set up.  The main code is executed several times as the loop.  The epilogue
;is executed once after the loop terminates.
;
;The term expression means any Lisp form.  The term expression(s) means any number
;of Lisp forms, where only the first may be atomic.  It stops at the first atom
;after the first form.
;
;The following clauses exist:
;
;Prologue:
;	INITIALLY expression(s)
;		This explicitly inserts code into the prologue.  More commonly
;		code comes from variable initializations.
;
;Epilogue:
;	FINALLY expression(s)
;		This is the only way to explicitly insert code into the epilogue.
;
;Side effects:
;	DO expression(s)
;		The expressions are evaluated.  This is how you make a "body".
;		DOING is synonymous with DO.
;
;Return values:
;	RETURN expression(s)
;		The last expression is returned immediately as the value of the form.
;		This is equivalent to DO (RETURN expression) which you will
;		need to use if you want to return multiple values.
;	COLLECT expression(s)
;		The return value of the form will be a list (unless over-ridden
;		with a RETURN).  The list is formed out of the values of the
;		last expression.
;		COLLECTING is synonymous with COLLECT.
;		APPEND (or APPENDING) and NCONC (or NCONCING) can be used
;		in place of COLLECT, forming the list in the appropriate ways.
;	COUNT expression(s)
;		The return value of the form will be the number of times the
;		value of the last expression was non-NIL.
;	SUM expression(s)
;		The return value of the form will be the arithmetic sum of
;		the values of the last expression.
;     The following are a bit wierd syntactically, but Interlisp has them
;     so they must be good.
;	ALWAYS expression(s)
;		The return value will be T if the last expression is true on
;		every iteration, NIL otherwise.
;	NEVER expressions(s)
;		The return value will be T if the last expression is false on
;		every iteration, NIL otherwise.
;	THEREIS expression(s)
;		This is wierd, I'm not sure what it really does.


;		You probably want WHEN (NUMBERP X) RETURN X
;		or maybe WHEN expression RETURN IT
;
;Conditionals:  (these all affect only the main code)
;
;	WHILE expression
;		The loop terminates at this point if expression is false.
;	UNTIL expression
;		The loop terminates at this point if expression is true.
;	WHEN expression clause
;		Clause is performed only if expression is true.
;		This affects only the main-code portion of a clause
;		such as COLLECT.  Use with FOR is a little unclear.
;		IF is synonymous with WHEN.
;	WHEN expression RETURN IT (also COLLECT IT, COUNT IT, SUM IT)
;		This is a special case, the value of expression is returned if non-NIL.
;		This works by generating a temporary variable to hold
;		the value of the expression.
;	UNLESS expression clause
;		Clause is performed only if expression is false.
;
;Variables and iterations: (this is the hairy part)
;
;	WITH variable = expression {AND variable = expression}...
;		The variable is set to the expression in the prologue.
;		If several variables are chained together with AND
;		the setq's happen in parallel.  Note that all variables
;		are bound before any expressions are evaluated (unlike DO).
;
;	FOR variable = expression {AND variable = expression}...
;		At this point in the main code the variable is set to the expression.
;		Equivalent to DO (PSETQ variable expression variable expression...)
;		except that the variables are bound local to the loop.
;
;	FOR variable FROM expression TO expression {BY expression}
;		Numeric iteration.  BY defaults to 1.
;		BY and TO may be in either order.
;		If you say DOWNTO instead of TO, BY defaults to -1 and
;		the end-test is reversed.
;		If you say BELOW instead of TO or ABOVE instead of DOWNTO
;		the iteration stops before the end-value instead of after.
;		The expressions are evaluated in the prologue then the
;		variable takes on its next value at this point in the loop;
;		hair is required to win the first time around if this FOR is
;		not the first thing in the main code.
;	FOR variable IN expression
;		Iteration down members of a list.
;	FOR variable ON expression
;		Iteration down tails of a list.
;	FOR variable IN/ON expression BY expression
;		This is an Interlisp crock which looks useful.
;		FOR var ON list BY expression[var]
;			is the same as FOR var = list THEN expression[var]
;		FOR var IN list BY expression[var]
;			is similar except that var gets tails of the list
;			and, kludgiferously, the internal tail-variable
;			is substituted for var in expression.
;	FOR variable = expression THEN expression	
;		General DO-type iteration.
;	Note that all the different types of FOR clauses can be tied together
;	with AND to achieve parallel assignment.  Is this worthwhile?
;	[It's only implemented for = mode.]
;	AS is synonymous with FOR.
;	
;	FOR variable BEING expression(s) AND ITS pathname
;	FOR variable BEING expression(s) AND ITS a-r
;	FOR variable BEING {EACH} pathname {OF expression(s)} 
;	FOR variable BEING {EACH} a-r {OF expression(s)}
;		Programmable iteration facility.  Each pathname has a
;	function associated with it, on LOOP-PATH-KEYWORD-ALIST;  the
;	alist has entries of the form (pathname function prep-list).
;	prep-list is a list of allowed prepositions;  after either of
;	the above formats is parsed, then pairs of (preposition expression)
;	are collected, while preposition is in prep-list.  The expression
;	may be a progn if there are multiple prepositions before the next
;	keyword.  The function is then called with arguments of:
;	    pathnname variable prep-phrases inclusive? prep-list
;	Prep-phrases is the list of pairs collected, in order.  Inclusive?
;	is T for the first format, NIL otherwise;  it says that the init
;	value of the form takes on expression.  For the first format, the
;	list (OF expression) is pushed onto the fromt of the prep-phrases.
;	In the above examples, a-r is a form to be evaluated to get an
;	attachment-relationship.  In this case, the pathname is taken as
;	being ATTACHMENTS, and a-r is passed in by being treated as if it
;	had been used with the preposition IN.  The function should return
;	a list of the form (bindings init-form step-form end-test);  bindings
;	are stuffed onto loop-variables, init-form is initialization code,
;	step-form is step-code, and end-test tells whether or not to exit.
;
;Declarations?  Not needed by Lisp machine.  For Maclisp these will be done
;by a reserved word in front of the variable name as in PSZ's macro.
;
;The implementation is as a PROG.  No initial values are given for the
;PROG-variables.  PROG1 is used for parallel assignment.
;
;The iterating forms of FOR present a special problem.  The problem is that
;you must do everything in the order that it was written by the user, but the
;FOR-variable gets its value in a different way in the first iteration than
;in the subsequent iterations.  Note that the end-tests created by FOR have
;to be done in the appropriate order, since otherwise the next clause might get
;an error.
;
;The most general way is to introduce a flag, !FIRST-TIME, and compile the
;clause "FOR var = first TO last" as "INITIALLY (SETQ var first)
;WHEN (NOT !FIRST-TIME) DO (SETQ var (1+ var)) WHILE (<= var last)".
;However we try to optimize this by recognizing a special case:
;The special case is recognized where all FOR clauses are at the front of
;the main code; in this case if there is only one its stepping and
;endtest are moved to the end, and a jump to the endtest put at the
;front.  If there are more than one their stepping and endtests are moved
;to the end, with duplicate endtests at the front except for the last
;which doesn't need a duplicate endtest.  If FORs are embedded in the
;main code it can only be implemented by either a first-time flag or
;starting the iteration variable at a special value (initial minus step
;in the numeric iteration case).  This could probably just be regarded as
;an error.  The important thing is that it never does anything out of
;order. 

