(setq rcs-format-
   "$Header")

;;
;;  format.l				-[Fri Mar  4 12:20:16 1983 by jkf]-
;;
;;   This is a function for printing or creating nicely formatted strings.
;; This file is a modified version of the format program which runs in
;; the mit lisps.  When converting to franz, compatibility was the
;; major goal, thus we still use the \ character as a string delimiter
;; within a command string, even though it must be doubled in Franz.
;;
;; The file contains the user callable functions:
;;   format	- lexpr for doing formated printed output or creating
;;		  strings
;;   defformat  - macro for adding a format directive
;;

; FORMAT prints several arguments according to a control argument.
; The control argument is either a string or a list of strings and lists.
; The strings and lists are interpreted consecutively.
; Strings are for the most part just printed, except that the character ~
; starts an escape sequence which directs other actions.
; A ~ escape sequence has an (optional) numeric parameter followed by a
; mode character.
; These escape actions can use up one or more of the non-control arguments.
; A list in the control-argument list is also interpreted as an escape.
; Its first element is the mode, a symbol which may be any length,
; and its remaining elements are parameters.  The list (D 5) is equivalent
; to the ~ escape "~5D";  similarly, each ~ escape has an equivalent list.
; However, there are list escapes which have no ~ equivalent.

; Any undefined list escape is simply evaluated.

;These are the escape modes which are defined:
; ~nD Takes any number and prints as a decimal integer.  If no arg,
;     print without leading spaces.  If arg and it fits in, put in leading
;     spaces; if it doesnt fit just print it.  If second arg, use that
;     (or first char of STRING of it if not a number) instead of space
;     as a pad char.
; ~nF Floating point
; ~nE Exponential notation
; ~nO Like D but octal
; ~nA Character string.  If there is an n then pad the string with spaces 
;     on the right to make it n long.  If it doesn't fit, ignore n.
; ~n,m,minpad,padcharA  Pad on the right to occupy at least
;     n columns, or if longer than that to begin with, pad to occupy
;     n+p*m columns for some nonnegative integer p.
;     at least minpad pad characters are produced in any case
;     (default if not supplied = 0).
;     padchar is used for padding purposes (default if not supplied = space).
;      if padchar is not a number, the first character in STRING of it is used.

;     A mode can actually be used to PRINC anything, not just a string.
; ~S  Prin1 an object.  Just like ~A (including parameters) but uses PRIN1.
; ~C  One character, in any acceptable form.
;      Control and meta bits print as alpha, beta, epsilon.
; ~n* Ignore the next n args.  n defaults to 1.
; ~n% Insert n newlines.  n defaults to 1.
; ~n| Insert n formfeeds.  n defaults to 1.
; ~nX Insert n spaces.  n defaults to 1.
; ~n~ Insert n tildes.  n defaults to 1.
; ~&  Perform the :FRESH-LINE operation on the stream.
; ~n,mT  Tab to column n+pm, for p an integer >= 0.
; ~Q  Apply the next arg to no arguments.
; (Q ...) Apply the next arg to the (unevaluated) parameters following the Q.
; ~P  Insert an "s", unless its argument is a 1
; ~nG  Goto the nth argument (zero based).  The next command will get that
;      argument, etc.
; ~E and ~F are not implemented.  ~T is not implemented.

; (FORMAT <stream> <control arg> <args>)
; If <stream> is NIL, cons up and return a symbol.
; If <stream> is T, use STANDARD-OUTPUT (saves typing).

;; defformat:
;;  to add a format handler, the defformat macro is used.
;; the form is (defformat code args type . body)
;;   where
;;   code is the code this will handle.  the code can be a multi
;;    character symbol, however it will have to be called with \\code\\.
;;   args is either a one or two symbol list, depending on type
;;   type is either: none, one, or many.
;;    none means that type handler will not use any argument (it may use
;;      use parameters however)
;;    one means that it takes exactly one argument
;;    many means that it may take from zero to ?? arguments.
;;   body is the body of the function.  Its return value is only important
;;    in the case of 'many' handlers since these handlers must return the
;;    list of the arguments they didn't use.
;;
;;  'none' handlers get passes a hunk which contains the parameters provide
;;	for this format directive.
;;  'one' handlers are passed the argument and the parameters.
;;  'many' handlers are passed the list of remaining arguments and the
;;	parameters. they return the arguments they don't use.

;to do:
;  3) make sure the semantics follows the lisp machine defs
;  6) do exponential (~e) floating point formats correctly.
;  7) move ferror elsewhere (near error would be a good place).
;  8) document it.
;  11) fix ~a to left justify if given correct flag
;  13) make sure that multi character directives are lower cased
;  14) make the 'x parameter work correctly
;  15) fix the english printer (wrt stream arg) and add ordinal


;;; Kludges to make MacLISP like some of the LISPM functions

(declare (special Format-Standard-Output roman-old 
		  format-params-supplied format format-handlers
		  format-sharpsign-vars))

(setq format-sharpsign-vars 'franz-symbolic-character-names)

;; format-params-supplied : numbers of parameters to format parameter
;; roman-old when t, the roman printer will print IIII instead of IV

(or (boundp 'roman-old) (setq roman-old nil))

(declare (setq defmacro-for-compiling nil defmacro-displace-call nil ))
  (defmacro nsubstring (&rest w) `(format\:nsubstring ,.w))
  (defmacro string-search-char (&rest w) `(format\:string-search-char ,.w))
  (defmacro ar-1 (ar ind) `(cxr ,ind ,ar))
  (defmacro as-1 (val ar ind) `(rplacx ,ind ,ar ,val))
  (defmacro >= (x y) `(not (< ,x ,y)))
  (defmacro <= (x y) `(not (> ,x ,y)))
  (defmacro neq (x y) `(not (= ,x ,y)))
  (defmacro pop (stack) `(prog1 (car ,stack) (setq ,stack (cdr ,stack))))
(declare (setq defmacro-for-compiling 't defmacro-displace-call 't))


(declare
 (special ctl-string		 ;The control string.
	  ctl-length		 ;string-length of ctl-string.
	  ctl-index		 ;Our current index into the control string.
				 ; Used by the conditional command. (NYI)
	  atsign-flag		 ;Modifier
	  colon-flag		 ;Modifier
	  format-temporary-area	 ;For temporary consing
	  format-arglist	 ;The original arg list, for ~G.
	  arglist-index		 ;How far we are in the current arglist
	  float-format		 ; format used when printing floats
	  poport		; franz's standard output
	  ))

(defun format (stream ctl-string &rest args)
 (let (format-string Format-Standard-Output
		     (format-arglist args)
		     (arglist-index 0))
      (setq stream (cond ((eq stream 't) poport )
			 ((null stream)
			  (setq format-string 't)
			  (list nil))
			 (t stream)))
      (setq Format-Standard-Output stream)
      (cond ((symbolp ctl-string)
	     (setq ctl-string (get_pname ctl-string))))
      (cond ((stringp ctl-string)
	     (format-ctl-string args ctl-string 0 (pntlen ctl-string)))
	    (t (do ((ctl-string ctl-string (cdr ctl-string)))
		   ((null ctl-string))
		   (setq args
			 (cond ((symbolp (car ctl-string))
				(format-ctl-string
				 args
				 (car ctl-string)
				 0
				 (pntlen (car ctl-string))))
			       (t (format-ctl-list args (car ctl-string))))))))
      (and format-string
	   (setq format-string (maknam (nreverse (cdr stream)))))
      format-string))

(defun format-ctl-list (args ctl-list)
       (format-ctl-op (car ctl-list) args (cdr ctl-list)))

(defun format-ctl-string (args ctl-string ctl-index ctl-length)
    (declare (fixnum ctl-index ctl-length))
    (do   ((ch) (tem) (str) (sym))
	  ((>= ctl-index ctl-length) args)
	(setq tem (cond ((string-search-char #/~ ctl-string ctl-index))
			(t ctl-length)))
	(cond ((neq tem ctl-index)		;Put out some literal string
	       (do n ctl-index n (>= n tem)
		   (format:tyo (substringn ctl-string (setq n (1+ n)) 0)))
	       (and (>= (setq ctl-index tem) ctl-length)
		    (return args))))
	;; (ar-1 ch ctl-index) is a tilde.
	(do ((atsign-flag nil)	;Modifier
	     (colon-flag nil)	;Modifier
	     (params (makhunk 10))
	     (param-leader -1)
			        ;PARAMS contains the list of numeric parameters
	     (param-flag nil)	;If T, a parameter has been started in PARAM
	     (param))		;PARAM is the parameter currently
				; being constructed
	    ((>= (setq ctl-index (1+ ctl-index)) ctl-length))
	  (setq ch (getcharn ctl-string (1+ ctl-index)))
	  (cond ((and (>= ch #/0) (<= ch #/9))		 	 ; 
		 (setq param (+ (* (or param 0) 10.) (- ch #/0)) ; 
		     param-flag t))
		((= ch #/@)					 ;ascii @
		 (setq atsign-flag t))
		((= ch #/:)					 ;ascii :
		 (setq colon-flag t))
		((or (= ch #/v) (= ch #/V))			;ascii v, v
		 (as-1 (pop args) params
		       (setq param-leader (1+ param-leader)))
		 (setq arglist-index (1+ arglist-index))
		 (setq param nil param-flag nil))
		((= ch #/#)
		 (as-1 (length args) params
		       (setq param-leader (1+ param-leader))))
		((= ch #/,)
		   ;comma, begin another parameter, ascii ,
		 (and param-flag (as-1 param params (setq param-leader
							  (1+ param-leader))))
		 (setq param nil param-flag t))
		  ;omitted arguments made manifest by the
		  ;presence of a comma come through as nil
		(t		;must be a command character
		    ;upper case to lower 
		  (and (>= ch #/A) (<= ch #/Z) (setq ch (+ ch (- #/a #/A))))
		  (setq ctl-index (1+ ctl-index)) ;advance past command char
		  (and param-flag (as-1 param params (setq param-leader
							   (1+ param-leader))))
		  (setq param-flag nil param nil tem nil)
		   ;str gets a string which is the name of the operation to do
		  (setq
		    str (cond ((= ch #/\ )			 ;ascii \
			       (let ((i (string-search-char
					   #/\
					   ctl-string
					   (1+ ctl-index))))
				  (and (null i)
				       (ferror nil
					  '|Unmatched \\ in control string.|))
				  (prog1 ; don't uppercase! we are a two
				     ; case system
				     (setq tem
					   (nsubstring ctl-string
						       (1+ ctl-index)
						       i))
				     (setq ctl-index i))))
			      ;makes ~<newline> work!  ;SMH@EMS
			      ((= ch #\newline)
			       #.(list 'quote (concat "ch" #\newline)))
			      (t (ascii ch))))
		   ;; SYM gets the symbol corresponding to STR
		  (cond ((setq sym str)
			 (setq format-params-supplied param-leader)
			 (setq args (format-ctl-op sym args params)))
			(t (ferror nil '|~C is an unknown FORMAT op in \"~A\"|
				   tem ctl-string)))
		  (return nil))))))

;Perform a single formatted output operation on specified args.
;Return the remaining args not used up by the operation.
(defun format-ctl-op (op args params &aux tem)
   (cond ((stringp op) (setq op (concat op))))  ; make into a symbol
   (cond ((setq tem (assq op format-handlers))
	  (cond ((eq 'one (cadr tem))
		 (or args
		     (ferror nil "arg required for ~a, but no more args" op))
		 (funcall (caddr tem) (car args) params)
		 (setq arglist-index (1+ arglist-index))
		 (cdr args))
		((eq 'none (cadr tem))
		 (funcall (caddr tem) params)
		 args)
		((eq 'many (cadr tem))
		 (funcall (caddr tem) args params))
		(t (ferror nil "Illegal format handler: ~s" tem))))
	 (t (ferror nil '|\"~S\" is not defined as a FORMAT command.| op)
	    args)))

(setq format-handlers nil)
;; Format handlers
;;
(defmacro defformat (name arglist type &rest body)
   (let (newname)
      ;; allow the name to be the fixnum rep of a character too.
      (cond ((fixp name) (setq name (concat "ch" name))))
      
      (cond ((not (memq type '(none one many)))
	     (ferror nil "The format type, \"~a\" is not: none, one or many"
		     type)))
      (cond ((or (not (symbolp name))
		 (not (dtpr arglist)))
	     (ferror nil "Bad form for name and/or arglist: ~a ~a"
		     name arglist)))
      (cond ((memq type '(one many))
	     (cond ((not (= (length arglist) 2))
		    (ferror nil "There should be 2 arguments to ~a" name))))
	    (t (cond ((not (= (length arglist) 1))
		      (ferror nil "There should be 1 argument to ~a" name)))))
      (setq newname (concat name ":format-handler"))
      `(progn 'compile
	      (defun ,newname ,arglist ,@body)
	      (let ((handler (assq ',name format-handlers)))
		 (cond (handler (rplaca (cddr handler) ',newname))
		       (t (setq format-handlers (cons (list ',name
							    ',type
							    ',newname)
						      format-handlers))))))))



(defformat d (arg params) one
   (let ((width (cxr 0 params))
	 (padchar (cxr 1 params)))
    (cond ((and colon-flag (< arg 4000.) (> arg 0))
	   (roman-step arg 0))
	  (atsign-flag (english-print arg 'cardinal))
	  ((let ((base 10.) (*nopoint t))
	     (cond ((null padchar) (setq padchar 32.))
		   ((not (numberp padchar))
		    (setq padchar (getcharn padchar 1))))
	     (and width (format-ctl-justify width (flatc arg) padchar))
	     (format:patom arg))))))

(defformat f (arg params) one
   (cond ((not (floatp arg)) (format:patom arg))
	 (t (let ((float-format "%.16g")
		  (prec (cxr 0 params)))
	       (cond ((and prec (fixp prec) (> prec 0) (< prec 16))
		      (setq float-format (concat "%" prec "g"))))
	       (format:patom arg)))))

; r format
; no params and flags: print as cardinal (four)
; no params and colon: print as ordinal  (fourth)
; no params and atsign: print as roman (IV)
; no params and colon and atsign: print as old roman (IIII)
; params:  radix,mincol[0],padchar[<space>]
;		print in radix with at least mincol columns, padded on left
;		with padchar
;
(defformat r (arg params) one
   (format:anyradix-printer arg params nil))

; o format - like ~8r, but params are like ~d.
;
(defformat o (arg params) one
   (format:anyradix-printer arg params 8.))

(defun format:anyradix-printer (arg params radix)
   ; this is called by ~r and ~o.  for ~r, the mincol parameter starts at
   ; cxr 1, for ~o the mincol parameter starts at cxr 0.  We compute
   ; paramstart as either 0 or 1
   ; radix is given as third argument iff this is ~o
   (let ((paramstart (cond (radix 0)
			   (t 1))))
      (cond ((null radix) (setq radix (cxr 0 params))))
      (cond ((null radix)	; if not to any given base
	     (cond ((and (null colon-flag) (null atsign-flag))
		    (english-print arg 'cardinal))
		   ((and colon-flag (null atsign-flag))
		    (english-print arg 'ordinal))
		   ((and (null colon-flag) atsign-flag)
		    (roman-step arg 0))
		   ((and colon-flag atsign-flag)
		    (let ((roman-old t))
		       (roman-step arg 0)))))
	    (t (let ((mincol (cxr paramstart params))
		     (padchr (or (cxr (+ 1 paramstart) params) #\space))
		     (res))
		  (cond (mincol 	;; if mincol specified
			   (let ((Format-Standard-Output (list nil)))
			      (format-binpr arg radix)
			      (setq res (cdr Format-Standard-Output)))
			   (format-ctl-justify mincol (length res) padchr)
			   (mapc 'format:tyo (nreverse res)))
			(t (format-binpr arg radix))))))))
	       

(defun format-binpr (x base)
   (cond ((equal x 0)(format:patom 0))
	 ((or (> base 36.) (< base 2))
	  (ferror nil "\"~s\" is not a base between 2 and 36" base))
	 ((lessp x 0)
	  (format:patom '-)
	  (format-binpr1 (minus x) base))
	 (t (format-binpr1 x base)))
   x)



(defun format-binpr1 (x base)
   (cond ((equal x 0))
	 (t (format-binpr1 (quotient x base) base)
	    (format-prc (remainder x base)))))

(defun format-prc (x)
   (cond ((< x 10.) (format:patom x))
	 (t (format:tyo (plus (- #/a 10.) x)))))
			; works for 10.=A, 35.=Z.

;; must get the width stuff to work!!
(defun format-ctl-octal (arg params)
   (let ((width (cxr 0 params)) (padchar (cxr 1 params)))
      (let ((base 8))
	 (cond ((null padchar)
		(setq padchar 32.))
	       ((not (numberp padchar))
		(setq padchar (getcharn padchar 1))))
	 (and width (format-ctl-justify width (flatc arg) padchar))
	 (format:patom arg))))

(defformat a (arg params) one
   (format-ctl-ascii arg params nil))

(defun format-ctl-ascii (arg params prin1p)
    (let ((edge (cxr 0 params))
	  (period (cxr 1 params))
          (min (cxr 2 params))
	  (padchar (cxr 3 params)))
	 (cond ((null padchar)
		(setq padchar #\space))
	       ((not (numberp padchar))
		(setq padchar (getcharn padchar 1))))
         (cond (prin1p (format:print arg))
               (t (format:patom arg)))
	 (cond ((not (null edge))
		(let ((width (cond (prin1p (flatsize arg)) ((flatc arg)))))
		  (cond ((not (null min))
			 (format-ctl-repeat-char min padchar)
			 (setq width (+ width min))))
		  (cond (period
			 (format-ctl-repeat-char
			  (- (+ edge (* (\\ (+ (- (max edge width) edge 1)
					       period)
					    period)
					period))
			     width)
			  padchar))
			(t (format-ctl-justify edge width padchar))))))))

(defformat s (arg params) one
    (format-ctl-ascii arg params t))

(defformat c (arg params) one
   (cond ((or (not (fixp arg))
	      (< arg 0)
	      (> arg 127))
	  (ferror nil "~s is not a legal character value" arg)))
   (cond ((and (not colon-flag) (not atsign-flag))
	  ; just print out the character after converting to ascii
	  (format:patom (ascii arg)))
	 (t ; it may have an extended name, check for that first
	    (let (name)
	       (cond ((setq name (car
				    (rassq arg (symeval format-sharpsign-vars))))
		      ; it has an extended name.
		      ; if : flag, then print in human readable
		      (cond (colon-flag (format:patom name))
			    (atsign-flag (format:patom "#\\")
					 (format:patom name))))
		     ((< arg #\space)
		      ; convert from control to upper case
		      (setq arg (+ arg #/@))
		      (cond (colon-flag (format:patom "^")
					(format:patom (ascii arg)))
			    (atsign-flag (format:patom "#^")
					 (format:patom (ascii arg)))))
		     (t (cond (colon-flag (format:patom (ascii arg)))
			      (atsign-flag (format:patom "#/")
					   (format:patom (ascii arg))))))))))

(defformat p (args params) many
  (let (arg)
    (cond (colon-flag
	   (setq arg (nth (1- arglist-index) format-arglist)))
	  ((null args)
	   (ferror () "Argument required for p, but no more arguments"))
	  (t (setq arg (pop args)
		   arglist-index (1+ arglist-index))))
    (if (= arg 1)
	(if atsign-flag (format:tyo #/y))
	(cond (atsign-flag
	       (format:tyo #/i)
	       (format:tyo #/e)
	       (format:tyo #/s))
	      (t (format:tyo #/s))))
    args))

(defformat * (args params) many
  (let ((count (or (cxr 0 params) 1)))
    (if colon-flag (setq count (minus count)))
    (setq arglist-index (+ arglist-index count))
;;  (nthcdr count format-arglist)		;; ??? SMH@EMS
    (nthcdr arglist-index format-arglist)))	;; SMH@EMS

(defformat g (arg params) many
       (let ((count (or (cxr 0 params) 1)))
	    (nthcdr count format-arglist)))

(defformat % (params) none
       (declare (fixnum i))
       (let ((count (or (cxr 0 params) 1)))
	    (do i 0 (1+ i) (= i count)
		(format:terpr))))

;  ~ at the end of the line
;  no params: ignore newline and following whitespace
;  @ flag: leave the newline in the string but ignore whitespace
;  : flag: ignore newline but leave the whitespace
;  :@ flags: leave both newline and whitespace
;
(defformat #\newline (params) none
   (cond (atsign-flag
	    (format:tyo #\newline)))
   (cond ((not colon-flag)
	  (setq ctl-index (1+ ctl-index))
	  (do ()
	      ((>= ctl-index ctl-length))
	      (cond ((memq (getcharn ctl-string ctl-index)
			   '(#\space #\tab))
		     (setq ctl-index (1+ ctl-index)))
		    (t (setq ctl-index (1- ctl-index))
		       (return)))))))

		 
(defformat & (params) none
    (format:fresh-line))

(defformat x (params) none
   (format-ctl-repeat-char (cxr 0 params) #\space))

(defformat \| (params) none
   (format-ctl-repeat-char (cxr 0 params) #\ff))
   
(defformat ~ (params) none
   (format-ctl-repeat-char (cxr 0 params) #/~))

(defun format-ctl-repeat-char (count char)
    (declare (fixnum i))
    (cond ((null count) (setq count 1)))
    (do i 0 (1+ i) (=& i count)
	(format:tyo char)))

;; Several commands have a SIZE long object which they must print
;; in a WIDTH wide field.  If WIDTH is specified and is greater than
;; the SIZE of the thing to be printed, this put out the right
;; number of  CHARs to fill the field.  You can call this before
;; or after printing the thing, to get leading or trailing padding.
(defun format-ctl-justify (width size &optional (char #\space))
    (and width (> width size) (format-ctl-repeat-char (- width size) char)))

(defformat q (arg params) one
   ;; convert params given to a list
   (do ((ii format-params-supplied (1- ii))
	(params-given nil))
       ((< ii 0) (apply arg params-given))
       (setq params-given (cons (cxr ii params) params-given))))

;; Fixed nested ~[ ~] parser to handle ~:[ ~] and ~@:[ ~] as well.  SMH@EMS
(defun case-scan (goal &optional (lim ctl-length) &aux flag)
 (declare (fixnum lim ctl-index flag))
 (do ((ch))
     ((>= ctl-index lim) nil)
     (setq ch (getcharn ctl-string (setq ctl-index (1+ ctl-index))))
     (cond ((= ch #/~)
	    (setq flag 0)
	    (do nil (nil)
		(setq ch (getcharn ctl-string (setq ctl-index (1+ ctl-index))))
		(cond ((= ch #/:) (setq flag (+ flag 2)))
		      ((= ch #/@) (setq flag (+ flag 1)))
		      (t (return nil))))
	    (cond ((= ch goal) (return flag))
		  ((= ch #/[) (case-scan #/] lim)))))))

; [ format
;  the case selector is the first parameter given, and if no  parameter
;  is given, then it is the next argument
;
(defformat \[ (args params) many	; ]
 (let ((start ctl-index)
       (num (cond ((> format-params-supplied -1)
		   (cxr 0 params))
		  (t (cond ((null args)
			    (error "the [ format requires an argument")))
		     (prog1 (car args)
			    (setq args (cdr args))
			    (setq arglist-index (1+ arglist-index)))))))
      (and colon-flag (setq num (cond (num 1) (t 0))))
      (and (null num)
	   (ferror nil
		   "The FORMAT \"[\" command requires a numeric parameter"))
      (or (case-scan		; [
	   #/])
	  (ferror nil "Unbalanced \"[\" in FORMAT control string"))
      (let ((i ctl-index) (tmp))
	   (setq ctl-index start)
	   (do n num (1- n) (= n 0)
	       (setq tmp (case-scan #/;
				    i))
	       (cond ((null tmp) (return nil))
		     ((and (numberp tmp)
			   (>= tmp 2))
		      (return nil)))))
      args))

(defformat \] (params) none nil)

(defformat \; (params) none
   (case-scan #/]))

(defformat \{ (args params) many
 (let ((loop-times (or (cxr 0 params) -1))
       (loop-string) (loop-start) (loop-length)
       (at-least-once nil)
       (return-args)
       (return-format-arglist)
       (return-arglist-index))
      (do ((i (format:string-search-char #/~ ctl-string ctl-index)
	      (format:string-search-char #/~ ctl-string (1+ i))))
	  ((or (null i) (= (1+ i) ctl-length))
	   (ferror () "No matching \"}\" for \"{\" in format"))
	  (cond ((= #/} (getcharn ctl-string (+ 2 i)))
		 (setq loop-start ctl-index
		       loop-length i
		       ctl-index (+ 2 i))
		 (return t))
		((and (= #/: (getcharn ctl-string (+ 2 i)))
		      (= #/} (getcharn ctl-string (+ 3 i))))
		 (setq loop-start ctl-index
		       loop-length i
		       ctl-index (+ 3 i)
		       at-least-once t)
		 (return t))))
      (if (= 0 loop-length)
	  (setq loop-string (pop args)
		arglist-index (1+ arglist-index)
		loop-start 0
		loop-length (pntlen loop-string))
	  (setq loop-string ctl-string))
      (if (null atsign-flag)
	  (setq return-args (cdr args)
		return-arglist-index arglist-index
		arglist-index 0
		return-format-arglist format-arglist
		format-arglist (car args)
		args format-arglist))
      (*catch '(loop-stop loop-abort)
	      (do ((i loop-times (1- i)))
		  ((and (null at-least-once)
			(or (null args) (= i 0))))
		  (setq at-least-once nil)
		  (cond ((null colon-flag)
			 (setq args (format-ctl-string args
						       loop-string
						       loop-start
						       loop-length)))
			(t (*catch 'loop-stop
				   (format-ctl-string (car args)
						      loop-string
						      loop-start
						      loop-length))
			   (setq args (cdr args)
				 arglist-index (1+ arglist-index))))))
      (cond (return-arglist-index
	     (setq args return-args
		   arglist-index (1+ return-arglist-index)
		   format-arglist return-format-arglist)))
      args))

(defformat \} (params) none nil)

(defformat \^ (args params) many
  (let ((terminate nil))
    (cond ((null (cxr 0 params))
	   (setq terminate (null args)))
	  ((null (cxr 1 params))
	   (setq terminate (zerop (cxr 0 params))))
	  ((null (cxr 2 params))
	   (setq terminate (equal (cxr 1 params) (cxr 0 params))))
	  (t (setq terminate (and (< (cxr 0 params) (cxr 1 params))
				  (< (cxr 1 params) (cxr 2 params))))))
    (if terminate
	(if colon-flag (*throw 'loop-abort t) (*throw 'loop-stop t))
	args))) 


(declare (special english-small english-medium english-large))

(defun make-list-array (list)
   (let ((a (makhunk (length list))))
      (do ((i 0 (1+ i))
	   (ll list (cdr ll)))
	  ((null ll))
	  (rplacx i a (car ll)))
      a))

(setq english-small
   (make-list-array '(|one| |two| |three| |four| |five| |six|
			    |seven| |eight| |nine| |ten| |eleven| |twelve|
			    |thirteen| |fourteen| |fifteen| |sixteen|
			    |seventeen| |eighteen| |nineteen|)))

(setq english-medium
   (make-list-array '(|twenty| |thirty| |forty| |fifty| |sixty| |seventy|
			       |eighty| |ninty|)))

(setq english-large
   (make-list-array '(|thousand| |million| |billion| |trillion| |quadrillion|
				 |quintillion|)))


(defun english-print (n type)
       (declare (fixnum i n limit))
       (cond ((zerop n)
	      (cond ((eq type 'cardinal) (format:patom "zero"))
		    (t (format:patom "zeroth"))))
	     ((< n 0)
	      (format:patom '|minus|)
	      (format:tyo #\space)
	      (english-print (minus n) type))
	     (t
	      (do ((n n)
                   (p)
		   (flag)
		   (limit 1000000.
			  (quotient limit 1000.))
		   (i 1 (1- i)))
		  ((< i 0)
		   (cond ((> n 0)
			  (and flag (format:tyo #\space))
			  (english-print-thousand n))))
		  (cond ((not (< n limit))
			 (setq p (quotient n limit)
			       n (remainder n limit))
			 (cond (flag (format:tyo #\space))
			       (t (setq flag t)))
			 (english-print-thousand p)
			 (format:tyo #\space)
			 (format:patom (ar-1 english-large i))))))))

(defun english-print-thousand (n)
       (declare (fixnum i n limit))
       (let ((n (remainder n 100.))
	     (h (quotient n 100.)))
	    (cond ((> h 0)
		   (format:patom (ar-1 english-small (1- h)))
		   (format:tyo #\space)
		   (format:patom '|hundred|)
		   (and (> n 0) (format:tyo #\space))))
	    (cond ((= n 0))
		  ((< n 20.)
		   (format:patom (ar-1 english-small (1- n))))
		  (t
		   (format:patom (ar-1 english-medium
						   (- (quotient n 10.) 2)))
		   (cond ((zerop (setq h (remainder n 10.))))
			 (t
			  (format:tyo #/-) ;ascii -
			  (format:patom (ar-1 english-small (1- h)))))))))

(defun roman-step (x n)
    (cond ((> x 9.)
	   (roman-step (quotient x 10.) (1+ n))
	   (setq x (remainder  x 10.))))
    (cond ((and (= x 9) (not roman-old))
	   (roman-char 0 n)
	   (roman-char 0 (1+ n)))
	  ((= x 5)
	   (roman-char 1 n))
	  ((and (= x 4) (not roman-old))
	   (roman-char 0 n)
	   (roman-char 1 n))
	  (t (cond ((> x 5)
		    (roman-char 1 n)
		    (setq x (- x 5))))
	     (do i 0 (1+ i) (>= i x)
	       (roman-char 0 n)))))

(defun roman-char (i x)
    (format:tyo (car (nthcdr (+ i x x) '(#/I #/V #/X #/L #/C #/D #/M)))
	   		 ;  i   v   x   l   c   d   m
))

;;; Kludges to make MacLISP like some of the LISPM functions


(defun format:tyo (char)
   (cond ((dtpr Format-Standard-Output)
	  (rplacd Format-Standard-Output
		  (cons char (cdr Format-Standard-Output))))
	 (t (tyo char Format-Standard-Output))))

(defun format:patom (arg)
   (format:printorpatom arg nil))

(defun format:print (arg)
   (format:printorpatom arg t))

(defun format:printorpatom (argument slashify)
   (cond ((dtpr Format-Standard-Output)
	  (rplacd Format-Standard-Output
		  (nreconc (cond (slashify
					 (mapcar '(lambda (x)
						     (getcharn x 1))
					         (explode argument)))
					((exploden argument)))
				  (cdr Format-Standard-Output))))
	 (t (cond (slashify (print argument Format-Standard-Output))
		  (t (patom argument Format-Standard-Output))))))

(defun format:terpr nil
   (cond ((dtpr Format-Standard-Output)
	  (rplacd Format-Standard-Output
		  (cons #\newline (cdr Format-Standard-Output))))
	 (t (terpr Format-Standard-Output))))

(defun format:fresh-line nil
   (cond ((dtpr Format-Standard-Output)
	  (cond ((and (cdr Format-Standard-Output)
		      (not (= (cadr Format-Standard-Output) #\newline)))
		 (rplacd Format-Standard-Output
			 (cons #\newline (cdr Format-Standard-Output))))))
	 (t (and (not (= 0 (nwritn Format-Standard-Output)))
		 (terpr Format-Standard-Output)))))
   
	  


(defun format\:string-search-char (char str start-pos)
       (declare (fixnum i start-pos str-len))
       (do ((i start-pos (1+ i))
	    (str-len (flatc str)))
	   ((>& i str-len) nil)
	   (and (=& char (getcharn str (1+ i))) (return i))))

(defun format\:nsubstring (str from to)
       (declare (fixnum i from to))
       (substring str (+ 1 from) (- to from)))  ;substring is 1 based

(defun ferror (&rest args)
   (let (str)
      ; if the first arg to ferror is a string we assume that it is the
      ; format control string, otherwise we assume that it is a port
      ; specification, and we ignore it since we want to build a string.
      (if (stringp (car args))
	 then (setq str (lexpr-funcall 'format nil args))
	 else (setq str (lexpr-funcall 'format nil (cdr args))))
      (error str)))


(defun format-test nil
   (format t "Start test, newline:~%freshline:~&")
   (format t "decimal:~d, width=5:~5d~%" 10 10)
   (format t "decimal pad with period:~10,vd~%" #/.  12)
   (format t "char normal:~c, as # would read:~@c, human read:~:c~%"
	   #\space #\space #\space)
   (format t "cardinal:~r, roman new:~@r, roman-old:~:@r~
   		<same line I hope>~@
		new line but at beginning~:
   same line, but spaced out~:@
   	new line and over two tabs~%" 4 4 4))

(putprop 'format t 'version)
