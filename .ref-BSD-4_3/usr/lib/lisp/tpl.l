(setq rcs-tpl-
   "$Header: tpl.l,v 1.6 84/02/29 19:31:09 jkf Exp $")

;				-[Thu Feb 16 07:49:26 1984 by jkf]-
;

; to do
; ?state : display  status translink, *rset, displace-macros.
;		current error, prinlevel and prinlength
;	   add a way of modifying the values
; ?bk [n] : do a baktrace (default 10 frames from bottom)
; ?zo [n] : add an optional number of frames to zoom
; ?retf : return value from 'current' frame
; ?retry : retry expr in 'current' frame (required mod to lisp).
;
; the frame re-eval question is not asked when it should.
; interact with tracebreaks correctly
;
; add stepper.
; get 'debugging' to work ok.

;--- state
;
(declare (special tpl-debug-on tpl-step-on
		  tpl-top-framelist tpl-bot-framelist
		  tpl-eval-flush tpl-trace-flush
		  tpl-prinlength tpl-prinlevel
		  prinlevel prinlength top-level-print
		  tpl-commands tpl-break-level
		  tpl-spec-char
		  tpl-last-loaded
		  tpl-level
		  tpl-fcn-in-eval
		  tpl-contuab
		  ER%tpl ER%all given-history res-history
		  tpl-stack-bad tpl-stack-ok
		  tpl-history-count
		  tpl-history-show
		  tpl-dontshow-tpl
		  tpl-step-enable	;; if stepping is on
		  tpl-step-print	;; if should print step forms
		  tpl-step-triggers	;; list of fcns to enable step
		  tpl-step-countdown	;; if positive, then don't break
		  tpl-step-reclevel	;; recursion level
		  evalhook funcallhook
		  *rset % piport ^w
		  debug-error-handler
		  displace-macros
		  ))

(putd 'tpl-eval (getd 'eval))
(putd 'tpl-funcall (getd 'funcall))
(putd 'tpl-evalhook (getd 'evalhook))
(putd 'tpl-funcallhook (getd 'funcallhook))


;--- macros which should be in the system
;
(defmacro evalframe-type (evf) `(car ,evf))
(defmacro evalframe-pdl (evf)  `(cadr ,evf))
(defmacro evalframe-expr (evf) `(caddr ,evf))
(defmacro evalframe-bind (evf) `(cadddr ,evf))
(defmacro evalframe-np (evf)   `(caddddr ,evf))
(defmacro evalframe-lbot (evf) `(cadddddr ,evf))


;; messages are passed between break levels by means of catch and
;; throw:
(defmacro tpl-throw (value) `(*throw 'tpl-break-catch ,value))
(defmacro tpl-catch (form) `(*catch 'tpl-break-catch ,form))

; A tpl-catch is placed around the prompting and evaluation of forms.
; if something abnormal happens in the evaluation, a tpl-throw is done
; which then tells the break look that something special should be
; done.
;
; messages:
;   contbreak  -  this tells the break level to print out the message
;		  it prints when it is entered (such as the error message).
;		  [see poplevel message]. 
;   poplevel   -  tells the break level to jump up to the next higher
;		  break level and continue there.  It sends  contbreak
;		  message to that break level so that it will remind the
;		  user what the state is. [see cmd: ?pop ]
;   reset      -  This tells the break level to send a reset to the next
;		  higher break level.  Thus a reset is done by successive
;		  small pops.  This isn't totally necessary, but it is
;		  clean.
;  (retbreak v) - return from the break level, returning the value v.
;		  If this an error break, then we return (list v) since
;		  that is required to indicate that an error has been
;		  handled.
;  (retry v)	- instead of asking for a new value, retry the given one.
;  popretry     - take the expression that caused the current break and
;		  send a (retry expr) message to the break level above us
;		  so that it can be tried again.

(setq tpl-eval-flush nil  tpl-trace-flush nil
   tpl-prinlevel 3 tpl-prinlength 4
   tpl-spec-char #/?)

(or (boundp 'tpl-last-loaded) (setq tpl-last-loaded nil))

(defun tpl nil
   (let ((debug-error-handler 'tpl-err-all-fcn))
      (setq ER%tpl 'tpl-err-tpl-fcn)
      (putd '*break (getd 'tpl-*break))
      (setq given-history nil
	    res-history   nil
	    tpl-debug-on  nil
	    tpl-step-on   nil
	    tpl-top-framelist nil
	    tpl-bot-framelist nil
	    tpl-stack-bad t
	    tpl-stack-ok nil
	    tpl-fcn-in-eval nil
	    tpl-level nil
	    tpl-history-count 0
	    tpl-break-level -1
	    tpl-dontshow-tpl t
	    tpl-history-show 10
	    tpl-step-enable nil
	    tpl-step-countdown 0
	    tpl-step-reclevel 0)
      (do ((retv))
	  (nil)
	  (setq retv
		(tpl-catch
		   (tpl-break-function nil))))))


;--- do-one-transaction
;  do a single read-eval-print transaction
;  If eof-form is given, then we provide a prompt and read the input,
;   otherwise given is what we use, but we print the prompt and the
;   given input before evaling it again.
; (given must be in the form (sys|user ..)
;
(defun do-one-transaction (given prompt eof-form)
   (let (retv)
      (patom prompt)
      (If eof-form
	 then (setq given
		    (car (errset (ntpl-read nil eof-form))))
	      (If (eq eof-form given)
		 then (If (status isatty)
			 then (msg "EOF" N)
			      (setq given '(sys  <eof>))
			 else (exit)))
	 else (tpl-history-form-print given)
	      (terpr))
      (add-to-given-history given)
      (If (eq 'user (car given))
	 then (setq tpl-stack-bad t)
	      (setq retv
		    (if tpl-step-enable
		       then (tpl-evalhook (cdr given)
					  'tpl-do-evalhook
					  'tpl-do-funcallhook)
		       else (tpl-eval (cdr given))))
	      (setq tpl-stack-bad t)
	 else (setq retv (process-fcn (cdr given)))
	      (setq tpl-stack-bad (not tpl-stack-ok)))
      (add-to-res-history retv)
      (ntpl-print retv)
      (terpr)
      ))
		     

;; reader
; if sees a rpar as the first non space char, it just reads all chars
; return (sys . form)  where form is a list, e.g
;			)foo bar baz rets (sys foo bar baz)
;  or
;  (user . form)
; note: if nothing is typed, (sys) is returned
;
(defun ntpl-read (port eof-form)
   (let (ch)
      ; skip all spaces
      (do ()
	  ((and (not (eq (setq ch (tyipeek port)) #\space))
		(not (eq ch #\newline))))
	  (setq ch (tyi)))
      (If (eq ch #\eof)
	 then eof-form
	 else (setq ch (tyi port))
	      (If (eq ch tpl-spec-char)
		 then (do ((xx (list #\lpar) (cons (tyi) xx)))
			  ((or (eq #\eof (car xx))
			       (eq #\newline  (car xx)))
			   (cons 'sys
				 (car (errset
					 (readlist
					    (nreverse
					       (cons #\rpar (cdr xx)))))))))
		 else (untyi ch)
		      (cons 'user (read port eof-form))))))

;--- tpl-history-form-print :: the inverse of tpl-read
; this takes the history form of an expression and prints it out
; just as the user would have typed it.
;
(defun tpl-history-form-print (form)
   (If (eq 'user (car form))
      then (print (cdr form))
      else (patom "?")
	   (mapc '(lambda (x) (print x) (patom " ")) (cdr form))))

(defun ntpl-print (form)
   (cond ((and top-level-print
		(getd top-level-print))
	   (funcall top-level-print form))
	  (t (print form))))

(setq tpl-commands
   '( ((help h) tpl-command-help
       " [cmd] - print general or specific info "
       " '?help' - print a short description of all commands "
       " '?help cmd' - print extended information on the given command ")
      ( ? tpl-command-redo
	" [args] - redo last or previous command "
	" '??' - redo last user command "
	" '?? n' - (for n>0) redo command #n (as printed by ?history)"
	" '?? -n' - (for n>0) redo n'th previous command (?? -1 == ??)"
	" '?? symb' - redo last with car == symb"
	" '?? symb *' - redo last with car == symb*")
      ( (his history) tpl-command-history
	" [r] - print history list "
	" ?history, ?his - print list of commands previously executed"
	" '?his r' - print results too")
      ( (re reset) tpl-command-reset
	" - pop up to the top level"
	" '?re, ?reset', pop up to the top level ")
      ( tr tpl-command-trace
	" [fn ..] - trace"
	" '?tr' - print list of traced functions"
	" '?tr fn ...' - trace given functions, can be fn or (fn cmd ...)"
	"	where cmds are trace commands")
      ( step tpl-command-step
	" [t] [funa funb ...] step always or when specific function hit"
	" '?step t' - step starting right away "
	" '?step funa funb' - step when either funa or funb to be called ")
      ( soff tpl-command-stepoff
	" - turn off stepping "
	" '?soff' - turn off stepping ")
      ( sc tpl-command-sc
	" [n] - continue stepping [don't break for n steps] "
	" '?sc' -  do one step then break "
	" '?sc n' - step for n steps before breaking "
	"	    if n is a non integer (e.g. inf) then "
	"	    step forever without breaking ")
      ( state tpl-command-state
	" [vals] - print or change state "
	" 'state' - print current state in short form "
	" 'state l' - print state in long form"
	" 'state sym val ... ...' - set values of state "
	"	symbols are those given in 'state  l' list")
      ( prt tpl-command-prt
	" - pop up a level and retry the command which caused this break"
	" ?prt - do a ?pop followed by a retry of the command which"
	"	caused this break to be entered")
      ( ld  tpl-command-load
	" [file ...] - load given or last files"
	" 'ld'  - loads the last files loaded with ?ld"
	" 'ld file ...' - loads the given files")
      ( debug tpl-command-debug
	" [off] - toggle debug state "
	" 'debug' Turns on debugging.  When debug is on then"
	"	enough information is kept around for viewing"
	"	and quering evaluation stack"
	" 'debug off' - Turns off debuging" )
      ( fast tpl-command-fast
	" - set switches for fastest execution "
	" '?fast - turn off ?debug mode (i.e. (*rset nil)), set the "
	"	translink table to 'on', and set displace-macros to t."
	" 	This will cause franz to run as fast as possible "
	"	(but will result in loss of debugging information ")
      ( pop tpl-command-pop
	" - pop up to previous break level"
	" 'pop' - if not at top level, pop up to the break level"
	"	above this one")
      ( ret tpl-command-ret
	" [val] - return value from this break loop "
	" 'ret [val]' if this is a break look due to a break command "
	"	or a continuable error, evaluate val (default nil)"
	"	and return it to the function that found an error,"
	" 	allowing it to continue")
      
      ( zo tpl-command-zoom
	" - view a portion of evaluation stack"
	" 'zo' - show a portion above and below the 'current' stack"
	"	frame.  Use )up and )dn or alter current stack frame")
      ( dn tpl-command-down
	" [n] - go down stack frames "
	" 'dn' - move the current stack frame down one.  Down refers to"
	"	older stack frames"
	" 'dn n' - n is a fixnum telling how many stack frames to go down"
	" 'dn n z' - after going down, do a zoom"
	" After dn is done, a limited zoom will be done")
      ( up tpl-command-up
	" [n] - go up stack frames "
	" 'up' - move the current stack frame up one.  Up refers to"
	"	younger stack frames"
	" 'up n' - n is a fixnum telling how many stack frames to go up")
      ( ev tpl-command-ev
	" symbol - eval the given symbol wrt the current frame "
	" 'ev symbol' - determine the value of the given symbol"
	"	after restoring the bindings to the way they were"
	"	when the current frame was current.  see ?zo,?up,?dn")
      ( pp tpl-command-pp
	" - pretty print the current frame "
	" 'pp' - pretty print the current frame (see ?zo, ?up, ?dn)")
      ( <eof> tpl-command-pop
	" - pop one break level up "
	" '^D' - if connect to tty, pops up one break level,"
	"        otherwise, exits doesn't exit unless  "))
   )
	       
;--- process-fcn :: do a user command
;
(defun process-fcn (form)
   (let ((sel (car form)))
      (setq tpl-stack-ok nil)
      (do ((xx tpl-commands (cdr xx))
	   (this))
	  ((null xx)
	   (msg "Illegal command, type ?help for list of commands" N))
	  (If (or (and (symbolp (setq this (caar xx)))
		       (eq sel this))
		  (and (dtpr this)
		       (memq sel this)))
	      then (return (tpl-funcall (cadar xx) form))))))
			    
	      
   
;--- tpl commands
;

;--- tpl-command-help
(defun tpl-command-help (x)
   (setq tpl-stack-ok t)
   (If (cdr x)
      then (do ((xx tpl-commands (cdr xx))
		(sel (cadr x))
		(this))
	       ((null xx)
		(msg "I don't know that command" N))
	       ; look for command in tpl-commands list
	       (If (or (and (symbolp (setq this (caar xx)))
		       (eq sel this))
		  (and (dtpr this)
		       (memq sel this)))
		  then (return (do ((yy (cdddar xx) (cdr yy)))
				   ((null yy))
				   ; print all extended documentation
				   (patom (car yy))
				   (terpr)))))
      else ; print short info on all commands
	   (mapc #'(lambda (x)
		      (let ((sel (car x)))
			 ; first print selector or selectors
			 (If (dtpr sel)
			    then (patom (car sel))
				 (mapc #'(lambda (y) (patom ",") (patom y))
					(cdr sel))
			    else (patom sel))
			 ; next print documentation
			 (patom (caddr x))
			 (terpr)))
		  tpl-commands))
   nil)

(defun tpl-command-load (args)
   (setq args (cdr args))
   (If args
      then (setq tpl-last-loaded args)
	   (mapc 'load args)
    elseif tpl-last-loaded
      then (mapc 'load tpl-last-loaded)
      else (msg "Nothing to load" N)))

	      
(defun tpl-command-trace (args)
   (setq args (cdr args))
   (apply 'trace args))

	 
   
;--- tpl-command-state
;
(defun tpl-command-state (x)
   (msg " State:  debug " tpl-debug-on ", step " tpl-step-enable N)
   (msg "	  *rset " *rset ", (status translink) " (status translink) N)
   (msg "  variables: tpl-prinlength " tpl-prinlength N)
   (msg " 	      tpl-prinlevel  " tpl-prinlevel N))

;--- tpl-command-debug
;
(defun tpl-command-debug (x)
   (If (memq 'off (cdr x))
      then (*rset nil)
	   (msg "Debug is off" N)
	   (setq tpl-debug-on nil)
      else (*rset t)
	   (sstatus translink nil)
	   (msg "Debug is on" N)
	   (setq tpl-debug-on t)))

;--- tpl-command-fast
;
(defun tpl-command-fast (x)
   (*rset nil)
   (setq tpl-debug-on nil)
   (sstatus translink on)
   (setq displace-macros t))

;--- tpl-command-zoom
;
(defun tpl-command-zoom (x)
   (tpl-update-stack)
   (setq tpl-stack-ok t)
   (tpl-zoom))

(defun tpl-command-down (args)
   ;; go down the evaluation stack and zoom
   ;; down means towards older frames
   (setq tpl-stack-ok t)
   (let ((count 1))
      (If (and (fixp (cadr args)) (> (cadr args) 0))
	 then (setq count (cadr args)))
      (do ((xx count (1- xx)))
	  ((= 0 xx))
	  (If tpl-bot-framelist
	     then (setq tpl-top-framelist (cons (car tpl-bot-framelist)
						tpl-top-framelist)
			tpl-bot-framelist (cdr tpl-bot-framelist))))
      (tpl-command-zoom nil)))

(defun tpl-command-up (args)
   ;; go up the stack and zoom
   ;; up is towards more recent stuff
   ;;
   (setq tpl-stack-ok t)
   (let ((count 1))
      (If (and (fixp (cadr args)) (> (cadr args) 0))
	 then (setq count (cadr args)))
      (do ((xx count (1- xx)))
	  ((= 0 xx))
	  (If tpl-top-framelist
	     then (setq tpl-bot-framelist (cons (car tpl-top-framelist)
						tpl-bot-framelist)
			tpl-top-framelist (cdr tpl-top-framelist))))
      (tpl-command-zoom nil)))

(defun tpl-command-ev (args)
   ;; ?ev foo
   ;; determine the value of variable foo with respect to the current
   ;; evaluation frame.
   ;;
   (let ((sym (cadr args)))
      (If (not (symbolp sym))
	 then (msg "ev must be given a symbol" N)
       elseif (null tpl-bot-framelist)
	 then (msg "there is no evaluation stack, is debug on?")
	 else (prog1 (car
			(errset
			   (eval sym
				 (evalframe-bind (car tpl-bot-framelist)))))
		     (setq tpl-stack-ok t)))))


(defun tpl-command-pp (args)
   (pp-form (evalframe-expr (car tpl-bot-framelist)))
   (terpr)
   nil)

;;-- history list maintainers
;
; history lists are just lists of forms
; one for the given, and one for the returned
;
(defun most-recent-given () (car given-history))

(defun add-to-given-history (form)
   (setq given-history (cons form given-history))
   (setq res-history   (cons nil  res-history))
   (If (not (eq (car form) 'history))
       then (setq tpl-history-count (1+ tpl-history-count))))

(defun add-to-res-history (form)
   (setq res-history (cons form (cdr res-history)))
   (setq % form))

   
;--- evalframe generation
;

(defun tpl-update-stack nil
   (If tpl-stack-bad
      then (If (tpl-yorn "Should I re-calc the stack(y/n):")
	      then (tpl-gentrace)
	      else (msg "[not re-calc'ed]" N)
		   (setq tpl-stack-bad nil))))

;--- tpl-gentrace
; this is called before an function which references the
; frame list.  it needn't be called unless one knows that
; the frame status has changed
;
(defun tpl-gentrace ()
   (let ((templist (tpl-getframelist)))
      ; templist contains the frame from bottom (oldest) to top

      (setq templist (nreverse templist)) ; now youngest to oldest

      
      ; determine a new framelist and put it on the bottom list
      ; the top list is empty.  the first thing in the
      ; bottom framelist is the 'current' frame.

      ; go though frames, based on flags, flush trace calls
      ; or eval calls
      (do ((xx templist (cdr xx))
	   (remember (If tpl-dontshow-tpl then nil else t))
	   (forget-this nil nil)
	   (res)
	   (exp)
	   (flushpoint))
	  ((null xx) (setq tpl-bot-framelist (nreverse res)))
	  (setq exp (evalframe-expr (car xx)))
	  (If (dtpr exp)
	     then (If (and tpl-dontshow-tpl
			   (memq (car exp) '(tpl-eval tpl-funcall
						      tpl-evalhook
						      tpl-funcallhook)))
		     then (setq remember nil)))
	  (If (dtpr exp)
	     then (If (and tpl-dontshow-tpl (memq (car exp)
						 '(tpl-err-tpl-fcn
						     tpl-funcall-evalhook
						     tpl-do-funcallhook)))
		      then (setq forget-this t)))
	  (If (and remember (not forget-this))
	      then (setq res (cons (car xx) res)))
	  (If (dtpr exp)
	     then (If (and tpl-dontshow-tpl
			   (eq (car exp) 'tpl-break-function))
		     then (setq remember t))))

      (setq tpl-top-framelist nil)))

(defun tpl-getframelist nil
   (let ((frames)
	 temp)
      (If *rset
	 then ; Getting the first few frames is tricky because
	      ; the frames disappear quickly.
	      (setq temp (evalframe nil))	; call to setq
	      (setq temp (evalframe (evalframe-pdl temp)))
	      (do ((xx (list (evalframe (evalframe-pdl temp)))
		       (cons (evalframe (evalframe-pdl (car xx))) xx)))
		  ((null (car xx))
		   (cdr xx))))))

	       
(defun tpl-printframelist (printdown  vals count)
   (If (null vals)
      then (If printdown
	      then (msg "*** bottom ***" N)
	      else (msg "*** top ***" N))
    elseif (= 0 count)
      then (msg "... " (length vals) " more ..." N)
    else (If (not printdown)
	    then (tpl-printframelist printdown (cdr vals) (1- count)))
	 (let ((prinlevel tpl-prinlevel)
	       (prinlength tpl-prinlength))
	    ; tag apply type forms with 'a:'
	    (if (eq 'apply (evalframe-type (car vals)))
	       then (msg "a:"))
	    (print (evalframe-expr (car vals)))
	    (terpr))
	 (If printdown
	    then (tpl-printframelist printdown (cdr vals) (1- count)))))


(defun tpl-zoom nil
   (tpl-printframelist nil tpl-top-framelist 4)
   (msg "// current \\\\" N)
   (tpl-printframelist t   tpl-bot-framelist 4))

		  

(defmacro errdesc-class (err) `(car ,err))
(defmacro errdesc-id    (err) `(cadr ,err))
(defmacro errdesc-contp (err) `(caddr ,err))
(defmacro errdesc-descr (err) `(cdddr ,err))

;--- error handler
;

(defun tpl-break-function (reason)
   (do ((tpl-fcn-in-eval (most-recent-given))
	(tpl-level reason)
	(tpl-continuab)
	(tpl-break-level (1+ tpl-break-level))
	;(tpl-step-enable)
	(prompt)
	(do-retry nil nil)
	(retry-value)
	(retv 'contbreak)
	(piport nil)
	(eof-form (ncons nil)))
       (nil)
       (If (eq retv 'contbreak)
	  then
	       (If (memq (car reason) '(error derror))
		  then (if (eq (car reason) 'error)
			  then (msg "Error: ")
			  else (msg "DError: "))
		       (patom (car (errdesc-descr (cdr reason))))
		       (mapc #'(lambda (x) (patom " ") (print x))
			      (cdr (errdesc-descr (cdr reason))))
		       (terpr)
		       (msg "Form: " (cdr tpl-fcn-in-eval))
		elseif (eq 'break (car reason))
		  then (msg "Break: ")
		       (patom (cadr reason))
		       (mapc #'(lambda (x) (patom " ") (print x))
			      (cddr reason)))
	       (terpr)
	       (setq tpl-contuab (or (memq (car reason) '(break derror step))
				     (errdesc-contp (cdr reason))))
	       (setq prompt (If reason
			       then (concat (if (eq (car reason) 'derror)
					       then "d"
					     elseif (eq (car reason) 'step)
					       then "s"
					       else "")
					    (If tpl-contuab then "c" else "")
					    "{"
					    tpl-break-level
					    "} ")
			       else "=> "))
	elseif (eq retv 'reset)
	  then (tpl-throw 'reset)
	elseif (eq retv 'poplevel)
	  then (tpl-throw 'contbreak)
	elseif (eq retv 'popretry)
	  then (tpl-throw `(retry ,tpl-fcn-in-eval))
	elseif (dtpr retv)
	  then (If (eq 'retbreak (car retv))
		  then (If (eq 'error (car reason))
			  then (return (cdr retv))	; return from error
			  else (return (cadr retv)))
		  else (If (eq 'retry (car retv))
			  then (setq do-retry t
				     retry-value (cadr retv)))))
       (setq retv
	     (tpl-catch
		     (do ()
			 (nil)
			 (If (null do-retry)
			    then (do-one-transaction nil prompt eof-form)
			    else (do-one-transaction retry-value prompt nil))
			 (setq do-retry nil)
			 nil)))))

;--- tpl-err-tpl-fcn
; attached to ER%tpl, the error will return to top level
; generic error handler
;
(defun tpl-err-tpl-fcn (err)
   (let ((^w nil))
      (tpl-break-function (cons 'error err))))

;--- tpl-err-all-fcn
; attached to ER%all if (debugging t) is done.
;
(defun tpl-err-all-fcn (err)
   (let ((^w nil))
      (setq ER%all 'tpl-err-all-fcn)
      (tpl-break-function (cons 'derror err))))
   
;-- tpl-command-pop
; pop a break level
; 
(defun tpl-command-pop (x)
   (If (= 0 tpl-break-level)
      then (msg "Already at top level " N)
      else (tpl-throw 'poplevel)))

       
	   
(defun tpl-command-ret (x)
   (If tpl-contuab
      then (tpl-throw (list 'retbreak (eval (cadr x))))
      else (msg "Can't return at this point" N)))

;--- tpl-command-redo
; see documentatio above for a list of the various things this accepts
;
(defun tpl-command-redo (x)
   (setq x (cdr x))
   (If (null x)
      then (tpl-redo-by-count 1)
    elseif (fixp (car x))
      then (If (< (car x) 0)
	      then (tpl-redo-by-count (- (car x)))
	      else (If (not (< (car x) tpl-history-count))
		      then (msg "There aren't that many commands " N)
		      else (tpl-redo-by-count (- tpl-history-count (car x)))))
      else (tpl-redo-by-car x)))


;--- tpl-redo-by-car :: locate command to do by the car of the command
;
(defun tpl-redo-by-car (x)
   (let ((command (car x))
	 (substringp (If (eq (cadr x) '*) thenret)))
      (If substringp
	 then (If (not (symbolp command))
		 then (msg "must give a symbol before *" N)
		 else (let* ((string (get_pname command))
			     (len (pntlen string)))
			 (do ((xx (tpl-next-user-in-history given-history)
				  (tpl-next-user-in-history (cdr xx)))
			      (pos))
			     ((null xx)
			      (msg "Can't find a match" N))
			     (If (and (dtpr (cdar xx))
				      (symbolp (setq pos (cadar xx))))
				then (If (equal (substring pos 1 len)
						string)
					then (tpl-throw
						     `(retry ,(car xx))))))))
	 else (do ((xx (tpl-next-user-in-history given-history)
		       (tpl-next-user-in-history (cdr xx)))
		   (pos))
		  ((null xx)
		   (msg "Can't find a match" N))
		  (If (and (dtpr (cdar xx))
			   (symbolp (setq pos (cadar xx))))
		     then (If (eq pos command)
			     then (tpl-throw
					  `(retry ,(car xx)))))))))
			     
;--- tpl-redo-by-count :: redo n'th previous input
; n>=0.  if n=0, then redo last.
;
(defun tpl-redo-by-count (n)
   (do ((xx  n (1- xx))
	(list (tpl-next-user-in-history given-history)
	      (tpl-next-user-in-history (cdr list))))
       ((or (not (> xx 0)) (null list))
	(If (null list)
	   then (msg "There aren't that many commands " N)
	   else (tpl-throw `(retry ,(car list)))))))


'(defun tpl-next-user-in-history (hlist)
   (do ((histlist hlist (cdr histlist)))
       ((or (null histlist)
	    (eq 'user (caar histlist)))
	histlist)))

(defun tpl-next-user-in-history (hlist)
   hlist)

;--- tpl-command-prt
; pop and retry command which failed this time
;
(defun tpl-command-prt (x)
   (tpl-throw 'popretry))


;--- tpl-command-history
;
(defun tpl-command-history (x)
   (let (show-res)
      (If (memq 'r (cdr x))
	 then (setq show-res t))
      (tpl-command-his-rec tpl-history-show tpl-history-count show-res
			   given-history res-history)))

(defun tpl-command-his-rec (count current show-res hlist rhlist)
   (If (and hlist (> count 0))
      then (tpl-command-his-rec (1- count) (1- current) show-res
				(cdr hlist) (cdr rhlist)))
   (If hlist
      then
	   (let ((prinlevel tpl-prinlevel)
		 (prinlength tpl-prinlength))
	      (msg current ": ") (tpl-history-form-print (car hlist))
	      (terpr)
	      (If show-res
		 then (msg "% " current ": " (car rhlist) N)))))


(defun tpl-command-reset (x)
   (tpl-throw 'reset))

(defun tpl-yorn (message)
   (drain piport)
   (msg message)
   (let ((ch (tyi)))
      (drain piport)
      (eq #/y ch)))

       
;--- tpl-*break :: handle breaks
;  when tpl starts, this is put on *break's function cell
;
(defun tpl-*break (pred message)
   (let ((^w nil))
      (cond (pred (tpl-break-function (list 'break message))))))



;; stepping code
(defun tpl-command-step (args)
   (setq tpl-step-enable t
	 tpl-step-print nil
	 tpl-step-triggers nil
	 tpl-step-countdown 0)
   (if (memq t args)
      then (setq tpl-step-print t)
      else (setq tpl-step-triggers args))
   (*rset t)
   (setq evalhook nil funcallhook nil)
   (sstatus translink nil)
   (sstatus evalhook t))


(defun tpl-command-stepoff (args)
   ;; we don't turn off status evalhook because then an
   ;; evalhook would cause an error (this probably should be fixed)
   (sstatus evalhook nil)
   (setq tpl-step-enable nil
	 tpl-step-print nil))

(defun tpl-command-sc (args)
   ;; continue after step
   (if (cdr args)
      then (if (fixp (cadr args))
	      then (setq tpl-step-countdown (cadr args))
	      else (setq tpl-step-countdown 100000)))
   (tpl-throw `(retbreak ,tpl-step-enable)))

(defun tpl-do-evalhook (arg)
   ;; arg is the form to eval
   (tpl-funcall-evalhook arg 'eval))

(defun tpl-do-funcallhook (&rest args)
   ;; this is called with n args.
   ;; args 0 to n-2 are the actual arguments.
   ;; arg n-1 is the function to call (notice that it comes at the end)
   ; the list in 'args' is a fresh list, we can clobber it
   (let (name)
      ; strip the last cons cells from the args list
      ; there will be at least one element in the list,
      ; namely the function being called
      (if (cdr args)
	 then ; case of at least one argument
	      (do ((xx args (cdr xx)))
		  ((null (cddr xx))
		   (setq name (cadr xx))
		   (setf (cdr xx) nil)))
	 else ; case of zero arguments
	      (setq name (car args) args nil))
      
      (tpl-funcall-evalhook (cons name args) 'funcall)))


(defun tpl-funcall-evalhook (fform type)
   ;; function called after an evalhook or funclalhook is triggered
   ;; The form is an s-expression to be evaluated
   ;; The type is either 'eval' or 'funcall',
   ;;   eval meaning that the form is something to be eval'ed
   ;;   funcall meaning that the car of the form is the function to
   ;;	 be applied to the list which is the cdr [actually the cdr
   ;;	 is spread out on the stack and a 'funcall' is done, but this
   ;;	 is what apply does anyway.
   ;; Upon entry we optionally print, optionally break, optionally continue
   ;;	  stepping, and then optionally print the value
   ;; We print if tpl-step-print is t
   ;; We break if tpl-step-print is t and tpl-step-countdown is <= 0
   ;; We continue stepping if tpl-step-enable is t
   ;; We print the result if we continued stepping.
   ;; 
   ;; note: if it were possible to call evalhook and funcallhook if
   ;; (status evalhook) were nil, then we could make ?soff turn off
   ;; (status evalhook), making things run faster [as it is now, stepping
   ;; continues until we reach top-level again.  We just don't print
   ;; things out]
   ;;
   (let ((tpl-step-reclevel (1+ tpl-step-reclevel)))
      (if (and (not tpl-step-print)
	       (dtpr fform)
	       (memq (car fform) tpl-step-triggers))
	 then (setq tpl-step-print t))
      (if tpl-step-print
	 then (tpl-step-printform tpl-step-reclevel type fform)
	      (if (<& tpl-step-countdown 1)
		 then (setq tpl-step-enable (tpl-break-function '(step)))
		 else (setq tpl-step-countdown (1- tpl-step-countdown))))
      (if tpl-step-enable
	 then (let ((newval))
		 (setq newval (if (eq type 'eval)
				 then (tpl-evalhook fform
						    'tpl-do-evalhook
						    'tpl-do-funcallhook)
				 else (tpl-funcallhook fform
						       'tpl-do-funcallhook
						       'tpl-do-evalhook)))
		 (if tpl-step-print
		    then (tpl-step-printform tpl-step-reclevel 'r newval))
		 newval)
	 else (if (eq type 'eval)
		 then (tpl-evalhook fform nil nil)
		 else (tpl-funcallhook fform nil nil)))))
      

(defun tpl-step-printform (indent key form)
   (printblanks indent nil)
   (let ((prinlevel 4) (prinlength 4))
      (msg (if (eq key 'r)
	      then '"=="
	    elseif (eq key 'funcall)
	      then 'f:
	    elseif (eq key 'eval)
	      then 'e:
	      else key)
	   form N)))

; in order to use this: (setq user-top-level 'tpl)

	   
(putprop 'tpl t 'version)
