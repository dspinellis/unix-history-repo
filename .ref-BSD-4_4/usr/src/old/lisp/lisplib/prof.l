(setq rcs-prof-
   "$Header: /usr/lib/lisp/RCS/prof.l,v 1.2 83/03/27 18:09:22 jkf Exp $")

;; prof
;; dynamic profiler for lisp	-[Tue Mar  8 08:15:47 1983 by jkf]-
;;
;; use:
;; -> (load 'prof)	;may not be necessary if autoloading is set up
;; -> (prof-start)	; start the profiling
;;   ... do what ever you want here, but don't do a (reset) since
;;   that turns off profiling
;; -> (prof-end)	; type this when you are finished
;; -> (prof-report)	; then type this, it will list each funtion
;;			; that was called, who called this function
;;			; and who this function calls.
;;
;; prof uses the evalhook/funcallhook mechanism to get control everytime
;; a function is called.  When it gets control, it knows what function
;; is doing the calling (via the Pcaller special variable) and what
;; function is being called.  It maintains a running count for each
;; function of the functions which call it and the number of time they
;; do the calling.
;;
;; When prof-end is called, the profiling is turned off and the
;; records kept are inverted, that is for each function it is calculated
;; how many times it calls other functions.  A list describing the results
;; is created and assigned to Profreport .  When prof-report is called,
;; this record (value of Profreport) is printed in a nice human
;; readable way.
;;
;; multiple profiling runs can be made one after the other and all
;; counts will revert to zero.
;;


(declare (special Pcalledby Pcalls Pfcns Pcaller evalhook funcallhook
		  Profreport Ptotcalls Pcallcnt Profile-in-progress))

;--- prof-start :: start profiling
;
;
(defun prof-start nil
   (setq Pcalledby (gensym)	; plist tag for who calls us
	 Pcalls (gensym) 	; plist tag for who we call
	 Pfcns (list '<top-lev>) ; list of all functions encountered
	 Pcaller '<top-lev>    ; function being evaluated
	 Pcallcnt (gensym)	; plist tag for tot number of times called
	 Ptotcalls 0		; total number of function calls
	 Profile-in-progress t) ; indicate we are begin done
   (sstatus translink nil)
   (setq evalhook 'Pevalhook* funcallhook 'Pfuncallhook*)
   (*rset t)
   (msg "profiling beginning" N)
   (sstatus evalhook t)
   t)

;--- prof-end :: turn off profiling and generate result list.
;
(defun prof-end nil
   ; turn off profiling
   (sstatus evalhook nil)
   (setq evalhook nil funcallhook nil)
   (*rset nil)
   (setq Profile-in-progress nil)
   (msg (length Pfcns) " different functions called" N)
   ; generate a profile report
   ; we already know for each function, who calls that function, now
   ; we want to figure out who each function calls
   (do ((xx Pfcns (cdr xx))
	(fcn))
       ((null xx))
       (setq fcn (car xx))
       (do ((called (get fcn Pcalledby) (cdr called))
	    (callcnt 0))
	   ((null called)
	    ; save total number of times this function was called
	    (putprop fcn callcnt Pcallcnt)
	    (setq Ptotcalls (+ callcnt Ptotcalls)))
	   ; keep count of the number of time we've been called
	   (setq callcnt (+ (cdar called) callcnt))
	   ; update data on caller.
	   (putprop (caar called)
		    (cons (cons fcn (cdar called))
			  (get (caar called) Pcalls))
		    Pcalls)))

   (msg Ptotcalls " function calls made" N)
   
   ; sort by total calls to function
   (setq Pfcns (sort Pfcns 'totcallsort))

   ; generate report list, really a list of lists each one with this
   ; form:
   ;    function-name  info who-called-it number-of-times-called who-it-called
   ;
   ; the car of the report form is the total number of function calls made
   (do ((rep nil)
	(xx Pfcns (cdr xx)))
       ((null xx)(setq Profreport (cons Ptotcalls rep)))
       (setq rep (cons (list (car xx)
			     (get (car xx) 'fcn-info)
			     (get (car xx) Pcalledby)
			     (get (car xx) Pcallcnt)
			     (get (car xx) Pcalls))
		       rep)))
   'done)

(declare (special poport))

;--- prof-report :: generate a human readable version of prof report
; input: Profreport (global) : variable set by (prof-end)
;
(defun prof-report (&optional (filename nil file-p))
   (if Profile-in-progress
      then (msg "[prof-end]" N)
	   (prof-end))
   (let ((totcalls (car Profreport))
	 (poport poport))
      (cond (file-p (setq poport (outfile filename))))
      (do ((xx (cdr Profreport) (cdr xx))
	   (name ) (info) (calledby) (calls) (callcnt))
	  ((null xx))
	  (setq name     (caar xx)
		info     (cadar xx)
		calledby (caddar xx)
		callcnt  (cadddar xx)
		calls    (caddddar xx))
	  (msg ":: " name " ")
	  (pctprint callcnt totcalls)
	  (If info then (msg " - " (cutatblank (cadr info))))
	  (msg N)
	  (If calledby
	     then (msg "Called by:" N)
		  (do ((yy (sort calledby 'lesscdr) (cdr yy)))
		      ((null yy))
		      (msg "	" (cdar yy) " :: " (caar yy) N)))
	  (If calls
	     then (msg " Calls: " N)
		  (do ((yy (sort calls 'lesscdr) (cdr yy)))
		      ((null yy))
		      (msg "	" (cdar yy) " :: " (caar yy) N)))
	  (msg N N))
      (cond (file-p (close poport)))
      nil))


;--- totcallsort :: sort by number of calls and then alphabetically
;
; this is the predicate used when sorting the list of functions
; called during the profiling run.
;
(defun totcallsort (x y)
   (let ((xc (get x Pcallcnt))
	 (yc (get y Pcallcnt)))
      (If (< xc yc)
	 thenret
       elseif (= xc yc)
	 then (alphalessp x y)
	 else nil)))

;--- lesscdr :: sort by decreasing cdr's
;
(defun lesscdr (x y)
   (> (cdr x) (cdr y)))

;--- pctprint :: print fraction and then percentage
;
(defun pctprint (this tot)
   (msg this "/" tot " " (quotient (* this 100) tot) "% "))

;--- cutatblank :: cut off a string at the first blank
;
(defun cutatblank (str)
   (do ((i 1 (1+ i)))
       ((> i 50) str)
       (If (= (substringn str i 0) #\sp)
	   then (return (substring str 1 i)))))


;--- Pfuncall-evalhook* :: common code to execute when function called.
;
; this function is called whenever a funcallhook or evalhook is taken.
; arguments are the form being evaluated and the type of the form
; which is either eval or funcall.  The difference is that a funcall's
; arguments are already evaluated.  This makes no difference to us
; but it will effect how the instruction is restarted.
;
(defun Pfuncall-evalhook* (form type)
   (let (name rcd (Pcaller Pcaller))
      (If (and (dtpr form) (symbolp (setq name (car form))))
	 then (If (setq rcd (get name Pcalledby))
		 then (let ((rent (assq Pcaller rcd)))
			 (If rent
			    then (rplacd rent (1+ (cdr rent)))
			    else (putprop name
					  (cons (cons Pcaller 1)
						rcd)
					  Pcalledby)))
		 else ; function hasn't been called before, set up a
		      ; record and add its name to the function list
		      (putprop name (ncons (cons Pcaller 1)) Pcalledby)
		      (setq Pfcns (cons name Pfcns)))
	      (setq Pcaller name))
      ; now continue executing the function
      (Pcontinue-evaluation form type)))

			      


;; the functions below are taken from /usr/lib/lisp/step.l and modified
; slightly (addition of P to name)

;--- Pfuncallhook* 
;
; automatically called when a funcall is done and funcallhook*'s 
; value is the name of this function (Pfuncallhook*).  When this is
; called, a function with n-1 args is being funcalled, the args
; to the function are (arg 1) through (arg (sub1 n)), the name of
; the function is (arg n)
;
(defun Pfuncallhook* n
  (let ((name (arg n))
	(args (listify (sub1 n))))
       (Pfuncall-evalhook* (cons name args) 'funcall)))

;--- Pevalhook* 
;
; called whenever an eval is done and evalhook*'s value is the 
; name of this function (Pevalhook*).  arg is the thing being
; evaluated.
;
(defun Pevalhook* (arg)
  (Pfuncall-evalhook* arg 'eval))

(defun Pcontinue-evaluation (form type)
  (cond ((eq type 'eval) (evalhook form 'Pevalhook* 'Pfuncallhook*))
	(t (funcallhook form 'Pfuncallhook* 'Pevalhook*))))
