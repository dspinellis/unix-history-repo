;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-utils.l "@(#)utils.l	5.1 (Berkeley) 5/31/85")

; FP command processor

(include specials.l)
(declare (localf u$print_fn intName pfn makeroom
		 getCmdLine) (special cmdLine codePort))

(defun get_cmd nil
  (prog (cmdLine command)
	(setq cmdLine (getCmdLine))
	(cond ((null cmdLine) (msg N "Illegal Command" N)
	       (return 'cmd$$)))
	(setq command (car cmdLine))
	(setq cmdLine (cdr cmdLine))
	(let ((cmdFn (get 'cp$ command)))
	     (cond ((null cmdFn)  (msg  N "Illegal Command" N))
		   (t (funcall cmdFn) (return 'cmd$$))))
	(return 'cmd$$)))

(defun getCmdLine nil
  (do ((names nil) (name$ nil)
       (c (tyipeek) (tyipeek)))
      ((eq c #.CR)
       (Tyi)
       (cond (name$ 
	      (nreverse (cons (implode (nreverse name$)) names)))
	     (t (nreverse names))))
      (cond ((memq c #.blankOrTab)
	     (cond (name$
		    (setq names (cons (implode (nreverse name$)) names))
		    (setq name$ nil)))
	     (Tyi))
	    
	    (t  (setq name$ (cons (Tyi) name$))))))


(defun (cp$ load) nil
  (cond (cmdLine
	 (let ((h (car cmdLine)))
	      (cond 
	       ((null (setq infile (car (errset (infile (concat h '.fp)) nil))))
		(cond 
		 ((null (setq infile (car  (errset (infile h) nil))))
		  (msg N "Can't open file" N)))))))
	(t (msg N "must supply a file" N))))



(defun (cp$ csave) nil
  (If cmdLine then
      (setq codePort (car (errset (outfile (car cmdLine)) nil)))
      (If (null codePort) then
	  (msg N "Can't open file" N)
	  
	  else
	  
	  (msg (P codePort) "(declare (special DynTraceFlg level))" N)
	  (do ((l (plist 'sources) (cddr l)))
	      
	      ((null l) (msg (P codePort) N) (close codePort))
	      
	      (apply 'pp (list '(P codePort) (concat (car l) '_fp)))
	      (msg (P codePort) N)
	      (msg (P codePort)
		   "(eval-when (load) (putprop 'sources '"
					       (cadr l)
					       " '" (car l)
					       "))" N))
	  )
      else
      
      (msg "must supply a file" N)))

(defun (cp$ fsave) nil
  (If cmdLine then
      (setq codePort (car (errset (outfile (car cmdLine)) nil)))
      (If (null codePort) then
	  (msg N "Can't open file" N)
	  
	  else
	  
	  (msg (P codePort) "(declare (special DynTraceFlg level))" N)
	  (do ((l (plist 'sources) (cddr l)))
	      
	      ((null l) (msg (P codePort) N) (close codePort))
	      
	      (let ((fName (concat (car l) '_fp)))
		   (msg (P codePort)
			N "(def " fName N (getd `,fName) ")" N))

	      (msg (P codePort)
		   "(eval-when (load) (putprop 'sources '"
					       (cadr l)
					       " '" (car l)
					       "))" N))
	  )
      else
      
      (msg "must supply a file" N)))


(defun (cp$ cload) nil
  (If cmdLine then 
      (let ((codeFile (car cmdLine)))
	   (If (probef codeFile)
	       then (load codeFile)
	       else (If (probef (concat codeFile ".o"))
			then (load (concat codeFile ".o"))
			else (msg N codeFile ": No such File" N))))
      else (msg "must supply a file" N)))


(defun (cp$ fns) nil
  (terpri)
  (let ((z (plist 'sources)))
       (cond ((null z) nil)
	     (t (do ((slist
		      (sort
		       (do ((l z (cddr l))
			    (ls nil))
			   ((null l) ls)
			   (setq ls (cons (car l)  ls)))
		       'alphalessp)
		      (cdr slist))

		     (trFns (mapcar 'extName TracedFns)))

		    ((null slist) (terpri) (terpri))

		    (let ((oldn (nwritn))
			  (fnName  (car slist)))
			 (cond ((memq fnName trFns) (setq fnName (concat
								  fnName
								  '@))))
			 (let ((nl (makeroom 80 fnName)))
			      (patom fnName)
			      (let ((vv (- 13 (mod (- (nwritn) 
						      (cond (nl 0) (t oldn))) 12))))
				   (cond ((lessp 80 (+ (nwritn) vv)) (terpri))
					 (t 
					  (mapcar 
					   '(lambda (nil) (tyo #.BLANK)) (iota$fp vv))))))))))))
(defun (cp$ pfn) nil
  (mapcar '(lambda (u) (terpri) (u$print_fn u) (terpri)) cmdLine))

(defun  u$print_fn (fn_name)
  (let ((source nil))
       (setq source (get 'sources fn_name))
       (cond ((null source) (msg fn_name  " is not defined"))
	     (t (mapcar 'p_strng (reverse source))))
       (terpri)))

(defun (cp$ save) nil
  (cond (cmdLine
	 (cond ((null (setq outfile (car (errset (outfile (car cmdLine)) nil))))
		(msg N "Can't open file" N))
	       (t (let ((poport outfile))
		       (terpri)
		       (do ((l (plist 'sources) (cddr l)))
			   ((null l) (terpri) (terpri))
			   (mapcar 'p_strng (reverse (cadr l)))
			   (terpri)
			   (terpri)))
		  (setq outfile nil))))
	(t (msg N "You must supply a file" N))))

; This is called by delete and function definition
; in case the function to be deleted is being traced.
; It handles the traced-expr property hassles.

(defun untraceDel (name)
  (let* ((fnName (concat name '_fp))
	 (tmp (get fnName 'traced-expr)))
	
	; Do nothing if fn isn't being traced
	(cond ((null tmp)) 	
	      (t (remprop fnName 'traced-expr)
		 (setq TracedFns (remove fnName TracedFns))))))

(defun (cp$ delete) nil
  (mapcar 'dfn cmdLine))

(defun dfn (fn)
  (cond ((null (get 'sources fn)) (msg fn ": No such fn" N))
	(t (remprop 'sources fn)
	   (remob (concat fn '_fp))
	   (untraceDel fn))))

(defun (cp$ timer) nil
  (let ((d (car cmdLine)))
       (cond ((eq d 'on) (setq timeIt t) 
	      (msg N "Timing applications turned on" N))
	     ((eq d 'off) (setq timeIt nil)
	      (msg N "Timing applications turned off" N))
	     (t (msg N "Bad Timing Mode" N)))
       (terpri)))

(defun (cp$ script) nil
  (let ((cmd (get 'scriptCmd (car cmdLine))))
       (cond (cmd (funcall cmd))
	     (t (msg N "Bad Script Mode" N)))
       (terpri)))
	     

(defun (scriptCmd open) nil
  (let ((nScriptName (cadr cmdLine)))
       (cond ((null  nScriptName) (msg N "No Script-file specified" N))
	     (t
	      (let ((Nptport (outfile nScriptName)))
		   (cond ((null Nptport) (msg N "Can't open Script-file" N))
			 (t (msg N  "Opening Script File" N)
			    (and ptport (close ptport))
			    (setq ptport Nptport))))))))


(defun (scriptCmd append) nil
  (let ((nScriptName (cadr cmdLine)))
       (cond (ptport (patom nScriptName ptport)))
       (let ((Nptport (outfile nScriptName 'append)))
	    (cond ((null Nptport) (msg N "Can't open Script-file" N))
		  (t (msg N "Appending to Script File" N)
		     (and ptport (close ptport))
		     (setq ptport Nptport))))))

(defun (scriptCmd close) nil
  (close ptport)
  (setq ptport nil)
  (msg N "Closing Script File" N))

(defun (cp$ help) nil
  (terpri)
  (patom "		Commands are:")
  (terpri)
  (do
   ((z (plist 'helpCmd) (cddr z)))
   ((null z)(terpri))
   (terpri)
   (patom (cadr z))))


(defun (cp$ stats) nil
  (let ((statOption (get 'statFn (car cmdLine))))
       (setq cmdLine (cdr cmdLine))
       (cond (statOption (funcall statOption))
	     (t
	      (msg N "Bad Stats Option" N)
	      (terpri)))))

(defun (statFn on) nil
  (terpri)
  (msg N "Stats collection turned on" N)
  (terpri)
  (terpri)
  (startDynStats))


(defun startDynStats nil
  (cond ((null DynTraceFlg)
	 (setq DynTraceFlg t) ; initialize DynTraceFlg
	 (setq TracedFns nil)) ; initialize TracedFns

	(t
	 (terpri)
	 (msg N "Dynamics statistic collection in progress" N)
	 (terpri))))



(defun (statFn off) nil
  (terpri)
  (msg N "Stats collection turned off" N)
  (terpri)
  (terpri)
  (stopDynStats))

(defun (statFn reset) nil
  (terpri)
  (msg N "Clearing stats" N)
  (terpri)
  (terpri)
  (clrDynStats))

(defun (statFn print) nil
  (PrintMeasures (car cmdLine)))

(defun (cp$ lisp) nil
  (break))

(defun (cp$ debug) nil
  (let ((d (car cmdLine)))
       (cond ((eq d 'on) (setq debug t) 
	      (msg N "Debug flag Set" N ))
	     ((eq d 'off) (setq debug nil)
	      (msg  N "Debug flag Reset" N))
	     (t (msg N "Bad Debug Mode" N)))
       (terpri)))

(defun (cp$ trace) nil
  (let ((mode (car cmdLine)))
       (setq cmdLine (cdr cmdLine))
       (cond ((eq mode 'on) (Trace (mapcar 'intName cmdLine)))
	     ((eq mode 'off) (Untrace (mapcar 'intName cmdLine)))
	     (t (msg N "Bad Trace Mode" N)))))

(defun intName (fName)
  (implode
   (nreverse
    (append 
     '(p f _)
     (nreverse
      (aexplodec fName))))))


; function so see if there's enought room on the line to print
; out some information.  If not then start on a new line, too
; bad if the info is longer than one line.

(defun makeroom (rMargin name)
  (cond ((greaterp (+ (flatc name 0) (nwritn)) rMargin) (msg N) t)
	(t nil)))

