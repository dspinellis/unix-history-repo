;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-fpMain.l "@(#)fpMain.l	5.1 (Berkeley) 5/31/85")

; Main routine to start up FP

(include specials.l)
(declare (special arg parse_tree)
  (localf syntaxErr synErrMsg last_cr p_indic display rtime doExit)
  )

; may ask for debug output,
; specifiy character set, only ASCII (asc) supported at this time.
; exit to shell if invoked  from it.

(defun fpMain (debug from_shell)	
  
  (do ((arg nil)
       (parse_tree (*catch '(parse$err end_condit end_while)  (parse 'top_lev))
		   (*catch '(parse$err  end_condit end_while) (parse 'top_lev))))
      
      ; exit if an EOF has been entered from the terminal
      ; (and it was the only character entered on the line)
      
      ((and (eq parse_tree 'eof$$) (null infile))
       (terpri) 
       (doExit from_shell))	 ; in any case exit
      
      ; if the EOF was from a file close it and then accept
      ; input from terminal again
      
      (cond 
       ((not (eq parse_tree 'eof$$))
	(cond (debug (print parse_tree) 
		     (terpri)))
	(cond
	 ((not (eq parse_tree 'cmd$$))
	  (cond 
	   ((not (listp parse_tree))
	    (let
	     ((defn (put_fn fn_name parse_tree)))	; define the function
	     (cond (in_def
		    (patom "{")
		    (patom (setq usr_fn_name
				 (implode 
				  (nreverse (cdddr (nreverse (explode fn_name)))))))
		    (patom "}") (terpri)
		    (putprop 'sources in_buf usr_fn_name)))
	     (cond ((and debug in_def) (pp fn_name))))
	    
	    ; read in an FP sequence once a colon (apply) has been detected
	    
	    (cond ((not in_def)
		   (cond ((and (null infile) ptport)
			  (do
			   ((c (tyipeek) (tyipeek)))
			   ((or (null (memq c #.whiteSpace))))
			   (Tyi))))
		   (setq arg (*catch 'parse$err  (get_obj nil)))
		   
		   (cond ((find 'err$$ arg)
			  (syntaxErr))
			 ((undefp arg)
			  (terpri) (patom '?) (terpri))
			 (t  
			  (let ((sPlist
				 (If DynTraceFlg then
				     (copy (plist 'Measures)) else nil))
				(wcTime1 (sys:time))
				(time1 (ptime))
				(rslt (*catch 'bottom$up (funcall fn_name arg)))
				(time2 (ptime))
				(wcTime2 (sys:time)))
			       
			       (fpPP rslt)

			       (If (and DynTraceFlg (undefp rslt)) then (setplist 'Measures sPlist))
			       (cond (timeIt
				      (let ((gcTime (diff (cadr time2) (cadr time1))))
					   (msg N "cpu + gc [wc] = ")
					   (rtime  (diff (diff (car time2) (car time1)) gcTime) 60.0)
					   (patom " + ")
					   (rtime  gcTime 60.0)
					   (patom " [")
					   (rtime (diff wcTime2 wcTime1) 1.0)
					   (msg "]"))
				      (msg (N 2))))))))))
	   
	   (t (syntaxErr) ))))))
      
      
      (cond (in_def  (setq fn_name 'tmp$$)))
      
      (cond ((and infile (eq parse_tree 'eof$$))
	     (patom "      ") (close infile) (setq infile nil))
	    
	    (t (cond ((and (null infile) (not (eq parse_tree 'eof$$)))
		      (patom "      ")))))
      
      (setq level 0)
      (setq in_buf nil)
      (setq in_def nil)))


; Display a LISP list as an equivalent FP sequence

(defun display (obj)
  (cond ((null obj) (patom "<>"))
	((atom obj) (patom obj))
	((listp obj)
	 (patom "<")
	 (maplist 
	  '(lambda (x) 
		   (display (car x))
		   (cond ((not (onep (length x))) (patom " ")))) obj)
	 (patom ">"))))

; Form a character string  of a LISP list as an equivalent FP sequence

(defun put_obj (obj)
  (cond ((null obj) "<>")
	((atom obj) obj)
	((listp obj)
	 (cond ((onep (length obj))
		(concat "<" (put_obj (car obj)) ">"))
	       (t (do
		   ((xx obj (cdr xx))
		    (zz t nil)
		    (yy "<"))
		   ((zerop (length xx)) (concat yy ">"))
		   (cond ((not zz) (setq yy (concat yy " "))))
		   (setq yy (concat yy (put_obj (car xx))))))))))



(defun rtime (time scale)
  (patom (quotient (float (fix (product 100 (quotient time scale))))
		   100.0)))

(defun doExit (exitCond)
  (cond (exitCond
	 (dontLoseStats)
	 (and (portp 'traceport) (close traceport)) ; if traceport is open
	 (and ptport (close ptport))	  	    ; if script port is open
	 (exit))))


(defun syntaxErr nil
  (let ((piport infile)
	(tbuf (ncons nil)))
       (cond ((and in_def (eq #/} (car in_buf)))
	      (do ((c (Tyi) (Tyi)))
		  ((memq c '(-1 #.CR))))
	      (synErrMsg)
	      (p_indic)
	      )
	     
	     (t (cond (in_def
		       (cond ((and 
			       (eq #.CR
				   (do ((c (tyipeek) (tyipeek))
					(e nil))
				       ((memq c '(-1 #/} #.CR))
					(If (eq c #/}) then 
					    (progn
					     (tconc tbuf c)
					     (setq e (Tyi)))
					    
					    else
					    
					    (If (eq c #.CR) then
						(setq e (Tyi))))

					(synErrMsg)
					(mapcar 'p_strng (car tbuf))
					(p_indic)
					e)
				       (tconc tbuf (Tyi))))
			       infile)
			      
			      (do ((c (tyipeek) (tyipeek))
				   (tbuf (ncons nil)))
				  ((memq c '(-1 #/}))
				   (If (eq c #/})
				   then (tconc tbuf (Tyi)))
				   (mapcar 'p_strng (car tbuf))
				   (terpri)
				   (If (eq c #/}) then
				       (do ((c (Tyi) (Tyi)))
					   ((memq c '(-1 #.CR)))))
				   )
				  
				  (tconc tbuf (Tyi))))))
		      
		      (t
		       (do ((c (tyipeek) (tyipeek)))
			   ((memq c '(-1 #.CR))
			    (Tyi)
			    (synErrMsg)
			    (mapcar 'p_strng (car tbuf))
			    (p_indic))
			   (tconc tbuf (Tyi)))))))
       ))

(defun synErrMsg nil
  (msg N "Syntax Error:" 
       (N 2))
  (mapcar 'p_strng (reverse in_buf)))


(defun p_indic nil
  (msg N (B (length (cdr (last_cr (reverse in_buf))))) "^" N)
  (If (null infile) then (terpr)))

(defun last_cr (zy)
  (cond ((null (memq #.CR zy)) zy) (t (last_cr (cdr (memq #.CR zy))))))

; throw bottom to the next level
; This shortens the compiled code

(defun bottom nil
  (*throw 'bottom$up '?))
