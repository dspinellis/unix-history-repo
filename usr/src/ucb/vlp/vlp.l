;$Header: vlp.l,v 1.5 84/05/07 15:09:59 jkf Exp $
;$Locker: layer $
;
;			-[Mon May  7 15:04:34 1984 by jkf]-
;
;  Filter for nroffing or troffing Lisp code.
;  This program reads files containing Lisp code and writes them out
;  along with the nroff (troff) commands required to make the Lisp line up.
;  It does this alignment by insuring that the leftmost
;  blank character lines up with the closest non blank character above it.
;
;  The program operates in one of two modes: filter mode or non-filter mode.
;
;  In filter mode, only characters between lines beginning with .Ls and .Le
;  are processed.  The directive .Ls accepts an optional argument that is 
;  the point size of the output.  Pages have no titles and point size is set
;  by the macro, so most shell arguments are ignored.
;
;  In non-filter mode, all lines in the file are processed.
;
;  The output goes to the standard output, unless the v switch is set,
;  in which case it goes to vtroff.
;
;  Usage:
;    vlp [-p size] [-d] [-f] [-l] [-v] [-T title1] file1 ....
;
;	where,
;		-p	Sets the point size to "size"
;		-d	Turn on debugging mode
;		-f	Run in filtered mode
;		-l	Turns OFF the labeling of functions 
;		-v	Sends the output to vtroff
;		-T	Sets the title of the next file to "titleN"
;
;  NOTE: If vlp is on the right side of a pipe, the "-T" switch can not be
;	 used.  
;
;
;  Written by John Foderaro
;  Modified by:
;	Kevin Layer to have enough tags.
;	Jim Larus to work in filtered mode.
;

(declare (macros t))

(allocate 'list 220)

(declare (special user-top-level EOF lines g-psize inport
		  spaces sps chs tls trname errport outfilename
		  filtermode g-title rest funcname readstdin debug
		  spout outfile tagsleft LabelFunctions)) 

(defvar space-count 500.)	; large number

;--- toplev :: top level lisp 
;
(defun toplev nil
   (let ((args (command-line-args)))
       (signal 2 'vlp-interrupt-routine)
       (signal 15 'vlp-interrupt-routine)
       (errset (apply 'vlp args))
       (exit 0)))

(eval-when (load) (setq user-top-level 'toplev))

(setq g-psize 8
      debug nil
      g-filtmode nil
      readstdin nil
      filtermode nil
      outfilename nil
      LabelFunctions t)


(defun GetModTime (FileName)
  ;
  ; Returns the last modification time as a string.
  ;
  (time-string (filestat:mtime (filestat (get_pname FileName)))))


(defun vlp fexpr (args)
   (prog (inport outfile)
       ;check command line for switches
       (setq args (do ((i args (cdr i)) (ll) (tem))
		      ((null i) (reverse ll))
		      (setq tem (aexplodec (car i)))
		      (cond ((eq '- (car tem))	; if switch
			     (cond ((eq 'd (cadr tem))
				    (setq debug t))
				   ((eq 'f (cadr tem))
				    (setq filtermode t))
				   ((eq 'l (cadr tem))
				    (setq LabelFunctions nil))
				   ((eq 'p (cadr tem))
				    (setq g-psize
					  (cond ((eq '|6| (cadr i)) 6)
						((eq '|10| (cadr i)) 10)
						((eq '|12| (cadr i)) 12)
						(t 8)))
				    (setq i (cdr i)))
				   ((eq 'v (cadr tem))
				    (setq outfilename
					  (concat "/tmp/vlp" (sys:getpid))))
				   (t (setq ll (cons (car i) ll)))))
			    (t (setq ll (cons (car i) ll)))))) ; else skip it
       ;
       (if debug then (msg (P errport) "args = " args N))
       ;
       ; insure all file names given actually exist before starting.
       (cond ((null (do ((xx args (cdr xx)))
			((null xx) t)
			(if (eq '- (getchar (car xx) 1))
			    then (setq xx (cdr xx)) ; skip switches
			    else (if (not (probef (car xx))) 
				     then (msg (P errport)
					       "File does not exist: "
					       (car xx))
					  (return nil)))))
	      (return nil)))
       ;
       ; do before-starting-first-file actions
       ;
       (if outfilename then (setq outfile (outfile outfilename)))
       (if (null args) then (setq readstdin t))

       (if (and LabelFunctions (null filtermode))
	   then ;; NOTE: the following concat'ing of strings 
		;; is neccessary because when vlp'ing vlp.l 
		;; soelim would source the file.
		(msg (P outfile) (concat "." "so /usr/lib/vlpmacs") N))
       (setq g-title "")
       ;
       ;
       (if readstdin
	   then (setq EOF nil)
		(setq lines nil)
		(if (not filtermode)
		    then (msg (P outfile) ".ps " g-psize N
			      ".vs " (+ g-psize 2) "p" N)
			 (msg (P outfile) ".Ti " '\" g-title '\" N)
			 (msg (P outfile) ".wh -1.25i He" N)
			 (msg (P outfile) ".nf" N))
		(do ()
		    (EOF t)
		    (if filtermode then (skiptostarter))
		    (processblock))
	   else (do ((files args (cdr files)))
		    ((null files) t)
		    (if (eq '- (getchar (car files) 1))
			then (if (eq '-p (car files))
				 then (if (null (setq g-psize
						      (cdr (assoc (cadr files)
								  '((\6 . 6)
								    (\8 . 8)
								    (\10 . 10)
								    (\12 . 12))))))
					  then (error "bad point size"
						      (cadr files)))
			      elseif (eq '-T (car files))
				 then (setq g-title (cadr files))
				 else (error "bad switch " (car files)))
			     (setq files (cdr files))
			else (setq EOF nil)
			     (setq lines nil)
			     (cond ((null (errset
					      (setq inport (infile (car files)))))
				    (msg (P errport) "Can't open file: "
					 (car args))
				    (return nil)))
			     (if (not filtermode)
				 then (msg (P outfile)
					   ".ps " g-psize N
					   ".vs " (+ g-psize 2) "p" N)
				      (msg (P outfile)
					   ".Fi " (car files)
					   " \"" (GetModTime (car files)) '\"
					   " \"" (time-string) '\"
					   N)
				      (msg (P outfile)
					   ".Ti " '\" g-title '\" N)
				      (msg (P outfile) ".wh -1.25i He" N)
				      (msg (P outfile) ".nf" N))
			     (do ()
				 (EOF)
				 (if filtermode then (skiptostarter))
				 (processblock))
			     (close inport))))
       (if outfilename
	   then
		(close outfile)
		(apply 'process
		       (ncons (concat "/bin/cat " outfilename
				      " | /usr/ucb/vtroff")))
		(if debug
		    then (msg (P errport)
			      "Troff filename is " outfilename N)
		    else (sys:unlink outfilename)))))


;--- skiptostarter :: skip to start directive (.Ls)
;
(defun skiptostarter nil
   ;
   ; Define a simple finite state machine that reads and prints everything
   ; up-to and including the directive .Ls.
   ; All text outside of the .Ls directive is simply sent on to the output.
   ;
   (do ((State 0)
	(c (tyi inport) (tyi inport)))
       ((=& 4 State))
       (cond ((and (=& State 0) (=& c #/.))
	      (setq State 1))
	     ((and (=& State 1) (=& c #/L))
	      (setq State 2))
	     ((and (=& State 2) (=& c #/s))
	      (setq State 3))
	     ((=& State 3)			; Read through arguments
	      (cond ((=& c #\lf)
		     (setq State 4)
		     (untyi c inport))))	; In case text starts immed.
	     ((=& c #\eof)
	      (setq EOF t)
	      (return))
	     (t
		 (setq State 0)))
       (tyo c outfile)))

(defun processblock nil
   ;
   ; Read lines until we come to one that has no blank space at its begining.
   ; Then process the previously accumulated lines.
   ; If the last line contains the directive .Le, then find the next block
   ; enclosed by .Ls.
   ;
   (do ((newline (getline) (getline)))
       (nil)
       (if (or EOF (and (zerop (car newline)) (cadr newline))) 
	   then (flushlines)
		(setq lines nil)
		(if (=& (caadr newline) #/')	;then must quote ' for troff
		    then (msg (P outfile) "\\")
		 elseif (and filtermode
			     (=& (caadr newline) #/.)
			     (=& (cadadr newline) #/L)
			     (=& (caddadr newline) #/e))
		    then (msg (P outfile) ".Le" N)
			 (return nil))
		(if EOF then (return nil)))
       (setq lines (cons newline lines))))


;--- getline :: read in new line and return structure:
; (spaces chars)
; spaces is a fixnum giving the number of spaces before the first
; non-blank on the line.
; chars is the list of characters appearing on the line, in order.
;
(defun getline nil
   (do ((col 1)
	(spaces 0)
	(chars nil)
	(spacemode t)
	(newc (tyi inport) (tyi inport) ))
       (nil)
       (if (=& #\eof newc) 
	   then (if (null chars) 
		    then (setq EOF t)
			 (return nil)
		    else (setq newc #\lf)))
       (if (=& #\lf newc)
	  then (if (null chars)
		  then ; for totally blank lines return a large
		       ; space count so this won't be used for tags
		       (return (list space-count nil))
		  else (return (list spaces (nreverse chars)))))
       (if (=& #\ff newc)
	   then (return (list spaces (nreverse (cons newc chars)))))
       (if spacemode 
	   then (if (=& #\sp newc)
		    then (setq spaces (1+ spaces))
		 elseif (=& #\tab newc)
		    then (setq spaces  (* (/ (+ spaces 8) 8) 8))
		    else (setq spacemode nil
			       chars (list newc)))
	   else (setq chars (cons newc chars)))))
			

;--- flushlines
; go back on all lines and determine where to put tags and reference
; tags.
;
(defun flushlines nil
  (let (thistag tagref rlines tagloc)
       (inittagslist '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
			 a b c d e f g h i j k l m n o p q r s t u v w x y z
			 1 2 3 4 5 6 7 8 9 0))
       (setq tagref nil tagloc nil rlines nil)
       (do ((xx lines (cdr xx))
	    (thistagloc nil nil)
	    (pendingtags)
	    (thistagref))
	   ((null xx))
	   (setq rlines (cons (car xx) rlines)
		 spaces (caar xx)
		 thistag (alloctag))
	   ; determine which of the pending tags we can insert at this level
	   (do ((yy pendingtags (cdr yy)))
	       ((or (null yy) (>& spaces (caar yy)))
		(setq pendingtags yy))
	       (setq thistagloc (cons (car yy) thistagloc)))
	   (setq tagloc (cons thistagloc tagloc))
	   ; free up the tags we allocated at this level.
	   (mapc '(lambda (x) (freetag (cadr x))) thistagloc)
	   ;
	   ; we only need to refer to a tags at this level if 
	   ; we are not at the left edge of the paper
	   ;
	   (if (and (greaterp spaces 0) (cdr xx))
	       then (setq tagref (cons thistag tagref))
		    (setq pendingtags (addtotags spaces thistag pendingtags))
	       else (setq tagref (cons nil tagref))))
       (if debug then (msg " **tagref = " tagref N " **tagloc = " tagloc N))
       ;
       ; print out the lines. 
       ; the lines we want to print are in the list rlines.  They are 
       ; in the correct order.
       ;
       (do ((yy rlines (cdr yy))
	    (commentmode nil nil)
	    (escapemode nil nil))
	   ((null yy))
	   ; first print out spaces
	   (setq sps (caar yy) 
		 chs (cadar yy)
		 tls (car tagloc)
		 trname (car tagref))
	   (headercheck sps chs)
	   (if debug
	       then (msg " spaces " sps N " chs " chs N
			 " tagloc " (car tagloc) N " tagref " (car tagref) N))
	   (setq spout 0)
	   ; if there are characters on this line, print out
	   ; leading spaces
	   (if chs
	      then (do ()
		       ((eq spout sps))
		       (tyo #\sp outfile)
		       (setq spout (1+ spout)))

		   (if trname then (tagrefprint trname)))
	   
	   (do ((xx chs (cdr xx)))
	       ((null xx)
		(if (and tls (not (<& (caar tls) spout)))
		    then (do ((i spout (1+ i)))
			     ((null tls))
			     (if (=& (caar tls) i) 
				 then (taglocprint (cadar tls))
				      (setq tls (cdr tls)))))
		(if commentmode then (msg (P outfile) "\\fP"))
		(terpr outfile))
	       (if (and tls (eq (caar tls) spout))
		   then (taglocprint (cadar tls))
			(setq tls (cdr tls)))
	       (if (and (not escapemode) 
			(null commentmode) 
			(=& #/; (car xx)))
		   then (msg (P outfile) "\\fI")
		        (setq commentmode t)
		elseif (=& #/\ (car xx))
		   then (tyo #/\ outfile))	; escape backslashes
	       (if (memq (car xx) '(#/\ #//))
		   then (setq escapemode t)
		   else (setq escapemode nil))
	       (if (=& (car xx) #\ff)
		   then (msg (P outfile) N ".bp" N)
		   else (tyo (car xx) outfile))
	       (setq spout (1+ spout)))
	   (setq tagloc (cdr tagloc)
		 tagref (cdr tagref)))))


; headercheck :: check if this is a function declaration.
; currently this means that there are no more than 2 spaces before
; the line begins and that the line begins with (defxxx  <name>)
(defun headercheck (spaces chars)
  (if (<& spaces 3)
      then (if (setq rest (match chars '(#/( #/d #/e #/f)))
	       then (setq funcname (skippastblkn rest))
	            (if (and funcname LabelFunctions)
			then (msg (P outfile) ".Lf " funcname N)))))

;--- match :: match lists 
;	list1 - list of characters (fixnum rep)
;	list2 - master list of characters (fixnum rep)
; list2 should be shorter than list1. If list2 is a substring of list1
; then the rest of list1 will be returned.  Otherwise nil is returned.
;
(defun match (list1 list2)
  (cond ((null list1) nil)
	((null list2) list1)
	((or (eq (car list1) (car list2))
	     (eq (car list1) (uppercase (car list2))))
	 (match (cdr list1) (cdr list2)))
	(t nil)))

;--- uppercase :: convert fixnum rep to upper case
;	char - fixnum representation of character
; convert character to upper case.
;
(defun uppercase (ch)
  (if (and (not (<& ch #/a)) (not (>& ch #/z)))
      then (- ch #.(- #/a #/A))))



;--- skippastblnk :: skip to and past blank field and return following name
;	list - list of characters (fixnums)
;
; We skip past all non blanks then all blanks and finally implode the next 
; word after that and return it.  
;
; bug- we can't tell the difference between a function named nil and
; no valid function, but then again you can't have a function named nil.
;
(defun skippastblkn (list)
   (let (res)
      ; skip to first blank
      (do ()
	  ((null list) nil)
	  (if (=& (car list) #\sp) then (return nil))
	  (setq list (cdr list)))
      ; skip to first non-blank
      (do ()
	  ((null list) nil)
	  (if (not (=& (car list) #\sp)) then (return nil))
	  (setq list (cdr list)))
      ; collect non blanks
      (if list 
	 then (do ()
		  ((null list))
		  (if (and (=& #\lpar (car list))
			   (null res))
		     then (setq list (cdr list)) ; form like "defun (foo ..)"
		   elseif (or (=& #\lpar (car list))
			      (=& #\rpar (car list))
			      (=& #\sp (car list)))
		     then (return nil)
		     else (setq res (cons (car list) res))
			  (setq list (cdr list))))
	      (if res then (implode (nreverse res))))))

;
; add (spaces character) to list so that the spaces number are in 
; ascending order.
;
(defun addtotags (spaces character oldlist)
  (if (or (null oldlist) (>& spaces (caar oldlist)))
      then (cons (list spaces character) oldlist)
      else (do ((prevtag oldlist (cdr prevtag))
		(thistag (cdr oldlist) (cdr thistag)))
	       ((or (null thistag) (>& spaces (caar thistag)))
		(setq thistag (cons (list spaces character) thistag))
		(rplacd prevtag thistag)))))

(defun inittagslist (list)
  (setq tagsleft list))

(defun alloctag nil
  (if (null tagsleft) 
      then (msg "Out of tags, aauuuuggh " N) 
           (exit)
      else (prog1 (car tagsleft) (setq tagsleft (cdr tagsleft)))))

(defun freetag (tagname)
  (setq tagsleft (cons tagname tagsleft)))


(defun taglocprint (name)
  (msg (P outfile) "\\k" name))


(defun tagrefprint (name)
  (msg (P outfile)  "\\h'|\\n" name "u'"))


;---new stuff
;
;--- programs deal in lineblk's
; A lineblk has these components:
;	class : normal or comment
;		comment is a line beginning with ;, in which case it is
;		not counted as far as spacing goes.
;	spaces : number of spaces before first useful info
;	dead : list of dead linblk's, that is those ready to print but
;	       waiting for this line to be done.
;	tagref: tag to go to at the beginning of this line
;	tagloc : tags defined on this line
;	chars : list of characters to print

(defun make-lineblk (class spaces chars)
   (list class spaces nil nil nil chars))
(defmacro get:class (lineblk) `(car ,lineblk))
(defmacro set:class (lineblk val) `(setf (get:class ,lineblk) ,val))
(defmacro get:spaces (lineblk) `(cadr ,lineblk))
(defmacro set:spaces (lineblk val) `(setf (get:spaces ,lineblk) ,val))
(defmacro get:dead (lineblk) `(caddr ,lineblk))
(defmacro set:dead (lineblk val) `(setf (get:dead ,lineblk) ,val))
(defmacro get:tagref (lineblk) `(cadddr ,lineblk))
(defmacro set:tagref (lineblk val) `(setf (get:tagref ,lineblk) ,val))
(defmacro get:tagloc (lineblk) `(caddddr ,lineblk))
(defmacro set:tagloc (lineblk val) `(setf (get:tagloc ,lineblk) ,val))
(defmacro get:chars (lineblk) `(cadddddr ,lineblk))
(defmacro set:chars (lineblk val) `(setf (get:chars ,lineblk) ,val))

   
(defun vlp-interrupt-routine (sig)
   (if (and (boundp 'outfilename) outfilename)
      then (sys:unlink outfilename))
   (exit 1))
