;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-scanner.l "@(#)scanner.l	5.1 (Berkeley) 5/31/85")

; Scanner code.

; get the next token: names, numbers, special symbols
; this is the top-level scanner section.

(include specials.l)
(declare (localf alpha$ numer$ get_num$ get_nam$ namtyp two_kind))

(defun get_tkn nil
  (do ((char_num (Getc) (Getc))
       (scan_fn nil))
      
      ((eq char_num -1) (*throw 'parse$err 'eof$$))		; eof control D
      
      ; if the first character is a letter then the next token is a name
      
      (cond ((alpha$ char_num) (return (namtyp char_num)))
	    
	    ; if the first character is a number then next token is a number 
	    
	    ((numer$ char_num) (return 
				(list 'select$$
				      (get_num$ char_num))))
	    
	    ((memq char_num #.whiteSpace))
	    ((eq char_num 35) (clr_teol)) 	; # is the comment char.
	    (t (setq scan_fn (get char_set (ascii char_num)))
	       (cond ((null scan_fn)
		      (*throw 'parse$err `(err$$ bad_char ,(ascii char_num))))
		     (t (return (funcall scan_fn))))))))
	   
; these are the scanner action functions


(defun (scan$asc |[|) nil
  'lbrack$$)

(defun (scan$asc |]|) nil
  'rbrack$$)

(defun (scan$asc |{|) nil
  'lbrace$$)

(defun (scan$asc |}|) nil
  'rbrace$$)

(defun (scan$asc |(|) nil
  'lparen$$)

(defun (scan$asc |)|) nil
  'rparen$$)

(defun (scan$asc |@|) nil
  'compos$$)

(defun (scan$asc |!|) nil
  'insert$$)

(defun (scan$asc |\||) nil ; tree insert
  'ti$$)

(defun (scan$asc |&|) nil
  'alpha$$)

(defun (scan$asc |;|) nil
  'semi$$)

(defun (scan$asc |:|) nil
  'colon$$)

(defun (scan$asc |,|) nil
  'comma$$)


(defun (scan$asc |+|) nil 			; plus or pos select
  (cond ((numer$ (peekc)) (list 'select$$ (get_num$ #/0)))
	(t '(builtin$$ plus))))
  
  
(defun (scan$asc |*|) nil
  '(builtin$$ times))

(defun (scan$asc |/|) nil
  '(builtin$$ div))

(defun (scan$asc |=|) nil
  '(builtin$$ eq))

  
  ; either a 1 or 2-char token
(defun (scan$asc |-|) nil
  (cond ((numer$ (peekc))				; subtract or neg select
	 (list 'select$$ (minus (get_num$ #/0))))
	(t (two_kind #/> 'arrow$$ '(builtin$$ sub)))))	; or arrow
  
(defun (scan$asc |>|) nil 				; > or >=
  (two_kind #/= '(builtin$$ ge) '(builtin$$ gt)))

(defun (scan$asc |<|) nil				 ; < or <=
  (two_kind #/= '(builtin$$ le) '(builtin$$ lt)))

(defun (scan$asc |~|) nil 				; ~= or error
  (two_kind #/= '(builtin$$ ne)
	    `(badtkn$$ ,(ascii char_num))))
  
  
  ; if a % then read in the next constant (object)

(defun (scan$asc |%|) nil
  (let ((v (get_obj nil)))
       (list 'constant$$ (list 'quote v))))
  

; these are the  support routines 

; routine to tell if a character is a letter

(defun alpha$ (x)
  (or (and (greaterp x 96) (lessp x 123))
      (and (greaterp x 64) (lessp x 91))))


; routine to  tell if character is a number

(defun numer$ (x)
  (and (greaterp x 47) (lessp x 58)))


; routine to read in a number 

(defun get_num$  (first_c)
  (do ((num$ (diff first_c 48 ))
       (c (peekc) (peekc)))
      ((memq c num_delim$) (return num$))
      (cond ((not (numer$ c)) (*throw 'parse$err  '(err$$ badnum)))
	    (t (setq num$ (plus (times 10 num$) (diff (Getc) 48 )))))))



; routine to read in a name

(defun get_nam$ (first_c)
  (do ((name$ (cons first_c nil))
       (c (peekc) (peekc)))
      ((not (or (numer$ c) (alpha$ c) (eq #/_ c))) (implode (nreverse name$)))
      (setq name$ (cons (Getc) name$))))

; routine to determine whether the name represents a builtin
; or not

(defun namtyp (c)
  (let ((x (get_nam$ c)))
       (cond ((eq x 'while) 'while$$)
	     (t (list 
		 (cond ((null (memq x builtins)) 'defined$$)
		       (t 'builtin$$)) x)))))


; read in a lisp sequence

(defun readit nil
  (If (not (memq (car in_buf) '(< % :)))
      then (setq in_buf (cons 32 in_buf)))

  (setq in_buf (cons #/< in_buf))
  (cond ((and ptport (null infile)) (patom '< ptport)))
  (let ((readtable newreadtable))
       (do ((xx (*catch 'parse$err  (get_obj t)) (*catch  'parse$err (get_obj t)))
	    (result nil))
	   ((eq xx '>) (nreverse result))

	   (cond ((find 'err$$ xx) (*throw 'parse$err `(err$$ bad_obj ,xx))))
	   (cond ((eq '\, xx))
		 (t (setq result (cons xx result)))))))


; peek ahead to see if the single character token in really
; a double character token

(defun two_kind (char2 dbl_nm sing_nm)
  (cond ((eq (peekc) char2)
	 (prog (dummy)
	       (setq dummy (Getc)) (return dbl_nm)))
	(t sing_nm)))

; check if any ? (bottom) in sequence

(defun chk_bot$ (x)
  (cond ((atom x) (eq x '?))
	(t (or (chk_bot$ (car x)) (chk_bot$ (cdr x))))))

; get an object and check for bottom (?) or errors (reserved symbols)

(defun get_obj (read_seq)
  (let ((readtable newreadtable))
       (prog (x)
	     (setq x (read_inp))
	     (cond ((chk_bot$ x) (return '?))
		   ((boolp x) (return x))
		   ((and (atom x) (memq x '(|,| |>|)))
		    (cond (read_seq (return x))
			  (t (*throw 'parse$err  '(err$$ bad_comma)))))
		   ((and (atom x) (memq x '(+ -)))
		    (cond ((numer$ (peekc))
			   (let ((z (*catch 'parse$err (get_obj nil))))
				(cond ((find 'err$$ z)
				       (*throw 'parse$err `(err$$ bad_num ,z)))
				      ((not (numberp z))
				       (*throw 'parse$err `(err$$ bad_num ,z)))
				      (t (cond ((eq x '+) (return z))
					       (t (return (diff z))))))))
			  (t (*throw 'parse$err `(err$$ bad_num ,x)))))
		   ((and (symbolp x) (numer$ (car (exploden x))))
		    (*throw 'parse$err `(err$$ bad_num ,x)))
		   ((and (atom x) (memq x e_rsrvd)) (*throw 'parse$err `(err$$ bad_obj ,x)))
		   (t (return x))))))


(defun read_inp nil
  (let ((c
	 (let ((piport infile))
	      (Read))))
       (If (not (listp c))
	   then (let ((ob (exploden c)))
		     (let ((OB
			    (If (and (not (= (car in_buf) #/<))
				     (not (= (car in_buf) #/>))
				     (not (= c '>)))
				then (cons 32 ob)
				else ob)))
			  
			  (If (onep (length OB))
			      then (setq in_buf (cons (car OB) in_buf))
			      else (setq in_buf (append (reverse OB) in_buf))))))
       c))



(defun clr_teol nil
  (let ((piport infile))
       (do ((c (Getc) (Getc)))
	   ((eq c #.CR) 
	    (cond ((not in_def) (setq in_buf nil)))
	    (cond ((and (not infile) (not in_def)) 
		   (patom "      ")))))))

(defun p_strng (s)
  (patom (ascii s)))
