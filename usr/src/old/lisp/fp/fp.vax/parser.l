;  FP interpreter/compiler
;  Copyright (c) 1982  Scott B. Baden
;  Berkeley, California
;
;  Copyright (c) 1982 Regents of the University of California.
;  All rights reserved.  The Berkeley software License Agreement
;  specifies the terms and conditions for redistribution.
;
(setq SCCS-parser.l "@(#)parser.l	5.1 (Berkeley) %G%")

(include specials.l)
(declare (special flag)
  (localf get_condit trap_err Push
	  prs_fn get_def get_constr get_while Pop))

(defun parse (a_flag)
  (let ((flag a_flag))
       (do
	((tkn (get_tkn) (get_tkn))
	 (rslt nil) (action nil) (wslen 0) (stk nil))
	
	((eq tkn 'eof$$) (cond ((eq flag 'top_lev) 'eof$$)
			       (t (*throw 'parse$err  '(err$$ eof)))))
	
	(cond ((null (plist (prs_fn))) (*throw 'parse$err `(err$$ unknown ,tkn))))
	(cond ((find 'badtkn$$ tkn) (*throw 'parse$err `(err$$ badtkn ,(cadr tkn)))))
	(setq action (get (prs_fn) flag))
	(cond ((null action) (*throw 'parse$err `(err$$ illeg ,tkn))))
	(setq rslt (funcall action))
	(cond ((eq rslt 'cmd$$) (return rslt)))
	(cond ((not (listp rslt)) (*throw 'parse$err  `(err$$ fatal1 ,stk ,tkn ,rslt))))
	(cond ((eq (car rslt) 'return)
	       (return 
		(cond ((eq (cadr rslt) 'done) (cdr rslt))
		      (t (cadr rslt)))))
	      
	      ((eq (car rslt) 'Push)
	       (cond ((eq flag 'while$$)
		      (cond ((or (zerop wslen) (onep wslen))
			     (Push (cadr rslt)))
			    ((twop wslen) (*throw 'parse$err  `(err$$ bad_whl ,stk ,tkn)))
			    (t (*throw  'parse$err '(err$$ bad_while parse)))))
		     (t 
		      (cond ((null stk) (Push (cadr rslt)))
			    (t (*throw  'parse$err `(err$$ stk_ful ,stk ,tkn)))))))
	      
	      ((eq (car rslt) 'nothing))
	      (t (*throw  'parse$err `(err$$ fatal2 ,stk ,tkn ,rslt)))))))
	  

; These are the parse action functions.
; There is one for each token-context combination.
; The contexts  are:
;     top_lev,constr$$,compos$$,alpha$$,insert$$.
; The name of each function is formed by appending p$ to the 
; name of the token just parsed.
; For each function name there is actually a set of functions
; associated by a plist (keyed on the context).

(defun (p$lbrace$$ top_lev) nil
  (cond (in_def  (*throw  'parse$err '(err$$ ill_lbrace)))
	(t (list 'nothing (get_def)))))

(defun (p$rbrace$$ top_lev) nil
  (cond ((not in_def)  (*throw 'parse$err  '(err$$ ill_rbrace)))
	(t (progn 
	    (cond ((null stk) (*throw  'parse$err '(err$$ stk_emp)))
		  ((null infile)
		   (do
		    ((c (Tyi) (Tyi)))
		    ((eq c 10)))))
	    `(return ,(Pop))))))

(defun (p$rbrace$$ semi$$) nil
  (cond 
   ((not in_def)  (*throw  'parse$err '(err$$ ill_rbrace)))
   (t (progn
       (cond ((null stk) (*throw  'parse$err '(err$$ stk_emp)))
	     ((null infile)
	      (do
	       ((c (Tyi) (Tyi)))
	       ((eq c 10)))))
       `(rbrace$$ ,(Pop))))))

(defun trap_err (p)
  (cond ((find 'err$$ p) (*throw  'parse$err p))
	(t p)))

(defun (p$lparen$$ top_lev) nil
  (cond ((not (null stk)) (*throw  'parse$err '(err$$ ill_lpar)))
	(t `(Push ,(trap_err (*catch '(parse$err end_while end_condit)  (parse tkn)))))))
    
(defun (p$lparen$$ constr$$) nil
  (cond ((not (null stk)) (*throw  'parse$err '(err$$ ill_lpar)))
	(t `(Push ,(trap_err (*catch '(parse$err end_while end_condit) (parse tkn)))))))
    
(defun (p$lparen$$ compos$$) nil
  (cond ((not (null stk)) (*throw  'parse$err '(err$$ ill_lpar)))
	(t `(return ,(trap_err (*catch '(parse$err end_while end_condit) (parse tkn)))))))
    
(defun (p$lparen$$ alpha$$) nil
  (cond ((not (null stk)) (*throw  'parse$err '(err$$ ill_lpar)))
	(t `(return ,(trap_err (*catch '(parse$err end_while end_condit) (parse tkn)))))))
    
(defun (p$lparen$$ ti$$) nil
  (cond ((not (null stk)) (*throw  'parse$err '(err$$ ill_lpar)))
	(t `(return ,(trap_err (*catch '(parse$err end_while end_condit) (parse tkn)))))))

(defun (p$lparen$$ insert$$) nil
  (cond ((not (null stk)) (*throw  'parse$err '(err$$ ill_lpar)))
	(t `(return ,(trap_err (*catch '(parse$err end_while end_condit) (parse tkn)))))))

(defun (p$lparen$$ arrow$$) nil
  (cond ((not (null stk)) (*throw  'parse$err '(err$$ ill_lpar)))
	(t `(Push ,(trap_err (*catch '(parse$err end_while end_condit) (parse tkn)))))))

(defun (p$lparen$$ semi$$) nil
  (cond ((not (null stk)) (*throw  'parse$err '(err$$ ill_lpar)))
	(t `(Push ,(trap_err (*catch '(parse$err end_while end_condit) (parse tkn)))))))

(defun (p$lparen$$ lparen$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_lpar)))
	(t `(Push ,(trap_err (*catch '(parse$err end_while end_condit) (parse tkn)))))))

(defun (p$lparen$$ while$$) nil
  (cond ((twop wslen) (*throw 'parse$err '(err$$ ill_lpar)))
	(t `(Push ,(trap_err (*catch '(parse$err end_while end_condit) (parse tkn)))))))

(defun (p$rparen$$ lparen$$) nil
  `(return ,(Pop)))

(defun (p$rparen$$ top_lev) nil			; process commands
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ unbalparen)))
	(t (cond ((null infile) (get_cmd))
		 (t (patom "commands may not be issued from a file")
		    (terpri)
		    'cmd$$)))))

(defun (p$rparen$$ semi$$) nil
  `(return ,(Pop)))

(defun (p$rparen$$ while$$) nil
  `(return ,(nreverse (list (Pop) (Pop)))))

(defun (p$alpha$$ top_lev) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_alpha)))
	(t `(Push ,(frm_hnk 'alpha$$ (parse tkn))))))
    
(defun (p$alpha$$ compos$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_alpha)))
	(t `(return ,(frm_hnk 'alpha$$ (parse tkn))))))
    
(defun (p$alpha$$ constr$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_alpha)))
	(t `(Push ,(frm_hnk 'alpha$$ (parse tkn))))))
    
(defun (p$alpha$$ insert$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_alpha)))
	(t `(return ,(frm_hnk 'alpha$$ (parse tkn))))))
    
(defun (p$alpha$$ ti$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_alpha)))
	(t `(return ,(frm_hnk 'alpha$$ (parse tkn))))))
    
(defun (p$alpha$$ alpha$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_alpha)))
	(t `(return ,(frm_hnk 'alpha$$ (parse tkn))))))
    
(defun (p$alpha$$ lparen$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_alpha)))
	(t `(Push ,(frm_hnk 'alpha$$ (parse tkn))))))
    
(defun (p$alpha$$ arrow$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_alpha)))
	(t `(Push ,(frm_hnk 'alpha$$ (parse tkn))))))
    
(defun (p$alpha$$ semi$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_alpha)))
	(t `(Push ,(frm_hnk 'alpha$$ (parse tkn))))))
    
(defun (p$alpha$$ while$$) nil
  (cond ((twop wslen) (*throw 'parse$err '(err$$ ill_alpha)))
	(t `(Push ,(frm_hnk 'alpha$$ (parse tkn))))))
    
    
(defun (p$insert$$ top_lev) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_insert)))
	(t `(Push ,(frm_hnk 'insert$$ (parse tkn))))))
    
(defun (p$insert$$ compos$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_insert)))
	(t `(return ,(frm_hnk 'insert$$ (parse tkn))))))
    
(defun (p$insert$$ constr$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_insert)))
	(t `(Push ,(frm_hnk 'insert$$ (parse tkn))))))
    
(defun (p$insert$$ insert$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_insert)))
	(t `(return ,(frm_hnk 'insert$$ (parse tkn))))))
    
(defun (p$insert$$ ti$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_insert)))
	(t `(return ,(frm_hnk 'insert$$ (parse tkn))))))
    
(defun (p$insert$$ alpha$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_insert)))
	(t `(return ,(frm_hnk 'insert$$ (parse tkn))))))

(defun (p$insert$$ lparen$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_insert)))
	(t `(Push ,(frm_hnk 'insert$$ (parse tkn))))))

(defun (p$insert$$ arrow$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_insert)))
	(t `(Push ,(frm_hnk 'insert$$ (parse tkn))))))

(defun (p$insert$$ semi$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_insert)))
	(t `(Push ,(frm_hnk 'insert$$ (parse tkn))))))

(defun (p$insert$$ while$$) nil
  (cond ((twop wslen) (*throw 'parse$err '(err$$ ill_insert)))
	(t `(Push ,(frm_hnk 'insert$$ (parse tkn))))))


(defun (p$ti$$ top_lev) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_ai)))
	(t `(Push ,(frm_hnk 'ti$$ (parse tkn))))))
    
(defun (p$ti$$ compos$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_ai)))
	(t `(return ,(frm_hnk 'ti$$ (parse tkn))))))
    
(defun (p$ti$$ constr$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_ai)))
	(t `(Push ,(frm_hnk 'ti$$ (parse tkn))))))
    
(defun (p$ti$$ insert$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_ai)))
	(t `(return ,(frm_hnk 'ti$$ (parse tkn))))))
    
(defun (p$ti$$ ti$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_ai)))
	(t `(return ,(frm_hnk 'ti$$ (parse tkn))))))
    
(defun (p$ti$$ alpha$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_ai)))
	(t `(return ,(frm_hnk 'ti$$ (parse tkn))))))

(defun (p$ti$$ lparen$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_ai)))
	(t `(Push ,(frm_hnk 'ti$$ (parse tkn))))))

(defun (p$ti$$ arrow$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_ai)))
	(t `(Push ,(frm_hnk 'ti$$ (parse tkn))))))

(defun (p$ti$$ semi$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ ill_ai)))
	(t `(Push ,(frm_hnk 'ti$$ (parse tkn))))))

(defun (p$ti$$ while$$) nil
  (cond ((twop wslen) (*throw 'parse$err '(err$$ ill_ai)))
	(t `(Push ,(frm_hnk 'ti$$ (parse tkn))))))


(defun (p$compos$$ top_lev) nil
  `(Push ,(frm_hnk 'compos$$ (Pop) (parse tkn))))

(defun (p$compos$$ compos$$) nil
  `(Push ,(frm_hnk 'compos$$ (Pop) (parse tkn))))

(defun (p$compos$$ constr$$) nil
  `(Push ,(frm_hnk 'compos$$ (Pop) (parse tkn))))

(defun (p$compos$$ lparen$$) nil
  `(Push ,(frm_hnk 'compos$$ (Pop) (parse tkn))))

(defun (p$compos$$ arrow$$) nil
  `(Push ,(frm_hnk 'compos$$ (Pop) (parse tkn))))

(defun (p$compos$$ semi$$) nil
  `(Push ,(frm_hnk 'compos$$ (Pop) (parse tkn))))

(defun (p$compos$$ while$$) nil
  `(Push ,(frm_hnk 'compos$$ (Pop) (parse tkn))))

    
(defun (p$comma$$ constr$$) nil
  `(return ,(Pop)))
    
(defun (p$comma$$ semi$$) nil
  `(comma$$ ,(Pop)))
    

(defun (p$lbrack$$ top_lev) nil
  `(Push ,(get_constr)))

(defun (p$lbrack$$ compos$$) nil
  `(return ,(get_constr)))

(defun (p$lbrack$$ constr$$) nil
  `(Push ,(get_constr)))

(defun (p$lbrack$$ lparen$$) nil
  `(Push ,(get_constr)))

(defun (p$lbrack$$ arrow$$) nil
  `(Push ,(get_constr)))

(defun (p$lbrack$$ semi$$) nil
  `(Push ,(get_constr)))

(defun (p$lbrack$$ alpha$$) nil
  `(return ,(get_constr)))

(defun (p$lbrack$$ insert$$) nil
  `(return ,(get_constr)))

(defun (p$lbrack$$ ti$$) nil
  `(return ,(get_constr)))

(defun (p$lbrack$$ while$$) nil
  `(Push ,(get_constr)))


(defun (p$rbrack$$ constr$$) nil
  `(return done ,(cond ((null stk) nil)
		       (t (Pop)))))
    
(defun (p$rbrack$$ semi$$) nil
  `(rbrack$$ ,`(done ,(cond ((null stk) nil)
			    (t (Pop))))))
    
    
(defun (p$defined$$ top_lev) nil
  `(Push ,(concat (cadr tkn) '_fp)))
    
(defun (p$defined$$ compos$$) nil
  `(return ,(concat (cadr tkn) '_fp)))
    
(defun (p$defined$$ constr$$) nil
  `(Push ,(concat (cadr tkn) '_fp)))
    
(defun (p$defined$$ lparen$$) nil
  `(Push ,(concat (cadr tkn) '_fp)))
    
(defun (p$defined$$ arrow$$) nil
  `(Push ,(concat (cadr tkn) '_fp)))
    
(defun (p$defined$$ semi$$) nil
  `(Push ,(concat (cadr tkn) '_fp)))
    
(defun (p$defined$$ alpha$$) nil
  `(return ,(concat (cadr tkn) '_fp)))

(defun (p$defined$$ insert$$) nil
  `(return ,(concat (cadr tkn) '_fp)))
    
(defun (p$defined$$ ti$$) nil
  `(return ,(concat (cadr tkn) '_fp)))
    
(defun (p$defined$$ while$$) nil
  `(Push ,(concat (cadr tkn) '_fp)))
    

(defun (p$builtin$$ top_lev) nil
  `(Push ,(concat (cadr tkn) '$fp)))
    
(defun (p$builtin$$ compos$$) nil
  `(return ,(concat (cadr tkn) '$fp)))
    
(defun (p$builtin$$ constr$$) nil
  `(Push ,(concat (cadr tkn) '$fp)))
    
(defun (p$builtin$$ lparen$$) nil
  `(Push ,(concat (cadr tkn) '$fp)))
    
(defun (p$builtin$$ arrow$$) nil
  `(Push ,(concat (cadr tkn) '$fp)))
    
(defun (p$builtin$$ semi$$) nil
  `(Push ,(concat (cadr tkn) '$fp)))
    
(defun (p$builtin$$ alpha$$) nil
  `(return ,(concat (cadr tkn) '$fp)))

(defun (p$builtin$$ insert$$) nil
  `(return ,(concat (cadr tkn) '$fp)))
    
(defun (p$builtin$$ ti$$) nil
  `(return ,(concat (cadr tkn) '$fp)))
    
(defun (p$builtin$$ while$$) nil
  `(Push ,(concat (cadr tkn) '$fp)))
    

(defun (p$select$$ top_lev) nil
  `(Push ,(makhunk tkn)))

(defun (p$select$$ compos$$) nil
  `(return ,(makhunk tkn)))
    
(defun (p$select$$ constr$$) nil
  `(Push ,(makhunk tkn)))
    
(defun (p$select$$ lparen$$) nil
  `(Push ,(makhunk tkn)))
    
(defun (p$select$$ arrow$$) nil
  `(Push ,(makhunk tkn)))
    
(defun (p$select$$ semi$$) nil
  `(Push ,(makhunk tkn)))
    
(defun (p$select$$ alpha$$) nil
  `(return ,(makhunk tkn)))
    
(defun (p$select$$ while$$) nil
  `(Push ,(makhunk tkn)))
    
    
(defun (p$constant$$ top_lev) nil
  `(Push ,(makhunk tkn)))

(defun (p$constant$$ compos$$) nil
  `(return ,(makhunk tkn)))
    
(defun (p$constant$$ constr$$) nil
  `(Push ,(makhunk tkn)))
    
(defun (p$constant$$ lparen$$) nil
  `(Push ,(makhunk tkn)))
    
(defun (p$constant$$ arrow$$) nil
  `(Push ,(makhunk tkn)))
    
(defun (p$constant$$ semi$$) nil
  `(Push ,(makhunk tkn)))
    
(defun (p$constant$$ alpha$$) nil
  `(return ,(makhunk tkn)))
    
(defun (p$constant$$ while$$) nil
  `(Push ,(makhunk tkn)))
    

(defun (p$colon$$ top_lev) nil
  (cond (in_def  (*throw 'parse$err '(err$$ ill_appl)))
	(t `(return ,(Pop)))))

(defun (p$colon$$ semi$$) nil
  (cond (in_def  (*throw 'parse$err '(err$$ ill_appl)))
	(t `(colon$$ ,(Pop)))))


(defun (p$arrow$$ lparen$$) nil
  (get_condit))
    

(defun (p$semi$$ arrow$$) nil
  `(return ,(Pop)))

(defun (p$while$$ lparen$$) nil
  (cond ((not (null stk)) (*throw 'parse$err '(err$$ bad_while)))
	(t (get_while))))


; parse action support functions

(defun get_condit nil
  (prog (q r)
	(setq q (parse 'arrow$$))
	(cond ((and (listp q) (find 'err$$ q)) (*throw 'parse$err q)))
	(setq r (parse 'semi$$))
	(cond ((and (listp r) (find 'err$$ r)) (*throw 'parse$err r)))
	(*throw 'end_condit (frm_hnk 'condit$$ (Pop) q r))))
	

(defun Push (value)
  (cond ((eq flag 'while$$)
	 (cond 
	  ((zerop wslen) (setq stk value) (setq wslen 1))
	  ((onep wslen) (setq stk (list stk value)) (setq wslen 2))
	  (t (*throw 'parse$err '(err$$ bad_while Push)))))
	(t (setq stk value))))

(defun Pop nil
  (cond
   ((null stk) (*throw 'parse$err '(err$$ stk_emp)))
   (t
    (prog (tmp)
	  (setq tmp stk)
	  (cond ((eq flag 'while$$)
		 (cond ((onep wslen) (setq stk nil) (setq wslen 0) (return tmp))
		       ((twop wslen) 
			(setq stk (car tmp)) (setq wslen 1) (return (cadr tmp)))
		       (t  (*throw 'parse$err '(err$$ bad_while Pop)))))
		(t (setq stk nil)
		   (return tmp)))))))

(defun get_def nil
  (prog (dummy)
	(setq in_def t)
	(setq dummy (get_tkn))
	(cond ((find 'builtin$$ dummy) (*throw 'parse$err '(err$$ redef)))
	      ((not (find 'defined$$ dummy)) (*throw 'parse$err  '(err$$ bad_nam)))
	      (t (setq fn_name (concat (cadr dummy) '_fp))))))
    
    
(defun get_constr  nil
  (cond ((eq flag 'while$$) (cond 
			     ((twop wslen) (*throw 'parse$err `(err$$ bad_whl ,stk ,tkn)))))
	(t (cond ((not (null stk)) (*throw 'parse$err '(err$$ bad_constr parse))))))
  (do
   ((v (parse 'constr$$) (parse 'constr$$))
    (temp nil)
    (fn_lst nil))
   
   ((eq tkn 'eof$$) (*throw 'parse$err '(err$$ eof$$)))
   
   (cond 
    ((listp v)
     (cond ((eq (car v) 'err$$) (*throw 'parse$err v))
	   ((eq (car v) 'done) 
	    (cond ((eq (cadr v) 'err$$) (*throw 'parse$err  (cdr v)))
		  (t (return
		      (makhunk (cons 'constr$$ (reverse (cons (cadr v) fn_lst))))))))
	   (t (setq fn_lst (cons v fn_lst)))))
    (t (setq fn_lst (cons v fn_lst))))))

(def frm_hnk (lexpr (z)
		    (prog (l bad_one)
			  (setq l (listify z))
			  (setq bad_one (assq 'err$$ (cdr l)))
			  (cond ((null bad_one) (return (makhunk l)))
				(t (*throw 'parse$err bad_one))))))



(defun prs_fn nil
  (concat 'p$ (cond ((atom tkn) tkn)
		    (t (car tkn)))))

(defun get_while nil
  (let ((r (parse 'while$$)))
       (cond ((and (listp r) (find 'err$$ r)) (*throw 'parse$err  r))
	     (t (*throw 'end_while (frm_hnk 'while$$ (car r) (cadr r)))))))

(defun twop (x)
  (eq 2 x))

