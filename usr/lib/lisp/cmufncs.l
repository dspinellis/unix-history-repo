(eval-when (compile eval) (load 'cmumacs))

(declare (special filelst %changes $%dotflg %prevfn% %%cfn part %%l
		  lastword %trcflg form fn))
(def tab (lexpr (n)
		(prog (nn prt) (setq nn (arg 1))
			       (cond ((> n 1)(setq prt (arg 2))))
			       (cond ((> (nwritn prt) nn) (terpri prt)))
			       (printblanks (- nn (nwritn prt)) prt))))


(dv $%dotflg nil)
(def %lineread
     (lambda 
      (chan)
      (prog (ans)
       loop (setq ans (cons (read chan 'EOF) ans))
	    (cond ((eq (car ans) 'EOF) (return (reverse (cdr ans)))))
       loop2(cond ((eq 10 (tyipeek chan)) (return (reverse ans)))
                  ((memq (tyipeek chan) '(41 93))
                   (tyi chan)
                   (go loop2))
                  (t (go loop))))))


(dv %prevfn% " ")
(dv %trcflg t)
   
(def attach
     (lambda 
      (x y)
      (cond ((dtpr y) (rplacd y (cons (car y) (cdr y))) (rplaca y x))
            (t (eprint y) (error '"IS AN ATOM, CAN'T BE ATTACHED TO")))))

(dv %changes ())

(def dremove
   (lambda (x l)
	   (cond ((atom l) nil)
		 ((eq x (car l))
		  (cond ((cdr l)
			 (rplaca l (cadr l))
			 (rplacd l (cddr l))
			 (dremove x l))))
		 (t (prog (z)
			  (setq z l)
		    lp    (cond ((atom (cdr l)) (return z))
				((eq x (cadr l)) (rplacd l (cddr l)))
				(t (setq l (cdr l))))
			  (go lp))))))
(def dreverse
     (lambda (l)
      (prog (l1 y z)
            (setq l1 l)
       l1   (cond
             ((atom (setq y l))
              (cond ((or (null z) (null (cdr z))) (return z))
                    ((null (cddr z))
                     (setq y (car l1))
                     (rplaca l1 (car z))
                     (rplaca z y)
                     (rplacd l1 z)
                     (rplacd z nil)
                     (return l1))
                    (t (rplacd (Cnth z (sub1 (length z))) z)
                       (setq y (car l1))
                       (rplaca l1 (car z))
                       (rplaca z y)
                       (rplacd l1 (cdr z))
                       (rplacd z nil)
                       (return l1)))))
            (setq l (cdr l))
            (setq z (rplacd y z))
            (go l1))))

(def dsubst
     (lambda (x y z)
      (prog (b)
            (cond ((eq y (setq b z)) (return (copy x))))
       lp   (cond ((atom z) (return b))
                  ((cond ((symbolp y) (eq y (car z))) (t (equal y (car z))))
                   (rplaca z (copy x)))
                  (t (dsubst x y (car z))))
            (cond ((and y (eq y (cdr z))) (rplacd z (copy x)) (return b)))
            (setq z (cdr z))
            (go lp))))

(putd 'eqstr (getd 'equal))

; where are the functions this calls??
(def every
     (lambda 
      (everyx everyfn1 everyfn2)
      (prog nil
       a    (cond ((null everyx) (return t))
                  ((funcall everyfn1 (car everyx))
                   (setq everyx
                         (cond ((null everyfn2) (cdr everyx))
                               (t (funcall everyfn2 everyx))))
                   (go a))
                  (t (return nil))))))
(def insert
     (lambda 
      (x l comparefn nodups)
      (cond ((null l) (list x))
            ((atom l)
             (eprint l)
             (error '"is an atom, can't be inserted into"))
            (t (cond
                ((null comparefn) (setq comparefn (function alphalessp))))
               (prog (l1 n n1 y)
                     (setq l1 l)
                     (setq n (length l))
                a    (setq n1 (*quo (add1 n) 2))
                     (setq y (Cnth l1 n1))
                     (cond ((< n 3)
                            (cond ((funcall comparefn x (car y))
                                   (cond
                                    ((not
                                      (and nodups (equal x (car y))))
                                     (rplacd y (cons (car y) (cdr y)))
                                     (rplaca y x))))
                                  ((eq n 1) (rplacd y (cons x (cdr y))))
                                  ((funcall comparefn x (cadr y))
                                   (cond
                                    ((not
                                      (and nodups (equal x (cadr y))))
                                     (rplacd (cdr y)
                                             (cons (cadr y) (cddr y)))
                                     (rplaca (cdr y) x))))
                                  (t (rplacd (cdr y) (cons x (cddr y))))))
                           ((funcall comparefn x (car y))
                            (cond
                             ((not (and nodups (equal x (car y))))
                              (setq n (sub1 n1))
                              (go a))))
                           (t (setq l1 (cdr y)) (setq n (- n n1)) (go a))))
               l))))

(def kwote (lambda (x) (list 'quote x)))

(def lconc
     (lambda 
      (ptr x)
      (prog (xx)
            (return
             (cond ((atom x) ptr)
                   (t (setq xx (last x))
                      (cond ((atom ptr) (cons x xx))
                            ((dtpr (cdr ptr))
                             (rplacd (cdr ptr) x)
                             (rplacd ptr xx))
                            (t (rplaca (rplacd ptr xx) x)))))))))

(def ldiff
     (lambda 
      (x y)
      (cond ((eq x y) nil)
            ((null y) x)
            (t
             (prog (v z)
                   (setq z (setq v (ncons (car x))))
              loop (setq x (cdr x))
                   (cond ((eq x y) (return z))
                         ((null x) (error '"NOT A TAIL - LDIFF")))
                   (setq v (cdr (rplacd v (ncons (car x)))))
                   (go loop))))))


(def lsubst
     (lambda 
      (x y z)
      (cond ((null z) nil)
            ((atom z) (cond ((eq y z) x) (t z)))
            ((equal y (car z)) (nconc (copy x) (lsubst x y (cdr z))))
            (t (cons (lsubst x y (car z)) (lsubst x y (cdr z)))))))

(def memcdr
     (lambda 
      (%x% %y%)
      (prog nil
       l1   (cond ((eq %x% (cdr %y%)) (return t))
                  ((eq %x% %y%) (return nil)))
            (setq %x% (cdr %x%))
            (go l1))))

(def merge
     (lambda 
      (a b %%cfn)
      (cond ((null %%cfn) (setq %%cfn (function alphalessp))))
      (merge1 a b)))

(def merge1
     (lambda 
      (a b)
      (cond ((null a) b)
            ((null b) a)
            (t
             (prog (val end)
                   (setq val
                         (setq end
                               (cond ((funcall %%cfn (car a) (car b))
                                      (prog1 a (setq a (cdr a))))
                                     (t (prog1 b (setq b (cdr b)))))))
              loop (cond ((null a) (rplacd end b) (return val))
                         ((null b) (rplacd end a) (return val))
                         ((funcall %%cfn (car a) (car b))
                          (rplacd end a)
                          (setq a (cdr a)))
                         (t (rplacd end b) (setq b (cdr b))))
                   (setq end (cdr end))
                   (go loop))))))

(def notany
     (lambda (somex somefn1 somefn2) (not (some somex somefn1 somefn2))))

(def notevery
     (lambda 
      (everyx everyfn1 everyfn2)
      (not (every everyx everyfn1 everyfn2))))

(def Cnth
     (lambda 
      (x n)
      (cond ((> 1 n) (cons nil x))
            (t
             (prog nil
              lp   (cond ((or (atom x) (eq n 1)) (return x)))
                   (setq x (cdr x))
                   (setq n (sub1 n))
                   (go lp))))))

(def nthchar
     (lambda 
      (x n)
      (cond ((plusp n) (car (Cnth (explodec x) n)))
            ((minusp n) (car (Cnth (reverse (explodec x)) (minus n))))
            ((zerop n) nil))))

(def prinlev
     (lambda 
      ($%x $%n)
      (cond ((not (dtpr $%x)) (print $%x))
            ((and %trcflg (eq (car $%x) 'evl-trace) (dtpr (cdr $%x)))
             (prinlev (cadr $%x) $%n))
            ((and %trcflg
                  (eq (car $%x) '\#)
                  (dtpr (cdr $%x))
                  (dtpr (cddr $%x)))
             (prinlev (caddr $%x) $%n))
            ((eq %prevfn% $%x) (princ '//\#//))
            ((eq $%n 0) (princ '"& "))
            (t
             (prog ($%kk $%cl)
                   (princ
                    (cond ($%dotflg (setq $%dotflg nil) '"... ")
                          (t '"(")))
                   (prinlev (car $%x) (sub1 $%n))
                   (setq $%kk $%x)
              lp   (cond
                    ((memcdr $%x $%kk)
                     (cond ($%cl (princ '" ...]") (return nil))
                           (t (setq $%cl t)))))
                   (cond ((not (*** eq (cdr $%kk) (unbound)))
                          (setq $%kk (cdr $%kk)))
                         (t (princ '" . unbound)") (return nil)))
                   (cond ((null $%kk) (princ '")") (return nil))
                         ((atom $%kk)
                          (princ '" . ")
                          (patom $%kk)
                          (princ '")")
                          (return nil)))
                   (princ '" ")
                   (prinlev (car $%kk) (sub1 $%n))
                   (go lp))))))

(def printlev (lambda ($%x $%n) (terpri) (prinlev $%x $%n) $%x))



(def remove
     (lambda 
      (elt list)
      (cond ((atom list) list)
            ((equal (car list) elt) (remove elt (cdr list)))
            ((cons (car list) (remove elt (cdr list)))))))

(def some
     (lambda 
      (somex somefn1 somefn2)
      (prog nil
       a    (cond ((null somex) (return nil))
                  ((funcall somefn1 (car somex)) (return somex))
                  (t (setq somex
                           (cond ((null somefn2) (cdr somex))
                                 (t (funcall somefn2 somex))))
                     (go a))))))

; this probably should have another names since is   ****
; just a duplication of an existing function and since it has a
; default second arg which I believe is not documented.
(def sort
     (lambda 
      (%%l %%cfn)
      (prog (val n)
            (cond ((null %%cfn) (setq %%cfn (function alphalessp))))
            (setq n 0)
            (setq val (sort1 0))
       loop (cond ((null %%l) (return val))
                  (t (setq val (merge1 val (sort1 n)))
                     (setq n (add1 n))
                     (go loop))))))

(def sort1
     (lambda 
      (n)
      (cond ((null %%l) nil)
            ((zerop n)
             (prog (run end)
                   (setq run %%l)
              loop (setq end %%l)
                   (setq %%l (cdr %%l))
                   (cond ((or (null %%l)
                              (not (funcall %%cfn (car end) (car %%l))))
                          (rplacd end nil)
                          (return run))
                         (t (go loop)))))
            (t (merge1 (sort1 (sub1 n)) (sort1 (sub1 n)))))))

(def subpair
     (lambda 
      (old new expr)
      (cond (old (subpr expr old (or new '(nil)))) (t expr))))

(def subpr
     (lambda 
      (expr l1 l2)
      (prog (d a)
            (cond ((atom expr) (go lp))
                  ((setq d (cdr expr)) (setq d (subpr d l1 l2))))
            (setq a (subpr (car expr) l1 l2))
            (return
             (cond ((or (neq a (car expr)) (neq d (cdr expr))) (cons a d))
                   (t expr)))
       lp   (cond ((null l1) (return expr))
                  (l2 (cond ((eq expr (car l1)) (return (car l2)))))
                  (t (cond ((eq expr (caar l1)) (return (cdar l1))))))
            (setq l1 (cdr l1))
            (and l2 (setq l2 (or (cdr l2) '(nil))))
            (go lp))))

(def tailp
     (lambda 
      (x y)
      (and x
           (prog nil
            lp   (cond ((atom y) (return nil)) ((eq x y) (return x)))
                 (setq y (cdr y))
                 (go lp)))))

(def tconc
     (lambda 
      (p x)
      (cond ((atom p) (cons (setq x (ncons x)) x))
            ((dtpr (cdr p)) (rplacd p (cdr (rplacd (cdr p) (ncons x)))))
            (t (rplaca p (cdr (rplacd p (ncons x))))))))

(def ttyesno (lambda nil (yesno (read))))

(def yesno (lambda (x) (selectq x ((t y yes) t) ((nil n no) nil) x)))

; this really duplicates a function in auxfns1.l but this does more
; error checking.
(defun nth (N L)
	(cond ((null L)nil)
	      (t(do ((LCDR L (cdr LCDR))
		     (COUNT N (1- COUNT)))
		    ((or (and (atom LCDR) LCDR
   		              (err '"non-proper list passed to nth"))
		         (or (lessp COUNT 0)(zerop COUNT)))
		     (car LCDR))
		    nil))))
(declare (special piport))
(def dc-dskin			; LWE Hacking to compile OK
   (nlambda (args)
	    (prog (tmp tmp1 tmp2)
		  (setq tmp
			(prog (c cc)
			      (setq cc (get (car args) 'comment))
			      loop
			      (cond ((not cc)(return nil)))
			      (setq c (car cc))
			      (cond ((eq (car c)(cadr args))
				     (return nil)))
			      (setq cc (cdr cc))
			      (go loop)))
		  (setq tmp2 piport)
		  (setq tmp1 (get-comment 27 tmp2))
		  (cond (tmp  (disgusting tmp
					  (cons (cadr args)
						(cons (caddr args) tmp1))))
			(t (putprop (car args)
				    (cons (cons (cadr args)
						(cons (caddr args) tmp1))
					  (get (car args) 'comment))
				    'comment)))
		  (mark!changed (car args))
		  (return nil))))

(def disgusting (lambda (a b) ; (rplaca a b)))
b))

(def get-comment
  (lambda (stopper piport)
    (prog (ans line)
          (cond ((eq 10 (tyipeek piport)) (tyi piport)))
     l:   (setq line nil)
;          (until (member (car line) (list 10 stopper))
;                 (setq line (cons (tyi piport) line)))
	   (prog nil loop
		(cond ((member (car line)(list 10 stopper))
		       (return nil)))
	        (setq line (cons (tyi piport) line))
		(go loop))
          (setq ans (cons (implode (dreverse (cdr line))) ans))
          (cond ((eq (car line) 10) (go l:)) (t (return (dreverse ans)))))))
