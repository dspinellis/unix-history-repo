(setq rcs-common2-
   "$Header: common3.l,v 1.4 84/02/29 23:23:35 layer Exp $")

;;
;; common3.l				-[Sat Sep 10 10:51:18 1983 by jkf]-
;;
;;

(declare (macros t))

(defun litatom macro (x)
  `(and (atom . ,(cdr x))
	(not (numberp . ,(cdr x)))))

; This function really should compile optimally in-line
;
(defun nequal (arg1 arg2)
  (not (equal arg1 arg2)))

(defun lineread (&rest args)
   (let (flag port)
      (mapc (function		; get the options
	       (lambda (x)
		  (cond ((portp x) (setq port x))
			((setq flag x)))))
	    args)
      (cond ((not (and flag	; flag for empty line
		       (eq (tyipeek port) #\lf)
		       (tyi port)))
	     (prog (input)
		(setq input (ncons nil))  ; initialize for tconc.
		(tconc input (read port))	; do read to make sure
						; an s-expression gets read
		loop
		(cond ((not (eq (tyipeek port) #\lf))
		       (tconc input (read port))
		       (go loop))
		      ( t ; the actual list is in the CAR.
			(tyi port)
			(return (car input)))))))))

(defun defv fexpr (l)
  (set (car l) (cadr l)))


(defun initsym (&rest l)
   (mapcar (function initsym1) l))

(defun initsym1 expr (l)
   (prog (num)
      (cond ((dtpr l)
	     (setq num (cadr l))
	     (setq l (car l)))
	    ( t (setq num 0)))
      (putprop l num 'symctr)
      (return (concat l num))))

(defun newsym (name)
   (concat name
	   (putprop name
		    (1+ (or (get name 'symctr)
			    -1))
		    'symctr)))

(defun oldsym (sym)
   (cond ((get sym 'symctr) (concat sym (get sym 'symctr)))
	 ( t sym)))

(defun allsym (name)
   (prog (num symctr syms)
      (cond ((dtpr name)
	     (setq num (cadr name))
	     (setq name (car name)))
	    ( t (setq num 0)))
      (or (setq symctr (get name 'symctr))
	  (return))
      loop
      (and (>& num symctr)
	   (return syms))
      (setq syms (cons (concat name symctr) syms))
      (setq symctr (1- symctr))
      (go loop)))

(defun remsym (&rest l)
   (mapcar (function remsym1) l))

(defun remsym1 expr (l)
   (prog1 (oldsym (cond ((dtpr l) (car l))
			( t l)))
	  (mapc (function remob) (allsym l))
	  (cond ((dtpr l)
		 (putprop (car l) (1- (cadr l)) 'symctr))
		( t (remprop l 'symctr)))))

(defun symstat (&rest l)
   (mapcar (function (lambda (k)
			(list k (get k 'symctr))))
	   l))

;; from peter@renoir
(defun wide-print-list (given-list &optional (left-margin (nwritn)))
  ;	given a (presumably long) list, print it as wide as possible.
  (declare (special lpar rpar))
  (let ((max-width 78))
       (tab left-margin)
       (cond ((not (listp given-list))
	      (patom given-list))
	     ((null given-list)
	      (patom nil))
	     (t
	      (patom lpar)
	      (do ((left given-list (cdr left))
		   (need-space-p nil t))
		  ((null left) nil)
		  (cond (need-space-p
			 (patom " ")))
		  (let* ((element (car left))
			 (length (flatc element))
			 (used (nwritn))
			 (available (- max-width used)))
			(cond ((>= length available)
			       (tab (1+ left-margin))))
			(cond ((listp element)
			       (wide-print-list element))
			      (t
			       (patom element)))))
	      (patom rpar)))))
