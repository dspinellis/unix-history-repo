(setq SCCS-cs284 "@(#)cs284.l      1.1     4/20/81")
; this file contains functions which were taught in cs 284 class
; and which are in the book Artificial Intelligence Programming
; Techniques by Charniak, Reisneck, and McDermott.(1980).
;
; for a detailed desciption of how they work and how to use them
; please see that book.
;
; bascially they do retrieve and insertions of patterns into
;	a discrimination net. The discrimination net serves as
;	a database.
;	(fetch pattern database) will retrieve all patterns that
;	are in the database and match pattern.
;	(index pattern database) will insert the given pattern into
;	the data base if it is not already there.
;	(retrieve pattern database) will do the same as fetch except
;	will search the database for (<- a b) rules and use these
;	to match pattern a to pattern b.
;	
; Also, one can put properties on property lists using the functions
;	e-get and e-put.
;
; (e-get list property database)
; and (e-put list value property database)
;
;	this were all the function i used outside this file for
;	my plan understanding program. And all that needs to be
;	understood in order to understand the file plan.l
;
;	Databases are initialized by (setq databasename (link nil nil))
;
;	This file is compiled liszt cs284.l
;		(-u flag is not needed since ucifnc is loaded explicitly)
;		and will generally be fasl'd into a system which
;		uses it.
;
(declare (macros t))
(eval-when (compile eval)
  (fasl 'ucifnc))

(record-type pcvar *var* (sym))
(drm /? (lambda () (list '*var* (read))))

(record-type bdg nil (sym val))

(de unify (pat1 pat2) (unify1 pat1 pat2 nil))

(de unify1 (pat1 pat2 subst-so-far)
  (cond ((is-pcvar pat1)
	 (var-unify pat1 pat2 subst-so-far))
	((is-pcvar pat2)
	 (var-unify pat2 pat1 subst-so-far))
	((atom pat1) (and (equal pat1 pat2) (list subst-so-far)))
	((atom pat2) nil)
	((for (subst-from-car in
			      (unify1 (car pat1) (car pat2) subst-so-far))
	      (splice (unify1 (cdr pat1) (cdr pat2) subst-from-car))))))

(de var-unify (v p sub)
  (let (bdg (sym-lookup (sym:pcvar v) sub))
       (cond (bdg (unify1 (val:bdg bdg) p sub))
	     ((equal v p) (list sub))
	     ((is-pcvar v)
	      (list (subcons (bdg (sym:pcvar v) p) sub)))
	     ((is-pcvar p)
	      (list (subcons (bdg (sym:pcvar p) v) sub)))
	     ((not (occurs-in v p sub))
	      (list (subcons (bdg (sym:pcvar v) p) sub))))))

(de occurs-in (v p sub)
  (cond ((atom p) nil)
	((is-pcvar p)
	 (or (equal v p)
	     (let (b (sym-lookup (sym:pcvar p) sub))
		  (and b (occurs-in v (val:bdg b) sub)))))
	((some '(lambda (y) (occurs-in v y sub)) p))))

(de sym-lookup (sym sub) (assoc sym sub))

(de subcons (bdg sub) (cons bdg sub))

(record-type link nil (key . alist ))
(record-type terminal nil (key . datum ))
  
(de traverse-links (item link)
  (cond ((null link) nil)
	((atom item) (traverse-link item link))
	((traverse-links
	  (cdr item)
	  (traverse-links (car item) (traverse-links '*cons* link))))))
  
(de traverse-link (key link) 
  (let (temp (assoc key (alist:link link)))
       (cond (temp (list temp)))))
  
(de index-off (pattern link)
  (for (terminal in (traverse-links2 pattern link))
       (splice (:= (datum:terminal terminal) nil))))
  
(de index (item link)
  (let (terminal-link (establish-links item link))
       (cond ((datum:terminal terminal-link))
	     (t (let (newdatum (terminal item nil))
		     (:= (datum:terminal terminal-link) newdatum)
		newdatum)))))

(de e-put (exp v p database)
  (cond ((atom exp) (putprop exp v p))
	((putprop (index exp database) v p))))

(de e-get (exp p database)
  (cond ((atom exp) (list (get exp p)))
	(t ((lambda (y)
		    (cond ((equal y '(nil))
			   nil)
			  (t y)))
	    (for (x in (fetch-all exp database))
		 (splice (list (get x p))))))))
  
(de establish-links (item link)
  (cond ((atom item) (establish-link item link))
	((is-pcvar item) (establish-link '*var* link))
	((establish-links
	  (cdr item)
	  (establish-links (car item)
			   (establish-link '*cons* link))))))
  
(dm add-to-alist (exp) (cons 'cons (cdr exp)))

(de establish-link (key link)
  (cond ((assoc key (alist:link link)))
	((let (new-link (link key nil))
	      (:= (alist:link link) (add-to-alist new-link *-*))
	      new-link))))
  
(de fetch (pattern link)
  (for (x in (fetch-all pattern link))
       (splice (copy (list (key:terminal x))))))
  
(de fetch-all (pattern link)
  (for (y in (for (terminal in (traverse-links2 pattern link))
		  (splice (copy (list (datum:terminal terminal))))))
       (splice (cond ((unify (key:terminal y) pattern) (list y))))))
  
(de traverse-links2 (pattern link)
  (cond ((is-mvar pattern) (skip-exp link))
	((nconc
	  (cond ((atom pattern) (traverse-link pattern link))
		((for
		  (sub-sub in
			   (for (sub-link in (traverse-link '*cons* link))
				(splice
				 (traverse-links2 (car pattern)
						  sub-link))))
		  (splice (traverse-links2 (cdr pattern) sub-sub)))))
	  (traverse-link '*var* link)))))
  
(de is-mvar (l) (and (not (atom l))
		     (symbolp (cadr l))
		     (equal (car l) '*var*)))
  
(de skip-exp (link)
  (for (lower-link in (alist:link link))
       (splice (cond ((not (eq (key:link lower-link) '*cons*))
		      (list lower-link))
		     (t 
		      (for (l-link in (skip-exp lower-link))
			   (splice (skip-exp l-link))))))))

(record-type db (assertions))
  
(de clean-up-bdgs (request answer)
  (for (bdg in answer)
       (filter (and (occurs-in (pcvar (sym:bdg bdg)) request nil)
		    (bdg (sym:bdg bdg) (varsubst (val:bdg bdg) 
						 answer))))))
  
(declare (special dis-tree))

(de retrieve (request dis-tree)
  (retrieve1 request))
  
(declare (special goal and or not eval))
  
(defprop and conj-retrieve retriever)
(defprop or disj-retrieve retriever)
(defprop not not-retrieve retriever)
(defprop eval eval-retrieve retriever)

(de not-retrieve (request)
  (cond ((retrieve1 (car request)) nil)
	(t '(nil))))

(de eval-retrieve (request)
  (cond ((eval (car request))
	 '(nil))))
  
(de disj-retrieve (disjuncts)
  (for (pattern in disjuncts)
       (splice (retrieve1 pattern))))
  
(de pcvar-val (pcvar sub)
  (let (bdg (sym-lookup (sym:pcvar pcvar) sub))
       (cond (bdg (varsubst (val:bdg bdg) sub))
	     (t pcvar))))
  
(declare (special *assoclist*))
  
(de vars-rename (pattern)
  ((lambda (*assoclist*) (var-rename2 pattern)) nil))
  
(de var-rename2 (pat)
  (cond ((atom pat) pat)
	((is-pcvar pat) 
	 (pick-new-var (val:bdg pat)))
	(t (cons (var-rename2 (car pat))
		 (var-rename2 (cdr pat))))))
  
(de pick-new-var (variable)
  (let (temp (assoc variable *assoclist*))
       (cond ((not (symbolp variable)) nil)
	     (temp (cadr temp))
	     (t (setq *assoclist* (cons
				   (bdg variable (newvar))
				   *assoclist*))
		(val:bdg (sym-lookup variable *assoclist*))))))
  
  
(de newvar ()
  (list '*var* (gensym '*var*)))
  
(de varsubst (*pattern* *sub*)
  (cond ((atom *pattern*) *pattern*)
	((is-pcvar *pattern*)
	 (let (temp (sym-lookup (sym:pcvar *pattern*) *sub*))
	      (cond (temp (varsubst (val:bdg temp) *sub*))
		    (t *pattern*))))
	(t (cons (varsubst (car *pattern*) *sub*)
		 (varsubst (cdr *pattern*) *sub*)))))

(de retrieve1 (request)
  (for (temp in (cond ((null request) nil)
		      ((and (atom (car request))
			    (get (car request) 'retriever))
		       (funcall (get (car request) 'retriever) (cdr request)))
		      ((nconc (for (a in (fetch request dis-tree))
				   (splice (unify request (vars-rename a))))
			      (let (implpat (list '<- request '?subreq))
				   (for (a in (fetch implpat dis-tree))
					(splice (chain implpat a))))))))
       (save (clean-up-bdgs request temp))))

(de chain (implpat possibilities)
  (for (sub in (unify (vars-rename possibilities) implpat))
       (splice
	(for (answer in (retrieve1 (pcvar-val '?subreq sub)))
	     (save (append answer sub))))))
  
(de conj-retrieve (conjuncts) (conj-retrieve1 conjuncts nil))
  
(de conj-retrieve1 (conjuncts answer-so-far)
  (cond ((null conjuncts) (list answer-so-far))
	((for (answer in (retrieve1 (varsubst (car conjuncts) answer-so-far)))
	      (splice
	       (conj-retrieve1
		(cdr conjuncts)
		(append answer
			(varsubst answer-so-far answer))))))))

(df mesg (l)
  (for (a in l)
       (do (cond ((eq a 'nl) (terpri))
		 ((patom (eval a)))))))

~  Here we have the form/match functions.
~	These have not been carefully debugged yet,
~	but are offered as a possible start to the user
~	and suggestions of what can be done.  The user
~	should read other examples.
~

(declare (*fexpr form2 addspec))

(defun form macro (exp)
  `(progn 'compile
	  (form2 ,@(cdr exp))
	  (dm ,(cadr exp) (l)
	      (append
	       (list 'form (cadr l) 'always ',(cadr exp))
	       (cddr l)))))
  
(df form2 (exp)
  (let (name (car exp) isa (caddr exp) slots (cddddr exp))
       (:= (get name 'type) 'form)
       (addspec-e name 'isa
		  (cond ((eq (cadr exp) 'always)
			 'always)
			('=))
		  isa)
       (for (slot in slots)
	    (do (loop (initial slot-name (car slot) aspects (cdr slot))
		      (while aspects)
		      (do (addspec-e name slot-name 
				     (car aspects) 
				     (cadr aspects)))
		      (next aspects (cddr aspects)))))))
  
(df addspec (exp) (apply 'addspec-e exp))
  
(de addspec-e (name slot aspect val)
  (:= (get slot 'slot-name) t)
  (let (aspect-val (assoc aspect (get name slot)))
       (cond (aspect-val (:= (cdr aspect-val) val))
	     (t (addprop name (cons aspect val) slot)))))
  
(de slot-name-p (name) (get name 'slot-name))
  
(de form-p (name) (eq (get name 'type) 'form))
  
(declare (special *form*))
  
(de slot-val (*form* slot) (slot-val2 *form* slot))
  
(de slot-val2 (name slot)
  (loop (initial answer nil an-isa name)
	(while an-isa)
	(do (:= answer
		(cond ((get-val an-isa slot))
		      ((eval (get-aspect an-isa slot 'if-needed))))))
	(next an-isa (get-isa an-isa))
	(until answer)
	(result answer)))
  
(de get-aspect (name slot aspect)
  (let (val (assoc aspect (get name slot)))
       (cond (val (cdr val)))))
  
(de get-val (name slot) (cond ((get-aspect name slot 'always))
			      ((get-aspect name slot '=))))
  
(de get-isa (name) (get-val name 'isa))

(de match (inst pat)
  (cond ((equal inst pat))
	((not (and (form-p inst) (form-p pat))) nil)
	((isa-p inst pat))
	((slot-by-slot-match inst pat))))

(de compatible (inst pat slot)
  (let (inst-val (slot-val inst slot)
		 pat-always (slot-always pat slot))
       (or (null inst-val) (null pat-always)
	   (match inst-val pat-always))))

(declare (special inst pat))

(de slot-by-slot-match (inst pat)
  (every 
   '(lambda (an-isa)
	    (loop (initial p-list (plist an-isa)
			   prop nil value nil
			   p2 nil
			   mismatch-flag nil)
		  (while (:= prop (pop p-list)))
		  (do (pop p-list)
		      (cond ((and (slot-name-p prop)
				  (not (eq prop 'isa)))
			     (setq value (getproperty an-isa prop))
			     (setq p2 (getproperty inst prop))
			     (cond ((and p2
					 value
					 (not (equal value p2)))
				    (:= mismatch-flag t))))))
		  (until mismatch-flag)
		  (result (not mismatch-flag))))
   (isas pat)))

(de get-p-list (atm) (plist atm))

(de isa-p (form1 form2) (member form2 (isas form1)))

(de slot-always (form slot)
  (let (ans (some '(lambda (an-isa) (get-aspect an-isa slot 'always))
		  (isas form)))
       (cond (ans (get-aspect (car ans) slot 'always)))))

(de isas (f)
  (loop (initial answer nil)
	(while f)
	(do (push f answer))
	(next f (get-isa f))
	(result (reverse answer))))
