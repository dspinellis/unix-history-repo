;----------- macros for the compiler -------------

(setq RCS-cmacros
   "$Header: cmacros.l,v 1.14 87/12/15 16:55:07 sklower Exp $")

(declare (macros t))			; compile and save macros

; If we are making an interpreted version, then const.l hasn't been
; loaded yet...
(eval-when (compile eval)
   (or (get 'const 'loaded) (load '../const.l)))

;--- comp-err
;    comp-warn
;    comp-note
;    comp-gerr
; these are the compiler message producing macros.  The form is
; (comp-xxxx val1 val2 val3 ... valn) , all values are printed according
;  to this scheme. If vali is an atom, it is patomed, if vali is a
;  list, it is evaluated and printed. If vali is N a newline is printed
; 
; furthermore
;    the name of the current function is printed first
;    after comp-err prints the message, it does a throw to Comp-err .
;    errors are preceeded by Error: 
;	warnings by %Warning: and
;	notes by %Note:
;     The message is sent to the message file
;
(def comp-err
   (macro (l)
	  `(progn (comp-msg "?Error: " v-ifile ": " g-fname ": "
			    ,@(cdr l) )
		  (setq er-fatal (1+ er-fatal))
		  (throw nil Comp-error))))

(def comp-warn
   (macro (l)
	  `(progn (setq er-warn (1+ er-warn))
		  (cond (fl-warn
			    (comp-msg "%Warning: " v-ifile ": "  g-fname ": "
				      ,@(cdr l)))))))

(def comp-note
   (macro (l)
	  `(progn (cond (fl-verb
			    (comp-msg "%Note: " v-ifile ": "  ,@(cdr l)))))))

(def comp-gerr
   (macro (l)
	  `(progn (comp-msg
		      "?Error: " v-ifile ": " g-fname ": ",@(cdr l))
		  (setq er-fatal (1+ er-fatal)))))

;--- comp-msg - port
;	      - lst
;  prints the lst to the given port.  The lst is printed in the manner
; described above, that is atoms are patomed, and lists are evaluated
; and printed, and N prints a newline.   The output is always drained.
;
(def comp-msg
   (macro (lis)
	  (do ((xx (cdr lis) (cdr xx))
	       (res nil))
	      ((null xx)
	       `(progn ,@(nreverse (cons '(terpri) res))))
	      (setq res
		    (cons (cond ((atom (car xx))
				 (cond ((eq (car xx) 'N) '(terpr))
				       ((stringp (car xx)) `(patom ,(car xx)))
				       (t `(niceprint ,(car xx)))))
				(t `(niceprint ,(car xx))))
			  res)))))

(def niceprint
   (macro (l)
	  `((lambda (float-format) (patom ,(cadr l))) "%.2f")))

;--- standard push macro
; (Push stackname valuetoadd)

(defmacro Push (atm val)
  `(setq ,atm (cons ,val ,atm)))

;--- unpush macro - like pop except top value is thrown away
(defmacro unpush (atm)
  `(setq ,atm (cdr ,atm)))

;--- and an increment macro
(defmacro incr (atm)
  `(setq ,atm (1+ ,atm)))

(defmacro decr (atm)
  `(setq ,atm (1- ,atm)))

;--- add a comment
(defmacro makecomment (arg)
  `(cond (fl-comments (setq g-comments (cons ,arg g-comments)))))

;--- add a comment irregardless of the fl-comments flag
(defmacro forcecomment (arg)
  `(setq g-comments (cons ,arg g-comments)))

;--- write to the .s file
(defmacro sfilewrite (arg)
  `(patom ,arg vp-sfile))

(defmacro sfilewriteln (arg)
  `(msg (P vp-sfile) ,arg N))

;--- Liszt-file  :: keep track of rcs info regarding part of Liszt
;  This is put at the beginning of a file which makes up the lisp compiler.
; The form used is   (Liszt-file name rcs-string)
; where name is the name of this file (without the .l) and rcs-string.
;
(defmacro Liszt-file (name rcs-string)
   `(cond ((not (boundp 'Liszt-file-names))
	   (setq Liszt-file-names (ncons ,rcs-string)))
	  (t (setq Liszt-file-names
		   (append1 Liszt-file-names ,rcs-string)))))

(eval-when (compile eval load)
   (defun immed-const (x)
	  (get_pname (concat #+(or for-vax for-tahoe) "$" #+for-68k "#" x))))

; Indicate that this file has been loaded, before
(putprop 'cmacros t 'version)

;-------- Instruction Macros

#+(or for-vax for-tahoe)
(defmacro e-add (src dst)
   `(e-write3 'addl2 ,src ,dst))

#+(or for-vax for-tahoe)
(defmacro e-sub (src dst)
   `(e-write3 'subl2 ,src ,dst))

#+(or for-vax for-tahoe)
(defmacro e-cmp (src dst)
   `(e-write3 'cmpl ,src ,dst))

(defmacro e-tst (src)
   `(e-write2 'tstl ,src))

#+for-vax
(defmacro e-quick-call (what)
   `(e-write2 "jsb" ,what))

#+for-tahoe
(defmacro e-quick-call (what)
   `(e-write3 "calls" "$4" ,what))

#+for-68k
(defmacro e-quick-call (what)
  `(e-write2 "jsbr" ,what))


;--- e-add3 :: add from two sources and store in the dest
;--- e-sub3 :: subtract from two sources and store in the dest

; WARNING:  if the destination is an autoincrement addressing mode, then
;	this will not work for the 68000, because multiple instructions
;	are generated:
;		(e-add3 a b "sp@+")
;	is
;		movl b,sp@+
;		addl a,sp@+	(or addql)
#+(or for-vax for-tahoe)
(defmacro e-add3 (s1 s2 dest)
   `(e-write4 'addl3 ,s1 ,s2 ,dest))

#+for-68k
(defmacro e-add3 (s1 s2 dest)
   `(progn
       (e-write3 'movl ,s2 ,dest)
       (e-add ,s1 ,dest)))

#+(or for-vax for-tahoe)
(defmacro e-sub3 (s1 s2 dest)
   `(e-write4 'subl3 ,s1 ,s2 ,dest))

#+for-68k
(defmacro e-sub3 (s1 s2 dest)
   `(progn
       (e-write3 'movl ,s2 ,dest)
       (e-sub ,s1 ,dest)))

(defmacro d-cmp (arg1 arg2)
  `(e-cmp (e-cvt ,arg1) (e-cvt ,arg2)))

(defmacro d-tst (arg)
  `(e-tst (e-cvt ,arg)))

;--- d-cmpnil :: compare an IADR to nil
;
(defmacro d-cmpnil (iadr)
   #+(or for-vax for-tahoe) `(d-tst ,iadr)
   #+for-68k `(d-cmp 'Nil ,iadr))

(defmacro e-cmpnil (eiadr)
   #+(or for-vax for-tahoe) `(break 'e-cmpnil)
   #+for-68k `(e-cmp (e-cvt 'Nil) ,eiadr))

(defmacro e-call-qnewint ()
   `(e-quick-call '_qnewint))

(defmacro C-push (src)
   #+for-68k `(e-move ,src '#.Cstack)
   #+(or for-vax for-tahoe) `(e-write2 'pushl ,src))

(defmacro L-push (src)
   `(e-move ,src '#.np-plus))

(defmacro C-pop (dst)
   `(e-move '#.unCstack ,dst))

#+(or for-vax for-68k)
(defmacro L-pop (dst)
   `(e-move '#.np-minus ,dst))

#+for-tahoe
(defmacro L-pop (dst)
   `(progn (e-sub '($ 4) '#.np-reg)
	   (e-move '(0 #.np-reg) ,dst)))

