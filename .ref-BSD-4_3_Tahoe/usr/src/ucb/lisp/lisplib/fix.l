(setq rcs-fix-
   "$Header: /usr/lib/lisp/RCS/fix.l,v 1.2 83/08/06 08:39:58 jkf Exp $")

; vi: set lisp :

(eval-when (compile eval)
  (or (get 'cmumacs 'version) (load 'cmumacs)))

; LWE 1/11/81 Hack hack....
;
; LWE 1/11/81 Bet you didn't know this, but this won't work INTERPRETED,
;	      but Dave assures me it works compiled. (In MACLisp...)
; 
(declare (special cmd frame x cnt var init label part incr limit selectq))

(dv fixfns
    ((*** This is FIXIT written by David Touretzky and adapted to Franz by Don
      Cohen)
     (declare (special framelist rframelist interrupt-handlers handler-labels)
              (special prinlevel prinlength evalhook-switch traced-stuff)
              (special lastword piport hush-debug)
              (*fexpr editf step type))
     (sstatus feature fixit)
     (*rset t)
     ER%tpl
     fixit
     debug
     debug-iter
     debug1
     debug-bktrace
     debug-print
     debug-print1
     debug-findcall
     debug-replace-function-name
     debug-scanflist
     debug-scanstk
     debug-getframes
     debug-nextframe
     debug-upframe
     debug-dnframe
     debug-upfn
     debug-dnfn
     debug-showvar
     debug-nedit
     debug-insidep
     debug-findusrfn
     debug-findexpr
     debug-pop
     debug-where
     debug-sysp
     interrupt-handlers
     handler-labels
     (or (boundp 'traced-stuff) (setq traced-stuff nil))
     (or (boundp 'evalhook-switch) (setq evalhook-switch nil))
     (setq hush-debug nil)))

(or (boundp 'traced-stuff) (setq traced-stuff nil))
(or (boundp 'evalhook-switch) (setq evalhook-switch nil))
(or (boundp 'debug-sysmode) (setq debug-sysmode nil))
(setq hush-debug nil)

(*** This is FIXIT written by David Touretzky and adapted to Franz by Don Cohen)

(declare (special framelist rframelist interrupt-handlers handler-labels)
         (special prinlevel prinlength evalhook-switch traced-stuff)
         (special lastword piport hush-debug debug-sysmode)
         (*fexpr editf step type))

(defvar fixit-eval nil)
(defvar fixit-print nil)
(defvar fixit-pp nil)

(sstatus feature fixit)

(*rset t)

; (jkf) it is not clear that you want this to take over on all errors,
; but the cmu people seem to want that.
#+cmu (progn 'compile
	     (dv ER%tpl fixit)
	     (dv ER%all fixit) ; LWE 1/17/81 MAYBE THIS WILL FIX THIS code
	     )

;--- eval, print and pretty-print functions are user-selectable by just
; assigning another value to fixit-eval, fixit-print and fixit-pp.
;
(defmacro fix-eval (&rest args)
   `(cond ((and fixit-eval
		(getd fixit-eval))
	   (funcall fixit-eval ,@args))
	  (t (eval ,@args))))

(defmacro fix-print (&rest args)
   `(cond ((and fixit-print
		(getd fixit-print))
	   (funcall fixit-print ,@args))
	  (t (print ,@args))))

(defmacro fix-pp (&rest args)
   `(cond ((and fixit-pp
		(getd fixit-pp))
	   (funcall fixit-pp ,@args))
	  (t ($prpr ,@args))))

(def fixit
  (nlambda (l)
    (prog (piport)
          (do nil (nil) (eval (cons 'debug l))))))

(def debug
  (nlambda (params)
    (prog (cmd frame framelist rframelist nframe val infile)
          (setq infile t)
          (and evalhook-switch (step nil))
          (setq rframelist
                (reverse
                 (setq framelist
                       (or (debug-getframes)
                           (list
                            (debug-scanstk '(nil) '(debug)))))))
          (setq frame (debug-findexpr (car framelist)))
          ;(tab 0)
          (cond
           ((and (car params) (not (eq (car params) 'edit)))
            (terpri)
            (princ '|;debug: |)
            (princ (cadddr params))
            (cond ((cddddr params)
                   (princ '| -- |)
                   (fix-print (cddddr params))))
            (terpri)
            (go loop)))
          (debug-print1 frame nil)
          (terpri)
          (cond (hush-debug (setq hush-debug nil) (go loop))
                ((not (memq 'edit params)) (go loop)))
          (drain nil)
          (princ '|type e to edit, <cr> to debug: |)
          (setq val (tyi))
          (cond ((or (= val 69) (= val 101))
                 (and (errset (debug-nedit frame))
                      (setq cmd '(ok))
                      (go cmdr)))
                ((or (= val 78) (= val 110)) (terpri) (debug-pop)))
     loop (terpri)
          (princ ':)
          (cond ((null (setq cmd (lineread))) (reset)))
     cmdr (cond
           ((dtpr (car cmd))
            (setq val (fix-eval (car cmd) (cadddr frame)))
            (fix-print val)
            (terpri)
            (go loop)))
          (setq nframe (debug1 cmd frame))
          (and (not (atom nframe)) (setq frame nframe) (go loop))
          (fix-print (or nframe (car cmd)))
          (princ '" Huh? - type h for help")
          (go loop))))

(def debug-iter
  (macro (x)
    (cons 'prog
          (cons 'nil
                (cons 'loop
                      (cons (list 'setq 'nframe (cadr x))
                            '((setq cnt (|1-| cnt))
                              (and (or (null nframe) (zerop cnt))
                                   (return nframe))
                              (setq frame nframe)
                              (go loop))))))))

(def debug1
  (lambda (cmd frame)
    (prog (nframe val topframe cnt item)
          (setq topframe (car framelist))
          (or (eq (typep (car cmd)) 'symbol) (return nil))
          ; if "> name", replace function or variable name with new atom
          (and (eq (car cmd) '>)
               (return (debug-replace-function-name cmd topframe)))
          (and (eq (getchar (car cmd) 1) 'b)
               (eq (getchar (car cmd) 2) 'k)
               (return (debug-bktrace cmd frame)))
          (setq cnt
                (cond ((fixp (cadr cmd)) (cadr cmd))
                      ((fixp (caddr cmd)) (caddr cmd))
                      (t 1)))
          (and (< cnt 1) (setq cnt 1))
          (setq item
                (cond ((symbolp (cadr cmd)) (cadr cmd))
                      ((symbolp (caddr cmd)) (caddr cmd))))
          (and item
               (cond ((memq (car cmd) '(u up))
                      (setq cmd (cons 'ups (cdr cmd))))
                     ((memq (car cmd) '(d dn))
                      (setq cmd (cons 'dns (cdr cmd))))))
          (selectq (car cmd)
                   (top (debug-print1 (setq frame topframe) nil))
                   (bot (debug-print1 (setq frame (car rframelist)) nil))
                   (p (debug-print1 frame nil))
                   (pp (fix-pp (caddr frame)))
                   (where (debug-where frame))
                   (help
                    (cond ((cdr cmd) (eval cmd))
                          (t (ty |/usr/lib/lisp/fixit.ref|))))
                   ((? h) (ty |/usr/lib/lisp/fixit.ref|))
                   ((go ok)
                    (setq frame (debug-findexpr topframe))
                    (cond ((eq (caaddr frame) 'debug)
                           (freturn (cadr frame) t))
                          (t (fretry (cadr frame) frame))))
                   (pop (debug-pop))
                   (step (setq frame (debug-findexpr frame))
                         (step t)
                         (fretry (cadr (debug-dnframe frame)) frame))
                   (redo (and item
                              (setq frame
                                    (debug-findcall item frame framelist)))
                         (and frame (fretry (cadr frame) frame)))
                   (return (setq val (eval (cadr cmd)))
                           (freturn (cadr frame) val))
                   (edit (debug-nedit frame))
                   (editf
                    (cond ((null item)
                           (setq frame
                                 (or (debug-findusrfn (debug-nedit frame))
                                     (car rframelist))))
                          ((dtpr (getd item))
                           (errset (funcall 'editf (list item))))
                          (t (setq frame nil))))
                   (u (debug-iter (debug-upframe frame))
                      (cond
                       ((null nframe) (terpri) (princ '|<top of stack>|)))
                      (debug-print1 (setq frame (or nframe frame)) nil))
                   (d (setq nframe
                            (or (debug-iter (debug-dnframe frame)) frame))
                      (debug-print1 nframe nil)
                      (cond ((eq frame nframe)
                             (terpri)
                             (princ '|<bottom of stack>|))
                            (t (setq frame nframe))))
                   (up (setq nframe (debug-iter (debug-upfn frame)))
                       (cond
                        ((null nframe) (terpri) (princ '|top of stack|)))
                       (setq frame (or nframe topframe))
                       (debug-print1 frame nil))
                   (dn (setq frame
                             (or (debug-iter (debug-dnfn frame))
                                 (car rframelist)))
                       (debug-print1 frame nil)
                       (cond
                        ((not (eq frame nframe))
                         (terpri)
                         (princ '|<bottom of stack>|))))
                   (ups (setq frame
                              (debug-iter
                               (debug-findcall item frame rframelist)))
                        (and frame (debug-print1 frame nil)))
                   (dns (setq frame
                              (debug-iter
                               (debug-findcall item frame framelist)))
                        (and frame (debug-print1 frame nil)))
		   (sys (setq debug-sysmode (not debug-sysmode))
			(patom "sysmode now ")(patom debug-sysmode) (terpr))
                   (cond ((not (dtpr (car cmd)))
                          (*** should there also be a boundp test here)
                          (debug-showvar (car cmd) frame))
                         (t (setq frame (car cmd)))))
          (return (or frame item)))))

(def debug-replace-function-name 
  (lambda (cmd frame)
    (prog (oldname newname errorcall nframe)
	  (setq errorcall (caddr frame))
	  (cond ((eq (caddddr errorcall) '|eval: Undefined function |)
		 (setq oldname (cadddddr errorcall))
		 (setq newname (cadr cmd))
		 (setq cnt 3.)
		 (setq frame (debug-iter (debug-dnframe frame)))
		 (dsubst newname oldname frame)
		 (fretry (cadr frame) frame))
		((eq (caddddr errorcall) '|Unbound Variable:|)
		 (setq oldname (cadddddr errorcall))
		 (setq newname (eval (cadr cmd)))
		 (setq cnt 3.)
		 (setq frame (debug-iter (debug-dnframe frame)))
		 (dsubst newname oldname frame)
		 (fretry (cadr frame) frame))
		( t (return nil))))))

(def debug-bktrace
  (lambda (cmd oframe)
    (prog (sel cnt item frame nframe)
          (mapc '(lambda (x)
                         (setq sel
                               (cons (selectq x
                                              (f 'fns)
                                              (a 'sysp)
                                              (v 'bind)
                                              (e 'expr)
                                              (c 'current)
                                              'bogus)
                                     sel)))
                (cddr (explodec (car cmd))))
          (setq item
                (cond ((eq (typep (cadr cmd)) 'symbol) (cadr cmd))
                      ((eq (typep (caddr cmd)) 'symbol) (caddr cmd))))
          (cond ((debug-sysp item) (setq sel (cons 'sysp sel)))
                ((not (memq 'sysp sel))
                 (setq sel (cons 'user sel))))
          (setq cnt
                (cond ((fixp (cadr cmd)) (cadr cmd))
                      ((fixp (caddr cmd)) (caddr cmd))
                      (item 1)))
          (cond ((null cnt)
                 (setq frame
                       (cond ((memq 'current sel) oframe)
                             (t (car rframelist))))
                 (go dbpr))
                ((null item)
                 (setq frame (car framelist))
                 (and (or (not (memq 'user sel))
                          (atom (caddr (car framelist)))
                          (not (debug-sysp (caaddr (car framelist)))))
                      (setq cnt (|1-| cnt)))
                 (setq frame
                       (cond ((zerop cnt) frame)
                             ((memq 'user sel)
                              (debug-iter (debug-dnfn frame)))
                             (t (debug-iter (debug-dnframe frame)))))
                 (setq frame (or frame (car rframelist)))
                 (go dbpr))
                (t (setq frame (car framelist))))
          (setq frame
                (cond ((and (= cnt 1)
                            (not (atom (caddr (car framelist))))
                            (eq item (caaddr (car framelist))))
                       (car framelist))
                      ((debug-iter (debug-findcall item frame framelist)))
                      (t (car rframelist))))
     dbpr (debug-print frame sel oframe)
          (cond ((eq frame (car rframelist))
                 (terpri)
                 (princ '|<bottom of stack>|)
                 (terpri))
                (t (terpri)))
          (cond
           ((memq 'bogus sel)
            (terpri)
            (princ (car cmd))
            (princ '| contains an invalid bk modifier|)))
          (return oframe))))

(def debug-print
  (lambda (frame sel ptr)
    (prog (curframe)
          (setq curframe (car framelist))
     loop (cond ((not
                  (and (memq 'user sel)
                       (not (atom (caddr curframe)))
                       (debug-sysp (caaddr curframe))))
                 (debug-print1 curframe sel)
                 (and (eq curframe ptr) (princ '|   <--- you are here|)))
                ((eq curframe ptr)
                 (terpri)
                 (princ '|  <--- you are somewhere in here|)))
          (and (eq curframe frame) (return frame))
          (setq curframe (debug-dnframe curframe))
          (or curframe (return frame))
          (go loop))))

(def debug-print1
  (lambda (frame sel)
    (prog (prinlevel prinlength varlist)
          (and (not (memq 'expr sel))
               (setq prinlevel 2)
               (setq prinlength 5))
          (cond
           ((atom (caddr frame))
            (terpri)
            (princ '|   |)
            (fix-print (caddr frame))
            (princ '| <- eval error|)
            (return t)))
          (and (memq 'bind sel)
               (cond ((memq (caaddr frame) '(prog lambda))
                      (setq varlist (cadr (caddr frame))))
                     ((and (atom (caaddr frame)) (dtpr (getd (caaddr frame))))
                      (setq varlist (cadr (getd (caaddr frame))))))
               (mapc (function
                      (lambda (v)
                              (debug-showvar v
                                             (or (debug-upframe frame)
                                                 frame))))
                     (cond ((and varlist (atom varlist)) (ncons varlist))
                           (t varlist))))
          (and (memq 'user sel)
               (debug-sysp (caaddr frame))
               (return nil))
          (cond ((memq (caaddr frame) interrupt-handlers)
                 (terpri)
                 (princ '<------------)
                 (fix-print (cadr (assq (caaddr frame) handler-labels)))
                 (princ '-->))
                ((eq (caaddr frame) 'debug)
                 (terpri)
                 (princ '<------debug------>))
                ((memq 'fns sel)
                 (terpri)
                 (and (debug-sysp (caaddr frame)) (princ '|  |))
                 (fix-print (caaddr frame)))
                (t (terpri)
                   (fix-print
                    (cond ((eq (car frame) 'eval) (caddr frame))
                          (t (cons (caaddr frame) (cadr (caddr frame))))))))
          (or (not (symbolp (caaddr frame)))
              (eq (caaddr frame) (concat (caaddr frame)))
              (princ '|  <not interned>|))
          (return t))))

(def debug-findcall
  (lambda (fn frame flist)
    (prog nil
     loop (setq frame (debug-nextframe frame flist nil))
          (or frame (return nil))
          (cond ((atom (caddr frame))
                 (cond ((eq (caddr frame) fn) (return frame)) (t (go loop))))
                ((eq (caaddr frame) fn) (return frame))
                (t (go loop))))))

(def debug-scanflist
  (lambda (frame fnset)
    (prog nil
     loop (or frame (return nil))
          (and (not (atom (caddr frame)))
               (memq (caaddr frame) fnset)
               (return frame))
          (setq frame (debug-dnframe frame))
          (go loop))))

(def debug-scanstk
  (lambda (frame fnset)
    (prog nil
     loop (or frame (return nil))
          (and (not (atom (caddr frame)))
               (memq (caaddr frame) fnset)
               (return frame))
          (setq frame (evalframe (cadr frame)))
          (go loop))))

(def debug-getframes
  (lambda nil
    (prog (flist fnew)
          (setq fnew
                (debug-scanstk '(nil)
                               (cons 'debug interrupt-handlers)))
     loop (and (not debug-sysmode)
	       (not (atom (caddr fnew)))
               (eq (caaddr fnew) 'debug)
               (eq (car (evalframe (cadr fnew))) 'apply)
               (memq (caaddr (evalframe (cadr fnew))) interrupt-handlers)
               (setq fnew (evalframe (cadr fnew))))
          (and (not debug-sysmode)
	       (null flist)
               (eq (car fnew) 'apply)
               (memq (caaddr fnew) interrupt-handlers)
               (setq fnew (evalframe (cadr fnew))))
          (and (not debug-sysmode)
	       (eq (car fnew) 'apply)
               (eq (typep (caaddr fnew)) 'symbol)
               (not (eq (caaddr fnew) (concat (caaddr fnew))))
               (setq fnew (evalframe (cadr fnew)))
               (setq fnew (evalframe (cadr fnew)))
               (setq fnew (evalframe (cadr fnew)))
               (setq fnew (evalframe (cadr fnew)))
               (go loop))
          (and (not debug-sysmode)
	       (not (atom (caddr fnew)))
               (memq (caaddr fnew) '(evalhook* evalhook))
               (setq fnew (evalframe (cadr fnew)))
               (go loop))
          (and (not debug-sysmode)
	       (eq (car fnew) 'apply)
               (eq (caaddr fnew) 'eval)
               (cadadr (caddr fnew))
               (or (not (fixp (cadadr (caddr fnew))))
                   (= (cadadr (caddr fnew)) -1))
               (setq fnew (evalframe (cadr fnew)))
               (go loop))
          (and fnew
               (setq flist (cons fnew flist))
               (setq fnew (evalframe (cadr fnew)))
               (go loop))
          (return (nreverse flist)))))

(def debug-nextframe
  (lambda (frame flist sel)
    (prog nil
          (setq flist (cdr (memq frame flist)))
          (and (not (memq 'user sel)) (return (car flist)))
     loop (or flist (return nil))
          (cond
           ((or (atom (caddr (car flist)))
                (not (debug-sysp (caaddr (car flist)))))
            (return (car flist))))
          (setq flist (cdr flist))
          (go loop))))

(def debug-upframe
  (lambda (frame)
    (debug-nextframe frame rframelist nil)))

(def debug-dnframe
  (lambda (frame)
    (debug-nextframe frame framelist nil)))

(def debug-upfn
  (lambda (frame)
    (debug-nextframe frame rframelist '(user))))

(def debug-dnfn
  (lambda (frame)
    (debug-nextframe frame framelist '(user))))

(def debug-showvar
  (lambda (var frame)
    (terpri)
    (princ '|   |)
    (princ var)
    (princ '| = |)
    (fix-print
     ((lambda (val) (cond ((atom val) '?) (t (car val))))
      (errset (fix-eval var (cadddr frame)) nil)))))

(def debug-nedit
  (lambda (frame)
    (prog (val body elem nframe)
          (setq elem (caddr frame))
          (setq val frame)
     scan (setq val (debug-findusrfn val))
          (or val (go nofn))
          (setq body (getd (caaddr val)))
          (cond ((debug-insidep elem body)
                 (princ '=)
                 (fix-print (caaddr val))
                 (edite body
                        (list 'f (cons '== elem) 'tty:)
                        (caaddr val))
                 (return frame))
                ((or (eq elem (caddr val)) (debug-insidep elem (caddr val)))
                 (setq val (debug-dnframe val))
                 (go scan)))
     nofn (setq nframe (debug-dnframe frame))
          (or nframe (go doit))
          (and (debug-insidep elem (caddr nframe))
               (setq frame nframe)
               (go nofn))
     doit (edite (caddr frame)
                 (and (debug-insidep elem (caddr frame))
                      (list 'f (cons '== elem) 'tty:))
                 nil)
          (return frame))))

(def debug-insidep
  (lambda (elem expr)
    (car (errset (edite expr (list 'f (cons '== elem)) nil)))))

(def debug-findusrfn
  (lambda (frame)
    (cond ((null frame) nil)
          ((and (dtpr (caddr frame))
                (symbolp (caaddr frame))
                (dtpr (getd (caaddr frame))))
           frame)
          (t (debug-findusrfn (debug-dnframe frame))))))

(def debug-findexpr
  (lambda (frame)
    (cond ((null frame) nil)
          ((and (eq (car frame) 'eval) (not (atom (caddr frame))))
           frame)
          (t (debug-findexpr (debug-dnframe frame))))))

(def debug-pop
  (lambda nil
    (prog (frame)
	  (setq frame (car framelist))
     l    (cond ((null (setq frame (evalframe (cadr frame))))(reset)))
	  (cond ((and (dtpr (caddr frame))(eq (caaddr frame) 'debug))
		 (freturn (cadr frame) nil)))
	  (go l))))

(def debug-where
  (lambda (frame)
    (prog (lev diff nframe)
          (setq lev (- (length framelist) (length (memq frame rframelist))))
          (setq diff (- (length framelist) lev 1))
          (debug-print1 frame nil)
          (terpri)
          (cond ((zerop diff) (princ '|you are at top of stack.|))
                ((zerop lev) (princ '|you are at bottom of stack.|))
                (t (princ '|you are |)
                   (princ diff)
                   (cond ((= diff 1) (princ '| frame from the top.|))
                         (t (princ '| frames from the top.|)))))
          (terpri)
          (and (or (atom (caddr frame)) (not (eq (car frame) 'eval)))
               (return nil))
          (setq lev 0)
          (setq nframe frame)
     lp   (and (setq nframe (debug-findcall (caaddr nframe) nframe framelist))
               (setq lev (|1+| lev))
               (go lp))
          (princ '|there are |)
          (princ lev)
          (princ '| |)
          (princ (caaddr frame))
          (princ '|'s below.|)
          (terpri))))

(def debug-sysp
  (lambda (x)
    (and (sysp x) (symbolp x) (not (dtpr (getd x))))))

(dv interrupt-handlers (fixit))

(dv handler-labels
    ((fixit error)
     (debug-ubv-handler ubv)
     (debug-udf-handler udf)
     (debug-fac-handler fac)
     (debug-ugt-handler ugt)
     (debug-wta-handler wta)
     (debug-wna-handler wna)
     (debug-iol-handler iol)
     (debug-*rset-handler rst)
     (debug-mer-handler mer)
     (debug-gcd-handler gcd)
     (debug-gcl-handler gcl)
     (debug-gco-handler gco)
     (debug-pdl-handler pdl)))


(or (boundp 'traced-stuff) (setq traced-stuff nil))

(or (boundp 'evalhook-switch) (setq evalhook-switch nil))

(setq hush-debug nil)


;; other functions grabbed from other cmu files to make this file complete
;; unto itself

;- from sysfunc.l
(declare (special system-functions\))
(defun build-sysp nil
  (do ((temp (oblist) (cdr temp))
       (sysfuncs))
      ((null temp)(setq system-functions\ sysfuncs));atom has ^G at end
      (cond ((getd (car temp))
	     (setq sysfuncs (cons (car temp) sysfuncs))))))

(defun sysp (x) ; (cond ((memq x system-functions\)t))
	(memq x '(funcallhook* funcallhook evalhook evalhook* 
			       continue-evaluation)))

(or (boundp 'system-functions\) (build-sysp))

(defun fretry (pdlpnt frame)
  (freturn pdlpnt
	   (cond ((eq (car frame) 'eval) (eval (caddr frame) (cadddr frame)))
		 ((eq (car frame) 'apply)
		  (eval `(apply ',(caaddr frame) ',(cadaddr frame)) 
			(cadddr frame))))))


; - from cmu.l

(def %lineread
  (lambda (chan)
	  (prog (ans)
	   loop (setq ans (cons (read chan 'EOF) ans))
		(cond ((eq (car ans) 'EOF) (return (reverse (cdr ans)))))
	   loop2(cond ((eq 10 (tyipeek chan)) (return (reverse ans)))
		      ((memq (tyipeek chan) '(41 93))
		       (tyi chan)
		       (go loop2))
		      (t (go loop))))))
