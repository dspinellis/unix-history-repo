;;; cmu file package.
;;;
(eval-when (compile eval)
   (load 'jkfmacs)	; need msg macro
   (load 'cmumacs)
   (load 'cmufncs)
   )

(declare (special $cur$ dc-switch piport %indent dc-switch
		  vars body form var init label part incr limit
		  getdeftable $outport$ tlmacros f tmp))

(declare (nlambda msg))

(declare
 (special %changes
          def-comment
          filelst
          found
          getdefchan
          getdefprops
          history
          historylength
          args
          i
          l
          lasthelp
          prop
          special
          special
          tlbuffer
          z))

(dv dc-switch dc-define)

(dv %indent 0)

(dv *digits ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(dv *letters (a b c d e f g h i j k l m n o p q r s t u v w x y z))

(def changes
  (lambda nil
    (changes1)
    (for-each f
              filelst
              (cond
               ((get f 'changes)
                (terpri)
                (princ f)
                (tab 15)
                (princ (get f 'changes)))))
    (cond
     (%changes (terpri) (princ '<no-file>) (tab 15) (princ %changes)))
    nil))

(def changes1
  (lambda nil
    (cond ((null %changes) nil)
          (t
           (prog (found prop)
                 (for-each f
                           filelst
                           (setq found
                                 (cons (set-of fn
                                               (cons (concat f 'fns)
                                                     (eval
                                                      (concat f
                                                              'fns)))
                                               (memq fn %changes))
                                       found))
                           (setq prop (get f 'changes))
                           (for-each fn
                                     (car found)
                                     (setq prop (insert fn prop nil t)))
                           (putprop f prop 'changes))
                 (setq found (apply 'append found))
                 (setq %changes (set-of fn %changes (not (memq fn found)))))))))

(def dc
  (nlambda (args)
    (eval (cons dc-switch args]

(def dc-define
  (nlambda (args)
    (msg "Enter comment followed by <esc>" (N 1))
    (drain piport)
    (eval (cons 'dc-dskin args]

(def dc-help
  (nlambda (args)
    (cond
     ((eval (cons 'helpfilter (cons (car args) (caddr args))))
      (transprint getdefchan)))))

(def dskin
  (nlambda (files)
    (mapc (function
           (lambda (f)
                   (prog nil
                         (setq dc-switch 'dc-dskin)
                         (file f)
                         (load f)
                         (changes1)
                         (putprop f nil 'changes)
			 (setq dc-switch 'dc-define)
)))
          files]

(***
The new version of dskout (7/26/80) tries to keep backup versions  It returns
the setof its arguments that were successfully written  If it can not write
a file (typically because of protection restrictions) it offers to (try to)
write a copy to /tmp  A file written to /tmp is not considered to have been
successfully written (and changes will not consider it to be up-to-date) )

(def dskout
  (nlambda (files)
    (changes1)
    (set-of f
            files
            (prog (ffns p tmp)
                  (cond ((atom (errset (setq p (infile f)) nil))
                         (msg "creating " f N D))
                        (t (close p)
                           (cond ((zerop
                                   (eval
                                    (list 'exec
                                          'mv
                                          f
                                          (setq tmp
                                                (concat f '|.back|)))))
                                  (msg  "old version moved to " 
					tmp N D))
                                 (t (msg 
                                         "Unable to back up "
                                         f
                                         " - continue? (y/n) " D)
                                    (cond ((not (ttyesno)) (return nil)))))))
                  (cond
                   ((atom
                     (errset (apply (function pp)
                                    (cons (list 'F f)
                                          (cons (setq ffns
                                                      (concat f
                                                              'fns))
                                                (eval ffns))))
                             nil))
                    (msg
                         "Unable to write "
                         f
                         " - try to put it on /tmp? (y/n) " D)
                    (cond
                     ((ttyesno)
                      (setq f (explode f))
                      (while (memq '/ f)
                             (setq f (cdr (memq '/ f))))
                      (setq f
                            (apply (function concat)
                                   (cons '/tmp/ f)))
                      (cond ((atom
                              (errset
                               (apply (function pp)
                                      (cons (list 'F f)
                                            (cons ffns (eval ffns))))))
                             (msg
                                  "Unable to create "
                                 f
                                  " - I give up! " N D  ))
                            (t (msg f " written " N D  )))))
                    (return nil)))
                  (putprop f nil 'changes)
                  (return t)))))

(def dskouts
  (lambda nil
    (changes1)
    (apply (function dskout) (set-of f filelst (get f 'changes)))))

(def evl-trace
  (nlambda (exp)
    (prog (val)
          (tab %indent)
          (prinlev (car exp) 2)
          ((lambda (%indent) (setq val (eval (car exp)))) (+ 2 %indent))
          (tab %indent)
          (prinlev val 2)
          (return val))))


(def file
  (lambda (name)
    (setq filelst (insert name filelst nil t))
    (cond
     ((not (boundp (concat name 'fns)))
      (set (concat name 'fns) nil)))
    name))

(def getdef
  (nlambda (%%l)
    (prog (x u getdefchan found)
          (setq getdefchan (infile (car %%l)))
     l    (cond ((atom
                  (setq u
                        (errset
                         (prog (x y z)
                               (cond
                                ((eq (tyipeek getdefchan) -1)
                                 (err 'EOF)))
                               (cond
                                ((memq (tyipeek getdefchan)
                                       '(12 13))
                                 (tyi getdefchan)))
                               (return
                                (cond
                                 ((memq (tyipeek getdefchan)
                                        '(40 91))
                                  (tyi getdefchan)
                                  (cond
                                   ((and (symbolp
                                          (setq y (ratom getdefchan)))
                                         (cond (t (comment - what about
                                                   intern?)
                                                  (setq x y)
                                                  t)
                                               ((neq y
                                                     (setq x
                                                           (intern y)))
                                                t)
                                               (t (remob1 x) nil))
                                         (assoc x getdeftable)
                                         (or (setq z (ratom getdefchan))
                                             t)
                                         (some (cdr %%l)
                                               (function
                                                (lambda (x)
                                                        (matchq x z)))
                                               nil)
                                         (cond ((symbolp z)
                                                (setq y z)
                                                t)
                                               (t (setq y z) t))
                                         (cond ((memq y found))
                                               ((setq found
                                                      (cons y found))))
                                         (not
                                          (cond
                                           ((memq (tyipeek
                                                   getdefchan)
                                                  '(40 91))
                                            (print x)
                                            (terpri)
                                            (princ y)
                                            (tyo 32)
                                            (princ
                                             '" -- bad format")
                                            t))))
                                    (cons x
                                          (cons y
                                                (cond ((memq (tyipeek
                                                              getdefchan)
                                                             '(41
                                                               93))
                                                       (tyi
                                                        getdefchan)
                                                       nil)
                                                      (t (untyi 40
                                                                getdefchan)
                                                         (read
                                                          getdefchan))))))))))))))
                 (close getdefchan)
                 (return found))
                (t (setq x (car u))
                   (*** free u)
                   (setq u nil)
                   (cond
                    ((not (atom x))
                     (apply (cdr (assoc (car x) getdeftable)) (ncons x))))))
          (cond ((not (eq (tyi getdefchan) 10)) (zap getdefchan)))
          (go l))))

(def getdefact
  (lambda (i p exp)
    (prog nil
          (cond ((or (null getdefprops) (memq p getdefprops))
                 (terpri)
                 (print (eval exp))
                 (princ '" ")
                 (prin1 p))
                (t (terpri)
                   (print i)
                   (princ '" ")
                   (prin1 p)
                   (princ '" ")
                   (princ 'bypassed))))))

(dv getdefprops (function value expr fexpr macro))

(dv getdeftable
    ((defprop lambda (x) (getdefact (cadr x) (cadddr x) x))
     (dc lambda
         (x)
         (cond
          ((or (null getdefprops) (memq 'comment getdefprops))
           (eval x))))
     (de lambda (x) (getdefact (cadr x) 'expr x))
     (df lambda (x) (getdefact (cadr x) 'fexpr x))
     (dm lambda (x) (getdefact (cadr x) 'macro x))
     (setq lambda (x) (getdefact (cadr x) 'value x))
     (dv lambda (x) (getdefact (cadr x) 'value x))
     (def lambda (x) (getdefact (cadr x) 'function x))))

(setq filelst nil)	;; initial values
(setq %changes nil)
