(setq SCCS-step "@(#)step.l	1.2	10/22/80")

(dv stepfns (trim $prin1 evalhook* step print* |7bit| skip st sn))

(def trim
  (lambda (arg depth length)
    (cond ((not (dtpr arg)) arg)
          ((zerop depth) '&)
          ((zerop length) '(<etc>))
          (t
           (cons (trim (car arg) (sub1 depth) length)
                 (trim (cdr arg) depth (sub1 length)))))))

(def $prin1
  (lambda (arg)
    (print (trim arg (cond (prinlevel) (t -1)) (cond (prinlength) (t -1))))))

(def evalhook*
  (lambda (form)
    (cond (evalhook* (and (not (atom form))
                          (not (eq evalhook* t))
                          (memq (car form) evalhook*)
                          (setq evalhook* t))
                     (cond ((eq evalhook* t)
                            (and (= evalhook\# 0) (drain piport))
                            (print*)
                            (cond ((atom form)
                                   (cond ((not
                                           (or (numberp form)
                                               (null form)
                                               (eq form t)))
                                          (princ '" = ")
                                          ((lambda (prinlevel prinlength)
                                                   (setq form
                                                         (evalhook form nil))
                                                   ($prin1 form))
                                           3
                                           5))
                                         (t))
                                   (terpri))
                                  (t
                                   (prog (cmd hookfn)
                                    cmdlp(setq cmd (tyi piport))
                                         (cond ((eq cmd 10)
                                                (cond 
; <<<<< start back on the left <<<<<
     (nil (and (not (atom form))
               (eq (car
                    (getl (car form)
                          '(expr fexpr lexpr subr fsubr lsubr macro)))
                   'macro))
          (setq form (funcall (get (car form) 'macro) form))
          (print*)
          (go cmdlp))
; >>>>> continue on the right >>>>>

                                                      (t
                                                       (setq hookfn
                                                             'evalhook*))))
                                               ((memq cmd '(83 115))
                                                (drain piport)
                                                (setq evalhook
                                                      'skip))
                                               ((memq cmd '(80 112))
                                                (drain piport)
                                                (print form)
                                                (go cmdlp))
                                               ((memq cmd '(71 103))
                                                (drain piport)
                                                (setq evalhook*
                                                      nil
                                                      hookfn
                                                      nil))
                                               ((memq cmd '(67 99))
                                                (drain piport)
                                                (setq hookfn nil))
                                               ((memq cmd '(68 100))
                                                (setq evalhook-switch nil)
                                                (sstatus evalhook nil)
                                                (debug)
                                                (setq evalhook-switch t)
                                                (sstatus evalhook t)
                                                (go cmdlp))
                                               ((memq cmd '(77 109))
                                                (drain piport)
                                                (setq hookfn
                                                      'evalhook*))
                                               ((memq cmd '(66 98))
                                                (drain piport)
                                                (break step)
                                                (print*)
                                                (go cmdlp))
                                               ((memq cmd '(81 113))
                                                (step nil)
                                                (reset))
                                               ((memq cmd
                                                      '(72 104 63))
                                                (ty |/usr/lisp/doc/step.ref|)
                                                (drain piport)
                                                (go cmdlp))
                                               (t (princ
                                                   '"Try one of ?BCDGMPQ or <cr>")
                                                  (drain piport)
                                                  (go cmdlp)))
                                         ((lambda (evalhook\#)
                                                  (setq form
                                                        (evalhook form
                                                                  hookfn)))
                                          (|1+| evalhook\#))
                                         (cond
                                          ((and evalhook*
                                                (not (zerop evalhook\#)))
                                           (print*)
                                           (terpri))))))
                            form)
                           (t (evalhook form 'evalhook*))))
          (t (evalhook form 'evalhook*)))))

(dv evalhook* nil)

(def step
  (nlambda (arg)
    (cond ((or (null arg) (car arg))
           (*rset t)
           (setq evalhook-switch t)
           (setq evalhook\# 0)
           (setq evalhook nil)
           (setq evalhook* (cond ((null arg) nil) ((eq (car arg) t)) (arg)))
           (setq evalhook 'evalhook*)
           (sstatus evalhook t))
          (t (setq evalhook* nil)
             (setq evalhook nil)
             (setq evalhook-switch nil)
             (sstatus evalhook nil)))))

(def print*
  (macro (s)
    '(do ((i 1 (|1+| i)) (indent (* 2 evalhook\#)) (prinlevel 3) (prinlength 5))
         ((> i indent) ($prin1 form))
         (tyo 32))))

(def 7bit
  (macro (s)
    (list '= (list 'boole 1 127 (cadr s)) (caddr s))))

(def skip
  (lambda (x)
    nil))

(def st
  (lambda nil
    (step t)))

(def sn
  (lambda nil
    (step nil)))


