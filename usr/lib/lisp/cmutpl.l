;;; cmu top level.
;;; Eventually this file will be able to be read in along with
;;; the standard franz top level and thus allow the user to select
;;; (possible via the .lisprc) the top level he wants.
;;;
(setq rcs-cmutpl-
   "$Header: /usr/lib/lisp/cmutpl.l,v 1.1 83/01/29 18:34:38 jkf Exp $")

(eval-when (compile eval)
   (or (get 'cmumacs 'version) (load 'cmumacs))
   (or (get 'cmufncs 'version) (load 'cmufncs)))

(declare (special history tlbuffer tlmacros historylength))

(dv historylength 25)

(def matchq
  (lambda (x y)
    (prog (xx yy)
          (return
           (cond
            ((and (atom x) (atom y))
             (cond ((matchq1 (setq xx (explode x)) (setq yy (explode y)))
                    (*** freelist xx)
                    (*** freelist yy)
                    t)
                   (t (*** freelist xx) (*** freelist yy)))))))))

(def matchq1
  (lambda (x y)
    (prog nil
     l1   (cond ((eq x y) (return t))
                ((or (equal y '(@)) (equal x '(@))) (return t))
                ((or (null x) (null y)) (return nil))
                ((eq (car x) (car y))
                 (setq x (cdr x))
                 (setq y (cdr y))
                 (go l1))
                (t (return nil))))))

(def showevents
  (lambda (evs)
    (for-each ev
              evs
              (terpri)
              (princ (car ev))
              (princ '".")
              (tlprint (cadr ev))
              (cond ((cddr ev) (terpri) (tlprint (caddr ev)))))))

(def tleval
  (lambda (exp)
    (prog (val)
          (setq val (eval exp))
          (rplacd (cdar history) (ncons val))
          (return val))))

(def tlgetevent
  (lambda (x)
    (cond ((null x) (car history))
          ((and (fixp x) (plusp x)) (assoc x history))
          ((and (fixp x) (minusp x)) (car (Cnth history (minus x)))))))

(dv tlmacros
    ((ed lambda
         (x)
         (prog (exp)
               (cond ((setq exp (copy (cadr (tlgetevent (cadr x)))))
                      (edite exp nil nil)
                      (return (ncons exp)))
                     (t (princ '"No such event")))))
     (redo lambda
           (x)
           (prog (exp)
                 (cond ((setq exp (tlgetevent (cadr x)))
                        (return (ncons (cadr exp))))
                       (t (princ '"No such event")))))
     (?? lambda
         (x)
         (prog (e1 e2 rest)
               (cond ((null (cdr x)) (showevents (reverse history)))
                     ((null (setq e1 (tlgetevent (cadr x))))
                      (princ '"No such event as ")
                      (princ (cadr x)))
                     ((null (cddr x)) (showevents (ncons e1)))
                     ((null (setq e2 (tlgetevent (caddr x))))
                      (princ '"No such event as ")
                      (princ (caddr x)))
                     (t (setq e1 (memq e1 history))
                        (cond ((setq rest (memq e2 e1))
                               (showevents
                                (cons e2 (reverse (ldiff e1 rest)))))
                              (t
                               (showevents
                                (cons (car e1)
                                      (reverse
                                       (ldiff (memq e2 history) e1))))))))))))

(def tlprint
  (lambda (x)
    (prinlev x 4)))

(def tlquote
  (lambda (x)
    (prog (ans)
     l    (cond ((null x) (return (reverse ans)))
                ((eq (car x) '!)
                 (setq ans (cons (cadr x) ans))
                 (setq x (cddr x)))
                (t (setq ans (cons (kwote (car x)) ans)) (setq x (cdr x))))
          (go l))))

(def tlread
  (lambda nil
    (prog (cmd tmp)
     top  (cond ((not (boundp 'history)) (setq history nil)))
          (cond
           ((null tlbuffer)
            (terpri)
            (princ (add1 (cond (history (caar history)) (t 0))))
            (princ '".")
            (cond
             ((null (setq tlbuffer (lineread)))
              (princ 'Bye)
              (terpri)
              (exit)))))
          (cond ((not (atom (setq cmd (car tlbuffer))))
                 (setq tlbuffer (cdr tlbuffer))
                 (go record))
                ((setq cmd (assoc cmd tlmacros))
                 (setq tmp tlbuffer)
                 (setq tlbuffer nil)
                 (setq cmd (apply (cdr cmd) (ncons tmp)))
                 (cond ((atom cmd) (go top))
                       (t (setq cmd (car cmd)) (go record))))
                ((and (null (cdr tlbuffer))
                      (or (numberp (car tlbuffer))
                          (stringp (car tlbuffer))
                          (hunkp (car tlbuffer))
                          (boundp (car tlbuffer))))
                 (setq cmd (car tlbuffer))
                 (setq tlbuffer nil)
                 (go record))
                ((or (and (dtpr (getd (car tlbuffer)))
                          (memq (car (getd (car tlbuffer)))
                                '(lexpr lambda)))
                     (and (bcdp (getd (car tlbuffer)))
                          (eq (getdisc (getd (car tlbuffer)))
                              'lambda)))
                 (setq cmd (cons (car tlbuffer) (tlquote (cdr tlbuffer))))
                 (setq tlbuffer nil)
                 (go record)))
          (setq cmd tlbuffer)
          (setq tlbuffer nil)
     record
          (setq history
                (cons (list (add1 (cond (history (caar history)) (t 0))) cmd)
                      history))
          (cond
           ((dtpr (cdr (setq tmp (Cnth history historylength))))
            (rplacd tmp nil)))
          (return cmd)))]

(def cmu-top-level
  (lambda nil
    (prog (tlbuffer)
     l    (tlprint (tleval (tlread)))
          (go l)))]

; LWE 1/11/81 The following might make this sucker work after resets:

(setq user-top-level 'cmu-top-level)
(putd 'user-top-level (getd 'cmu-top-level))
(setq top-level 'cmu-top-level)
(putd 'top-level (getd 'cmu-top-level))

(def transprint
  (lambda (prt)
    (prog nil
     l    (cond ((memq (tyipeek prt) '(27 -1)) (return nil))
                (t (tyo (tyi prt)) (go l))))))

(def valueof
  (lambda (x)
    (caddr (tlgetevent x))))

(def zap
  (lambda (prt)
    (prog nil
     l    (cond ((memq (tyi prt) '(10 -1)) (return nil)) (t (go l))))))
(dv dc-switch dc-define)
