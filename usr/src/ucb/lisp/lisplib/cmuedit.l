(setq rcs-cmuedit-
   "$Header: /usr/lib/lisp/cmuedit.l,v 1.1 83/01/29 18:33:36 jkf Exp $")

(eval-when (compile load eval) (load 'cmumacs) (load 'cmufncs))

(declare (special c2 c3 tem nopr %changes))

(dv editsfns
    ((declare
      (special |#1|
               |#2|
               |#3|
               $%dotflg
               %lookdpth
               %prevfn%
               atm
               autop
               com
               com0
               coms
               copyflg
               editcomsl
               editracefn
               %%w
               findflag
               l
               l0
               lastail
               lastp1
               lastp2
               lastword
               lcflg
               marklst
               maxlevel
               maxloop
               mess
               noprint
               oldprompt
               readbuf
               %%x
               toflg
               topflg
               undolst
               undolst1
               unfind
               upfindflg
               usermacros
               findarg
               commentflg
               changed))
     |##|
     editfns
     editf
     editv
     editp
     edite
     editl
     editl0
     edval
     editread
     (declare (*expr editracefn))
     editcom
     editcoma
     editcoml
     editmac
     editcoms
     edith
     edit!undo
     undoeditcom
     editsmash
     editnconc
     editdsubst
     edit1f
     edit2f
     edit4e
     editqf
     edit4f
     editfpat
     edit4f1
     editfindp
     editbf
     editbf1
     editnth
     bpnt0
     bpnt
     editri
     editro
     editli
     editlo
     editbi
     editbo
     editdefault
     edup
     edit*l
     edit*
     edor
     errcom
     edrpt
     edloc
     edlocl
     edit:
     editmbd
     editxtr
     editelt
     editcont
     editsw
     editmv
     editto
     editbelow
     editran
     edit!0
     editrepack
     editmakefn
     usermacros
     editracefn
     lastword
     maxlevel
     maxloop
     editcomsl
     autop
     upfindflg))

(declare
 (special |#1|
          |#2|
          |#3|
          $%dotflg
          %lookdpth
          %prevfn%
          atm
          autop
          com
          com0
          coms
          copyflg
          editcomsl
          editracefn
          %%w
          findflag
          l
          l0
          lastail
          lastp1
          lastp2
          lastword
          lcflg
          marklst
          maxlevel
          maxloop
          mess
          noprint
          oldprompt
          readbuf
          %%x
          toflg
          topflg
          undolst
          undolst1
          unfind
          upfindflg
          usermacros
          findarg
          commentflg
          changed))
(declare (special c nopr))	; LWE 1/11/80 Hacks for new compiler.
(def |##|
  (nlambda (coms)
    ((lambda (l undolst1) (editcoms coms)) l nil)))

(def editfns
  (nlambda (x)
    (prog (y)
          (setq y (eval (car x)))
     l1   (cond
           (y (print (car y))
              (eval
               (list 'errset
                     (cons 'editf (cons (car y) (cdr x)))))
              (setq y (cdr y))
              (go l1))))))

(def editf
  (nlambda (x)
    (prog (y fn changed)
          (cond
           ((null x)
            (print '=)
            (prin1 lastword)
            (setq x (ncons lastword))))
          (cond ((symbolp (car x))
                 (setq fn (car x))
                 (cond ((*** setq y (get fn 'trace)) (setq fn (cdr y))))
                 (cond ((setq y (getd fn))
                        (edite y (cdr x) (car x))
                        (cond
                         (changed
                          (*** cond
                               ((eq (car x) fn)
                                (*** move property to front)
                                (remprop (car x) (car y))
                                (putprop (car x) (cadr y) (car y)))
                               ((setq y (cdr (get fn 'funtype)))
                                (*** move the *right* property of the
                                 original word to the front)
                                (setq fn (get (car x) y))
                                (remprop (car x) y)
                                (putprop (car x) fn y)))))
                        (return (setq lastword (car x))))
                       ((and (boundp fn) (dtpr (cdr y))) (go l1))))
                ((dtpr (car x)) (go l1)))
          (print (car x))
          (princ '" not editable")
          (err nil)
     l1   (print '=editv)
          (return (eval (cons 'editv x))))))

(def editv
  (nlambda (x)
    (prog (y)
          (cond
           ((null x)
            (print '=)
            (prin1 lastword)
            (setq x (ncons lastword))))
          (cond ((dtpr (car x)) (edite (eval (car x)) (cdr x) nil) (return t))
                ((and (symbolp (car x))
                      (boundp (car x))
                      (setq y (eval (car x))))
                 (edite y (cdr x) (car x))
                 (return (setq lastword (car x))))
                (t (print (car x)) (princ '" not editable") (err nil))))))

(def editp
  (nlambda (x)
    (cond
     ((null x) (print '=) (prin1 lastword) (setq x (ncons lastword))))
    (cond ((dtpr (car x)) (print '=editv) (eval (cons 'editv x)))
          ((symbolp (car x))
           (edite (plist (car x)) (cdr x) (car x))
           (setq lastword (car x)))
          (t (print (car x)) (princ '" not editable") (err nil)))))

(def edite
  (lambda (expr coms atm)
    (cond ((atom expr) (print expr) (princ '" not editable") (err nil))
          (t (car (last (editl (ncons expr) coms atm nil nil)))))))

(def editl
  (lambda (l coms atm marklst mess)
    (prog (com lastail undolst undolst1 findflag lcflg unfind lastp1 lastp2 readbuf l0 com0 oldprompt upfindflg noprint findarg)
          (makunbound 'findarg)
          (setq upfindflg t)
          (cond ((dtpr (setq l (catch (eval '(editl0)) edit-abort)))
                 (return l))
                (t (err nil))))))

(def editl0
  (lambda nil
    (prog nil
          (cond
           (coms
            (cond ((eq (car coms) 'start)
                   (setq readbuf (append (cdr coms) (list nil)))
                   (setq coms nil)
                   (*** don 't quit if command fails))
                  (t (editcoms (append coms (list 'ok))) (return l)))))
          (cond
           ((or (null coms) (eq (car coms) 'start))
            (print (or mess 'edit))))
          (cond
           ((or (eq (car l)
                    (car
                     (last
                      (car
                       (cond ((setq com
                                    (get 'edit 'lastvalue)))
                             (t '((nil))))))))
                (and atm
                     (eq (car l)
                         (car
                          (last
                           (car
                            (cond ((setq com
                                         (get atm 'edit-save)))
                                  (t '((nil))))))))))
            (setq l (car com))
            (setq marklst (cadr com))
            (setq undolst (caddr com))
            (cond ((car undolst) (setq undolst (cons nil undolst))))
            (setq unfind (cdddr com))))
          (*** setq
               oldprompt
               (cons (sub1 (stkcount 'editl0 (add1 (spdlpt)) 0))
                     (prompt 35)))
     ct   (setq noprint t)
          (setq findflag nil)
     a    (setq undolst1 nil)
          (cond
           ((and autop (null readbuf) (not noprint)) (bpnt (list 0 autop))))
          (setq com (editread))
          (setq l0 l)
          (setq com0 (cond ((atom com) com) (t (car com))))
          (cond
           ((dtpr
             (prog1 (errset (editcom com t))
                    (cond
                     (undolst1 (setq undolst1
                                     (cons com0 (cons l0 undolst1)))
                               (setq undolst (cons undolst1 undolst))))))
            (go a)))
          (setq readbuf nil)
          (cond (coms (err nil)))
          (terpri)
          (cond (com (prin1 com) (princ '"  ?") (terpri)))
          (go ct))))

(def edval
  (lambda (%%x)
    (errset (eval %%x))))

(def editread
  (lambda nil
    (prog (x)
          (cond
           ((null readbuf)
            (prog nil
             l1   (terpri)
                  (princ '|#|)
                  (*** cond
                       ((neq (car oldprompt) 0) (princ (car oldprompt))))
                  (*** prompt 35)
                  (cond
                   ((atom (setq readbuf (errset (lineread))))
                    (terpri)
                    (go l1)))
                  (setq readbuf (car readbuf)))))
          (setq x (car readbuf))
          (setq readbuf (cdr readbuf))
          (return x))))

(declare (*expr editracefn))

(def editcom
  (lambda (c topflg)
    (setq com c)
    (cond (editracefn (editracefn c)))
    (cond (findflag
           (cond ((eq findflag 'bf) (setq findflag nil) (editbf c nil))
                 (t (setq findflag nil) (editqf c))))
          ((numberp c) (setq l (edit1f c l)) (setq noprint nil))
          ((atom c) (editcoma c (null topflg)))
          (t (editcoml c (null topflg))))
    (car l)))

(def editcoma
  (lambda (c copyflg)
    (prog (tem nopr)
          (selectq c
                   (help (setq nopr t)
                         (eval (cons 'help readbuf))
                         (setq readbuf nil)
                         (*** inserted dec 78 by don cohen))
                   (!0 (edit!0))
                   (!nx
                    (setq l
                          ((lambda (l)
                                   (prog (uf)
                                         (setq uf l)
                                    lp   (cond ((or (null (setq l (cdr l)))
                                                    (null (cdr l)))
                                                (err nil))
                                               ((or (null
                                                     (setq tem
                                                           (memq (car l)
                                                                 (cadr
                                                                  l))))
                                                    (null (cdr tem)))
                                                (go lp)))
                                         (edit* 1)
                                         (setq unfind uf)
                                         (return l)))
                           l)))
                   (!undo (edit!undo t t nil))
                   (? (bpnt0 (car l) 64) (setq nopr t))
                   (?? (edith undolst) (setq nopr t))
                   (bk (edit* -1))
                   (delete (setq c '(delete)) (edit: ': nil nil))
                   (mark (setq marklst (cons l marklst)) (setq nopr t))
                   (nex
                    (setq l
                          ((lambda (l) (editbelow '_ 1) (edit* 1) l)
                           l)))
                   ((f bf)
                    (cond ((null topflg) (setq findflag c))
                          (t (setq findarg
                                   (cond ((or readbuf
                                              (not
                                               (boundp 'findarg)))
                                          (editread))
                                         (t findarg)))
                             (selectq c
                                      (f (editqf findarg))
                                      (bf (editbf findarg nil))
                                      (err nil)))))
                   (nil (setq nopr t))
                   (autop nil)
                   (nx (edit* 1))
                   (ok (cond
                        (atm (cond
                              ((and (dtpr undolst) (car undolst))
                               (setq changed t)
                               (*** bound in editf)
                               (mark!changed atm)))
                             (remprop atm 'edit-save)))
                       (putprop 'edit
                                (cons (last l) (cons marklst (cons undolst l)))
                                'lastvalue)
                       (throw l edit-abort)
                       (*** prompt (cdr oldprompt))
                       (*** retfrom 'editl0 l))
                   (p (bpnt0 (car l) 2) (setq nopr t))
                   (pp (bpnt0 (car l) nil) (setq nopr t))
                   (pp* ((lambda (commentflg) (bpnt0 (car l) nil)) t)
                        (setq nopr t))
                   (repack (editrepack))
                   (save (cond
                          (atm (cond
                                ((and (dtpr undolst) (car undolst))
                                 (mark!changed atm)))
                               (putprop 'edit
                                        (putprop atm
                                                 (cons l
                                                       (cons marklst
                                                             (cons undolst
                                                                   unfind)))
                                                 'edit-save)
                                        'lastvalue)))
                         (*** prompt (cdr oldprompt))
                         (*** retfrom 'editl0 l)
                         (throw l edit-abort))
                   (stop (*** prompt (cdr oldprompt))
                         (*** spreval
                              (stksrch 'editl0 (spdlpt) nil)
                              '(err nil))
                         (throw nil edit-abort))
                   (test (setq undolst (cons nil undolst)) (setq nopr t))
                   (tty: (setq com com0)
                         (setq l (editl l nil atm nil 'tty:)))
                   (unblock (cond ((setq tem (memq nil undolst))
                                   (editsmash tem (ncons nil) (cdr tem)))
                                  (t (terpri) (princ '"not blocked")))
                            (setq nopr t))
                   (undo (edit!undo topflg nil (cond (readbuf (editread)))))
                   (up (edup))
                   (/
                    (cond (unfind (setq c l)
                                  (setq l unfind)
                                  (and (cdr c) (setq unfind c)))
                          (t (err nil))))
                   (/p
                    (cond ((and lastp1 (neq lastp1 l)) (setq l lastp1))
                          ((and lastp2 (neq lastp2 l)) (setq l lastp2))
                          (t (err nil))))
                   (^ (and (cdr l) (setq unfind l)) (setq l (last l)))
                   (_
                    (cond (marklst (and (cdr l) (setq unfind l))
                                   (setq l (car marklst)))
                          (t (err nil))))
                   (__
                    (cond (marklst
                           (and (cdr l)
                                (setq unfind l)
                                (setq l (car marklst))
                                (setq marklst (cdr marklst))))
                          (t (err nil))))
                   (tl (top-level) (setq nopr t))
                   (cond ((null (setq tem (editmac c usermacros nil)))
                          (editdefault c)
                          (setq nopr noprint))
                         (t (editcoms (copy (cdr tem))) (setq nopr noprint))))
          (setq noprint nopr))))

(def editcoml
  (lambda (c copyflg)
    (prog (c2 c3 tem nopr)
     lp   (cond ((dtpr (cdr c))
                 (setq c2 (cadr c))
                 (cond ((dtpr (cddr c)) (setq c3 (caddr c)))
                       (t (setq c3 nil))))
                (t (setq c2 (setq c3 nil))))
          (cond ((and lcflg
                      (selectq c2
                               ((to thru through)
                                (cond
                                 ((null (cddr c))
                                  (setq c3 -1)
                                  (setq c2 'thru)))
                                t)
                               nil))
                 (editto (car c) c3 c2)
                 (return nil))
                ((numberp (car c))
                 (edit2f (car c) (cdr c))
                 (setq noprint nil)
                 (return nil))
                ((eq c2 '::)
                 (editcont (car c) (cddr c))
                 (setq noprint nil)
                 (return nil)))
          (selectq (car c)
                   ((a b :) (edit: (car c) nil (cdr c)))
                   (below (editbelow c2 (cond ((cddr c) c3) (t 1))))
                   (bf (editbf c2 c3))
                   (bi
                    (editbi c2
                            (cond ((cddr c) c3) (t c2))
                            (and (cdr c) (car l))))
                   (bind (prog (|#1| |#2| |#3|)
                               (editcoms (cdr c)))
                         (setq nopr noprint))
                   (bk (edit* (minus c2)))
                   (bo (editbo c2 (and (cdr c) (car l))))
                   (change (editran c '((to) (edit: : |#1| |#3|))))
                   (coms (prog nil
                          l1   (cond
                                ((setq c (cdr c))
                                 (editcom (setq com (eval (car c))) nil)
                                 (go l1))))
                         (setq nopr noprint))
                   (comsq (editcoms (cdr c)) (setq nopr noprint))
                   (copy
                    (editran c '((to) (editmv |#1| (car |#3|) (cdr |#3|) t))))
                   (cp (editmv nil (cadr c) (cddr c) t))
                   (delete (editran c '(nil (edit: : |#1| nil))))
                   (e (setq tem (eval c2))
                      (cond ((null (cddr c)) (print tem)))
                      (setq nopr t))
                   (embed (editran c '((in with) (editmbd |#1| |#3|))))
                   (extract (editran c '((from) (editxtr |#3| |#1|))))
                   (f (edit4f c2 c3))
                   (f= (edit4f (cons '== c2) c3))
                   (fs
                    (prog nil
                     l1   (cond
                           ((setq c (cdr c))
                            (editqf (setq com (car c)))
                            (go l1)))))
                   (help (eval c)
                         (setq nopr t)
                         (*** inserted dec 78 by don cohen))
                   (i (setq c
                            (cons (cond ((atom c2) c2) (t (eval c2)))
                                  (mapcar (function
                                           (lambda (x)
                                                   (cond (topflg (print
                                                                  (setq x
                                                                        (eval
                                                                         x)))
                                                                 x)
                                                         (t (eval x)))))
                                          (cddr c))))
                      (setq copyflg nil)
                      (go lp))
                   (if (cond ((and (dtpr (setq tem (edval c2))) (car tem))
                              (cond ((cdr c) (editcoms c3))))
                             ((and (cddr c) (cdddr c)) (editcoms (cadddr c)))
                             (t (err nil)))
                       (setq nopr noprint))
                   (insert
                    (editran c '((before after for) (edit: |#2| |#3| |#1|))))
                   (lc (edloc (cdr c)))
                   (lcl (edlocl (cdr c)))
                   (li (editli c2 (and (cdr c) (car l))))
                   (lo (editlo c2 (and (cdr c) (car l))))
                   ((lp lpq)
                    (edrpt (cdr c) (eq (car c) 'lpq))
                    (setq nopr noprint))
                   (m (cond ((atom c2)
                             (cond ((setq tem (editmac c2 usermacros nil))
                                    (rplacd tem (cddr c)))
                                   (t
                                    (setq usermacros
                                          (cons (cons c2
                                                      (cons nil (cddr c)))
                                                usermacros)))))
                            (t
                             (cond ((setq tem
                                          (editmac (car c2) usermacros t))
                                    (rplaca tem (caddr c))
                                    (rplacd tem (cdddr c)))
                                   (t (nconc editcomsl (ncons (car c2)))
                                      (mark!changed 'editcomsl)
                                      (setq usermacros
                                            (cons (cons (car c2) (cddr c))
                                                  usermacros))))))
                      (mark!changed 'usermacros)
                      (setq nopr t))
                   (makefn
                    (cond ((or (null c2) (null c3) (null (cdddr c)))
                           (err nil))
                          (t
                           (editmakefn c2
                                       c3
                                       (cadddr c)
                                       (cond ((null (cddddr c)) (cadddr c))
                                             (t (car (cddddr c))))))))
                   (mbd (editmbd nil (cdr c)))
                   (move
                    (editran c
                             '((to) (editmv |#1| (car |#3|) (cdr |#3|) nil))))
                   (mv (editmv nil (cadr c) (cddr c) nil))
                   (n (cond ((atom (car l)) (err nil)))
                      (editnconc (car l)
                                 (cond (copyflg (copy (cdr c)))
                                       (t (append (cdr c) nil)))))
                   (nex
                    (setq l
                          ((lambda (l)
                                   (editbelow c2 (cond ((cddr c) c3) (t 1)))
                                   (edit* 1)
                                   l)
                           l)))
                   (nth
                    (cond
                     ((neq (setq tem (editnth (car l) c2)) (car l))
                      (setq l (cons tem l)))))
                   (nx (edit* c2))
                   (orf (edit4f (cons '*any* (cdr c)) 'n))
                   (orr (edor (cdr c)) (setq nopr noprint))
                   (p (cond
                       ((neq lastp1 l) (setq lastp2 lastp1) (setq lastp1 l)))
                      (bpnt (cdr c))
                      (setq nopr t))
                   (r ((lambda (l)
                               (edit4f c2 t)
                               (setq unfind l)
                               (setq c2
                                     (cond ((and (atom c2)
                                                 upfindflg
                                                 (dtpr (car l)))
                                            (caar l))
                                           (t (car l)))))
                       (ncons (car l)))
                      (editdsubst c3 c2 (car l)))
                   (repack (edloc (cdr c)) (editrepack))
                   (replace (editran c '((with by) (edit: : |#1| |#3|))))
                   (ri (editri c2 c3 (and (cdr c) (cddr c) (car l))))
                   (ro (editro c2 (and (cdr c) (car l))))
                   (s (set c2
                           (cond ((null c2) (err nil))
                                 (t ((lambda (l) (edloc (cddr c))) l))))
                      (setq nopr t))
                   (second (edloc (append (cdr c) (cdr c))))
                   (surround (editran c '((with in) (editmbd |#1| |#3|))))
                   (sw (editsw (cadr c) (caddr c)))
                   (third (edloc (append (cdr c) (cdr c) (cdr c))))
                   ((thru to) (editto nil c2 (car c)))
                   (undo (edit!undo topflg nil c2))
                   (xtr (editxtr nil (cdr c)))
                   (_
                    (setq l
                          ((lambda (l)
                                   (prog (uf)
                                         (setq uf l)
                                         (setq c2 (editfpat c2))
                                    lp   (cond ((cond ((and (atom c2)
                                                            (dtpr (car l)))
                                                       (eq c2 (caar l)))
                                                      ((eq (car c2)
                                                           'if)
                                                       (cond ((atom
                                                               (setq tem
                                                                     (edval
                                                                      (cadr
                                                                       c2))))
                                                              nil)
                                                             (t tem)))
                                                      (t
                                                       (edit4e c2
                                                               (cond ((eq (car
                                                                           c2)
                                                                          '@)
                                                                      (caar
                                                                       l))
                                                                     (t
                                                                      (car
                                                                       l))))))
                                                (setq unfind uf)
                                                (return l))
                                               ((setq l (cdr l)) (go lp)))
                                         (err nil)))
                           l)))
                   (cond ((null (setq tem (editmac (car c) usermacros t)))
                          (editdefault c)
                          (setq nopr noprint))
                         ((not (atom (setq c3 (car tem))))
                          (editcoms (subpair c3 (cdr c) (cdr tem)))
                          (setq nopr noprint))
                         (t (editcoms (subst (cdr c) c3 (cdr tem)))
                            (setq nopr noprint))))
          (setq noprint nopr))))

(def editmac
  (lambda (c lst flg)
    (prog (x y)
     lp   (cond ((null lst) (return nil))
                ((eq c (car (setq x (car lst))))
                 (setq y (cdr x))
                 (cond ((cond (flg (car y)) (t (null (car y)))) (return y)))))
          (setq lst (cdr lst))
          (go lp))))

(def editcoms
  (lambda (coms)
    (prog nil
     l1   (cond ((atom coms) (return (car l))))
          (editcom (car coms) nil)
          (setq coms (cdr coms))
          (go l1))))

(def edith
  (lambda (lst)
    (prog nil
          (terpri)
     l1   (cond ((null lst) (return nil))
                ((null (car lst)) (prin1 'block) (go l2))
                ((null (caar lst)) (go l3))
                ((numberp (caar lst))
                 (prin1 (list (caar lst) '--))
                 (go l2)))
          (prin1 (caar lst))
     l2   (princ '" ")
     l3   (setq lst (cdr lst))
          (go l1))))

(def edit!undo
  (lambda (printflg !undoflg undop)
    (prog (lst flg)
          (setq lst undolst)
     lp   (cond ((or (null lst) (null (car lst))) (go out)))
          (cond ((null undop)
                 (selectq (caar lst)
                          ((nil !undo unblock) (go lp1))
                          (undo (cond ((null !undoflg) (go lp1))))
                          nil))
                ((neq undop (caar lst)) (go lp1)))
          (undoeditcom (car lst) printflg)
          (cond ((null !undoflg) (return nil)))
          (setq flg t)
     lp1  (setq lst (cdr lst))
          (go lp)
     out  (cond (flg (return nil))
                ((and lst (cdr lst)) (print 'blocked))
                (t (terpri) (princ '"nothing saved"))))))

(def undoeditcom
  (lambda (x flg)
    (prog (c)
          (cond ((atom x) (err nil))
                ((neq (car (last l)) (car (last (cadr x))))
                 (terpri)
                 (princ '"different expression")
                 (setq com nil)
                 (err nil)))
          (setq c (car x))
          (setq l (cadr x))
          (prog (y z)
                (setq y (cdr x))
           l1   (cond
                 ((setq y (cdr y))
                  (setq z (car y))
                  (cond ((eq (car z) 'r)
                         ((lambda (l)
                                  (editcom (list 'r
                                                 (cadr z)
                                                 (caddr z))
                                           nil))
                          (cadddr z)))
                        (t (editsmash (car z) (cadr z) (cddr z))))
                  (go l1))))
          (editsmash x nil (cons (car x) (cdr x)))
          (and flg
               (setq flg
                     (cond ((not (numberp c)) c) (t (cons c '(--)))))
               (print flg)
               (princ 'undone))
          (return t))))

(def editsmash
  (lambda (old a d)
    (cond ((atom old) (err nil)))
    (setq undolst1 (cons (cons old (cons (car old) (cdr old))) undolst1))
    (rplaca old a)
    (rplacd old d)))

(def editnconc
  (lambda (x y)
    (prog (tem)
          (return
           (cond ((null x) y)
                 ((atom x) (err nil))
                 (t (editsmash (setq tem (last x)) (car tem) y) x))))))

(def editdsubst
  (lambda (x y z)
    (prog nil
     lp   (cond ((atom z) (return nil))
                ((cond ((symbolp y)
                        (or (eq y (car z))
                            (and (stringp (car z)) (eqstr y (car z)))))
                       (t (equal y (car z))))
                 (editsmash z (copy x) (cdr z)))
                (t (editdsubst x y (car z))))
          (cond
           ((and y (eq y (cdr z)))
            (editsmash z (car z) (copy x))
            (return nil)))
          (setq z (cdr z))
          (go lp))))

(def edit1f
  (lambda (c l)
    (cond ((eq c 0) (cond ((null (cdr l)) (err nil)) (t (cdr l))))
          ((atom (car l)) (err nil))
          ((> c 0)
           (cond ((> c (length (car l))) (err nil))
                 (t (cons (car (setq lastail (Cnth (car l) c))) l))))
          ((> (minus c) (length (car l))) (err nil))
          (t
           (cons (car
                  (setq lastail
                        (Cnth (car l) (+ (length (car l)) (add1 c)))))
                 l)))))

(def edit2f
  (lambda (n x)
    (prog (cl)
          (setq cl (car l))
          (cond ((atom cl) (err nil))
                (copyflg (setq x (copy x)))
                (t (setq x (append x nil))))
          (cond ((> n 0)
                 (cond ((> n (length cl)) (err nil))
                       ((null x) (go delete))
                       (t (go replace))))
                ((or (eq n 0) (null x) (> (minus n) (length cl))) (err nil))
                (t (cond ((neq n -1) (setq cl (Cnth cl (minus n)))))
                   (editsmash cl (car x) (cons (car cl) (cdr cl)))
                   (cond
                    ((cdr x)
                     (editsmash cl (car cl) (nconc (cdr x) (cdr cl)))))
                   (return nil)))
     delete
          (cond ((eq n 1)
                 (or (dtpr (cdr cl)) (err nil))
                 (editsmash cl (cadr cl) (cddr cl)))
                (t (setq cl (Cnth cl (sub1 n)))
                   (editsmash cl (car cl) (cddr cl))))
          (return nil)
     replace
          (cond ((neq n 1) (setq cl (Cnth cl n))))
          (editsmash cl (car x) (cdr cl))
          (cond ((cdr x) (editsmash cl (car cl) (nconc (cdr x) (cdr cl))))))))

(def edit4e
  (lambda (pat y)
    (cond ((eq pat y) t)
          ((atom pat)
           (or (eq pat '&)
               (equal pat y)
               (and (stringp y) (stringp pat) (eqstr pat y))))
          ((eq (car pat) '*any*)
           (prog nil
            lp   (cond ((null (setq pat (cdr pat))) (return nil))
                       ((edit4e (car pat) y) (return t)))
                 (go lp)))
          ((and (eq (car pat) '@) (atom y))
           (prog (z)
                 (setq pat (cdr pat))
                 (setq z (explodec y))
            lp   (cond ((eq (car pat) '@)
                        (*** freelist z)
                        (print '=)
                        (prin1 y)
                        (return t))
                       ((null z) (return nil))
                       ((neq (car pat) (car z))
                        (*** freelist z)
                        (return nil)))
                 (setq pat (cdr pat))
                 (setq z (cdr z))
                 (go lp)))
          ((eq (car pat) '--)
           (or (null (setq pat (cdr pat)))
               (prog nil
                lp   (cond ((edit4e pat y) (return t))
                           ((atom y) (return nil)))
                     (setq y (cdr y))
                     (go lp))))
          ((eq (car pat) '==) (eq (cdr pat) y))
          ((atom y) nil)
          ((edit4e (car pat) (car y)) (edit4e (cdr pat) (cdr y))))))

(def editqf
  (lambda (pat)
    (prog (q1)
          (cond ((and (dtpr (car l))
                      (dtpr (setq q1 (cdar l)))
                      (setq q1 (memq pat q1)))
                 (setq l
                       (cons (cond (upfindflg q1)
                                   (t (setq lastail q1) (car q1)))
                             l)))
                (t (edit4f pat 'n))))))

(def edit4f
  (lambda (pat %%x)
    (prog (ll x %%w)
          (setq %%w (ncons nil))
          (setq com pat)
          (setq pat (editfpat pat))
          (setq ll l)
          (cond
           ((eq %%x 'n)
            (setq %%x 1)
            (cond ((atom (car l)) (go lp1))
                  ((and (atom (caar l)) upfindflg)
                   (setq ll (cons (caar l) l))
                   (go lp1))
                  (t (setq ll (cons (caar l) l))))))
          (cond ((and %%x (not (numberp %%x))) (setq %%x 1)))
          (cond
           ((and (edit4e (cond ((and (dtpr pat) (eq (car pat) ':::))
                                (cdr pat))
                               (t pat))
                         (car ll))
                 (or (null %%x) (eq (setq %%x (sub1 %%x)) 0)))
            (return (setq l ll))))
          (setq x (car ll))
     lp   (cond ((edit4f1 pat x maxlevel)
                 (and (cdr l) (setq unfind l))
                 (return
                  (car
                   (setq l
                         (nconc (car %%w)
                                (cond ((eq (cadr %%w) (car ll)) (cdr ll))
                                      (t ll)))))))
                ((null %%x) (err nil)))
     lp1  (setq x (car ll))
          (cond ((null (setq ll (cdr ll))) (err nil))
                ((and (setq x (memq x (car ll))) (dtpr (setq x (cdr x))))
                 (go lp)))
          (go lp1))))

(def editfpat
  (lambda (pat)
    (cond ((dtpr pat)
           (cond ((or (eq (car pat) '==) (eq (car pat) '@)) pat)
                 (t (mapcar (function editfpat) pat))))
          ((eq (nthchar pat -1) '@) (cons '@ (explodec pat)))
          (t pat))))

(def edit4f1
  (lambda (pat x lvl)
    (prog nil
     lp   (cond ((not (> lvl 0))
                 (terpri)
                 (princ '"maxlevel exceeded")
                 (return nil))
                ((atom x) (return nil))
                ((and (dtpr pat)
                      (eq (car pat) ':::)
                      (edit4e (cdr pat) x)
                      (or (null %%x) (eq (setq %%x (sub1 %%x)) 0))))
                ((and (or (atom pat) (neq (car pat) ':::))
                      (edit4e pat (car x))
                      (or (null %%x) (eq (setq %%x (sub1 %%x)) 0)))
                 (cond
                  ((or (null upfindflg) (dtpr (car x)))
                   (setq lastail x)
                   (setq x (car x)))))
                ((and pat
                      (eq pat (cdr x))
                      (or (null %%x) (eq (setq %%x (sub1 %%x)) 0)))
                 (setq x (cdr x)))
                ((and %%x
                      (dtpr (car x))
                      (edit4f1 pat (car x) (sub1 lvl))
                      (eq %%x 0))
                 (setq x (car x)))
                (t (setq x (cdr x)) (setq lvl (sub1 lvl)) (go lp)))
          (cond ((and %%w (neq x (cadr %%w))) (tconc %%w x)))
          (return (or %%w t)))))

(def editfindp
  (lambda (x pat flg)
    (prog (%%x lastail %%w)
          (setq %%x 1)
          (and (null flg) (setq pat (editfpat pat)))
          (return (or (edit4e pat x) (edit4f1 pat x maxlevel))))))

(def editbf
  (lambda (pat n)
    (prog (ll x y %%w)
          (setq ll l)
          (setq %%w (ncons nil))
          (setq com pat)
          (setq pat (editfpat pat))
          (cond ((and (null n) (cdr ll)) (go lp1)))
     lp   (cond
           ((editbf1 pat (car ll) maxlevel y)
            (setq unfind l)
            (return
             (car
              (setq l
                    (nconc (car %%w)
                           (cond ((eq (car ll) (cadr %%w)) (cdr ll))
                                 (t ll))))))))
     lp1  (setq x (car ll))
          (cond ((null (setq ll (cdr ll))) (err nil))
                ((or (setq y (memq x (car ll))) (setq y (tailp x (car ll))))
                 (go lp)))
          (go lp1))))

(def editbf1
  (lambda (pat x lvl tail)
    (prog (y)
     lp   (cond ((not (> lvl 0))
                 (terpri)
                 (princ '"maxlevel exceeded")
                 (return nil))
                ((eq tail x)
                 (return
                  (cond
                   ((edit4e (cond ((and (dtpr pat)
                                        (eq (car pat) ':::))
                                   (cdr pat))
                                  (t pat))
                            x)
                    (tconc %%w x))))))
          (setq y x)
     lp1  (cond
           ((null (or (eq (cdr y) tail) (atom (cdr y))))
            (setq y (cdr y))
            (go lp1)))
          (setq tail y)
          (cond ((and (dtpr (car tail))
                      (editbf1 pat (car tail) (sub1 lvl) nil))
                 (setq tail (car tail)))
                ((and (dtpr pat)
                      (eq (car pat) ':::)
                      (edit4e (cdr pat) tail)))
                ((and (or (atom pat) (neq (car pat) ':::))
                      (edit4e pat (car tail)))
                 (cond
                  ((or (null upfindflg) (dtpr (car tail)))
                   (setq lastail tail)
                   (setq tail (car tail)))))
                ((and pat (eq pat (cdr tail))) (setq x (cdr x)))
                (t (setq lvl (sub1 lvl)) (go lp)))
          (cond ((neq tail (cadr %%w)) (tconc %%w tail)))
          (return %%w))))

(def editnth
  (lambda (x n)
    (cond ((atom x) (err nil))
          ((not (numberp n))
           (or (memq n x) (memq (setq n (editelt n (ncons x))) x) (tailp n x)))
          ((eq n 0) (err nil))
          ((null
            (setq n
                  (cond
                   ((or (not (minusp n))
                        (> (setq n (plus (length x) n 1)) 0))
                    (Cnth x n)))))
           (err nil))
          (t n))))

(def bpnt0
  (lambda (y n)
    (cond ((neq lastp1 l) (setq lastp2 lastp1) (setq lastp1 l)))
    (cond (n (setq $%dotflg (tailp (car l) (cadr l)))
             (setq %prevfn% '" ")
             (printlev y n))
          (t (terpri) (*** sprint y 1) ($prpr y) (terpri)))))

(def bpnt
  (lambda (x)
    (prog (y n)
          (cond ((eq (car x) 0)
                 (setq y (car l))
                 (setq $%dotflg (tailp (car l) (cadr l))))
                (t (setq y (car (editnth (car l) (car x))))))
          (cond ((null (cdr x)) (setq n 2))
                ((not (numberp (setq n (cadr x)))) (err nil))
                ((minusp n) (err nil)))
          (setq %prevfn% '" ")
          (return (printlev y n)))))

(def editri
  (lambda (m n x)
    (prog (a b)
          (setq a (editnth x m))
          (setq b (editnth (car a) n))
          (cond ((or (null a) (null b)) (err nil)))
          (editsmash a (car a) (editnconc (cdr b) (cdr a)))
          (editsmash b (car b) nil))))

(def editro
  (lambda (n x)
    (setq x (editnth x n))
    (cond ((or (null x) (atom (car x))) (err nil)))
    (editsmash (setq n (last (car x))) (car n) (cdr x))
    (editsmash x (car x) nil)))

(def editli
  (lambda (n x)
    (setq x (editnth x n))
    (cond ((null x) (err nil)))
    (editsmash x (cons (car x) (cdr x)) nil)))

(def editlo
  (lambda (n x)
    (setq x (editnth x n))
    (cond ((or (null x) (atom (car x))) (err nil)))
    (editsmash x (caar x) (cdar x))))

(def editbi
  (lambda (m n x)
    (prog (a b)
          (setq b (cdr (setq a (editnth x n))))
          (setq x (editnth x m))
          (cond ((and a (not (> (length a) (length x))))
                 (editsmash a (car a) nil)
                 (editsmash x (cons (car x) (cdr x)) b))
                (t (err nil))))))

(def editbo
  (lambda (n x)
    (setq x (editnth x n))
    (cond ((atom (car x)) (err nil)))
    (editsmash x (caar x) (editnconc (cdar x) (cdr x)))))

(def editdefault
  (lambda (editx)
    (prog nil
          (cond (lcflg
                 (return
                  (cond ((eq lcflg t) (editqf editx))
                        (t (editcom (list lcflg editx) topflg)))))
                ((null topflg) (err nil))
                ((memq editx editcomsl)
                 (cond (readbuf (setq editx (cons editx readbuf))
                                (setq readbuf nil))
                       (t (err nil))))
                (t (err nil)))
          (return (editcom (setq com editx) topflg)))))

(def edup
  (lambda nil
    (prog (c-exp l1 x y)
          (setq c-exp (car l))
     lp   (cond ((null (setq l1 (cdr l))) (err nil))
                ((tailp c-exp (car l1)) (return nil))
                ((not (setq x (memq c-exp (car l1)))) (err nil))
                ((or (eq x lastail) (not (setq y (memq c-exp (cdr x))))))
                ((and (eq c-exp (car lastail)) (tailp lastail y))
                 (setq x lastail))
                (t (terpri)
                   (princ c-exp)
                   (princ '"- location uncertain")))
          (cond ((eq x (car l1)) (setq l l1)) (t (setq l (cons x l1))))
          (return nil))))

(def edit*l
  (lambda (l)
    (edup)
    (length (car l))))

(def edit*
  (lambda (n)
    (car
     (setq l
           ((lambda (com l m)
                    (cond ((not (> m n)) (err nil)))
                    (edit!0)
                    (edit1f (difference n m) l))
            nil
            l
            (edit*l l))))))

(def edor
  (lambda (coms)
    (prog nil
     lp   (cond ((null coms) (err nil))
                ((dtpr
                  (errset
                   (setq l
                         ((lambda (l)
                                  (cond ((atom (car coms))
                                         (editcom (car coms) nil))
                                        (t (editcoms (car coms))))
                                  l)
                          l))))
                 (return (car l))))
          (setq coms (cdr coms))
          (go lp))))

(def errcom
  (lambda (coms)
    (errset (editcoms coms))))

(def edrpt
  (lambda (edrx quiet)
    (prog (edrl edrptcnt)
          (setq edrl l)
          (setq edrptcnt 0)
     lp   (cond ((> edrptcnt maxloop)
                 (terpri)
                 (princ '"maxloop exceeded"))
                ((dtpr (errcom edrx))
                 (setq edrl l)
                 (setq edrptcnt (add1 edrptcnt))
                 (go lp))
                ((null quiet) (print edrptcnt) (princ 'occurrences)))
          (setq l edrl))))

(def edloc
  (lambda (edx)
    (prog (oldl oldf lcflg edl)
          (setq oldl l)
          (setq oldf unfind)
          (setq lcflg t)
          (cond ((atom edx) (editcom edx nil))
                ((and (null (cdr edx)) (atom (car edx)))
                 (editcom (car edx) nil))
                (t (go lp)))
          (setq unfind oldl)
          (return (car l))
     lp   (setq edl l)
          (cond ((dtpr (errcom edx)) (setq unfind oldl) (return (car l))))
          (cond ((equal edl l) (setq l oldl) (setq unfind oldf) (err nil)))
          (go lp))))

(def edlocl
  (lambda (coms)
    (car
     (setq l
           (nconc ((lambda (l unfind) (edloc coms) l) (ncons (car l)) nil)
                  (cdr l))))))

(def edit:
  (lambda (type lc x)
    (prog (toflg l0)
          (setq l0 l)
          (setq x
                (mapcar (function
                         (lambda (x)
                                 (cond ((and (dtpr x)
                                             (eq (car x) '|##|))
                                        ((lambda (l undolst1)
                                                 (copy (editcoms (cdr x))))
                                         l
                                         nil))
                                       (t x))))
                        x))
          (cond
           (lc (cond ((eq (car lc) 'here) (setq lc (cdr lc))))
               (edloc lc)))
          (edup)
          (cond ((eq l0 l) (setq lc nil)))
          (selectq type
                   ((b before) (edit2f -1 x))
                   ((a after)
                    (cond ((cdar l) (edit2f -2 x))
                          (t (editcoml (cons 'n x) copyflg))))
                   ((: for)
                    (cond ((or x (cdar l)) (edit2f 1 x))
                          ((memq (car l) (cadr l))
                           (edup)
                           (edit2f 1 (ncons nil)))
                          (t (editcoms '(0 (nth -2) (2)))))
                    (return (cond ((null lc) l))))
                   (err nil))
          (return nil))))

(def editmbd
  (lambda (lc x)
    (prog (y toflg)
          (cond (lc (edloc lc)))
          (edup)
          (setq y (cond (toflg (caar l)) (t (ncons (caar l)))))
          (edit2f 1
                  (ncons
                   (cond ((or (atom (car x)) (cdr x)) (append x y))
                         (t (lsubst y '* (car x))))))
          (setq l
                (cons (caar l)
                      (cond ((tailp (car l) (cadr l)) (cdr l)) (t l))))
          (return (cond ((null lc) l))))))

(def editxtr
  (lambda (lc x)
    (prog (toflg)
          (cond (lc (edloc lc)))
          ((lambda (l unfind)
                   (edloc x)
                   (setq x
                         (cond ((tailp (car l) (cadr l)) (caar l))
                               (t (car l)))))
           (ncons (cond ((tailp (car l) (cadr l)) (caar l)) (t (car l))))
           nil)
          (edup)
          (edit2f 1 (cond (toflg (append x nil)) (t (ncons x))))
          (and (null toflg)
               (dtpr (caar l))
               (setq l
                     (cons (caar l)
                           (cond ((tailp (car l) (cadr l)) (cdr l)) (t l))))))))

(def editelt
  (lambda (lc l)
    (prog (y)
          (edloc lc)
     lp   (setq y l)
          (cond ((cdr (setq l (cdr l))) (go lp)))
          (return (car y)))))

(def editcont
  (lambda (lc1 %%x)
    (setq l
          ((lambda (l)
                   (prog nil
                         (setq lc1 (editfpat lc1))
                    lp   (cond ((null (edit4f lc1 'n)) (err nil))
                               ((atom (errset (edlocl %%x))) (go lp)))
                    lp1  (cond ((null (setq l (cdr l))) (err nil))
                               ((cond ((atom lc1) (eq lc1 (caar l)))
                                      ((eq (car lc1) '@)
                                       (edit4e lc1 (caar l)))
                                      (t (edit4e lc1 (car l))))
                                (return l)))
                         (go lp1)))
           l))))

(def editsw
  (lambda (m n)
    (prog (y z tem)
          (setq y (editnth (car l) m))
          (setq z (editnth (car l) n))
          (setq tem (car y))
          (editsmash y (car z) (cdr y))
          (editsmash z tem (cdr z)))))

(def editmv
  (lambda (lc op x cp)
    (prog (l0 l1 z toflg)
          (setq l0 l)
          (and lc (edloc lc))
          (cond ((eq op 'here)
                 (cond ((null lc) (edloc x) (setq x nil)))
                 (setq op ':))
                ((eq (car x) 'here)
                 (cond ((null lc) (edloc (cdr x)) (setq x nil))
                       (t (setq x (cdr x))))))
          (edup)
          (setq l1 l)
          (setq z (cond (cp (copy (caar l))) (t (caar l))))
          (setq l l0)
          (and x (edloc x))
          (cond ((eq op 'after) (setq op 'a))
                ((eq op 'before) (setq op 'b)))
          (editcoml (cond (toflg (cons op (append z nil))) (t (list op z)))
                    nil)
          (prog (l)
                (setq l l1)
                (cond ((not cp) (editcoms '(1 delete)))
                      (toflg (editcoml '(bo 1) nil))))
          (return
           (cond ((null lc) (setq unfind l1) l)
                 ((null x) (setq unfind l1) l0)
                 (t (setq unfind l) l0))))))

(def editto
  (lambda (lc1 lc2 flg)
    (setq l
          ((lambda (l)
                   (cond (lc1 (edloc lc1) (edup)))
                   (editbi 1
                           (cond ((and (numberp lc1)
                                       (numberp lc2)
                                       (> lc2 lc1))
                                  (difference (add1 lc2) lc1))
                                 (t lc2))
                           (car l))
                   (cond
                    ((and (eq flg 'to) (cdaar l))
                     (editri 1 -2 (car l))))
                   (editcom 1 nil)
                   l)
           l))
    (setq toflg t)))

(def editbelow
  (lambda (place depth)
    (cond ((minusp (setq depth (eval depth))) (err nil)))
    (prog (n1 n2)
          (setq n1
                (length
                 ((lambda (l lcflg) (editcom place nil) l) l '_)))
          (setq n2 (length l))
          (cond ((< n2 (+ n1 depth)) (err nil)))
          (setq unfind l)
          (setq l (Cnth l (difference (add1 n2) n1 depth))))))

(def editran
  (lambda (c def)
    (setq l
          (or ((lambda (l)
                       (prog (z w)
                             (cond ((null def) (err nil))
                                   ((null (setq z (car def))) (go out)))
                        lp   (cond ((null z) (err nil))
                                   ((null (setq w (memq (car z) c)))
                                    (setq z (cdr z))
                                    (go lp)))
                        out  (setq z
                                   (apply (car (setq def (cadr def)))
                                          (prog (|#1| |#2| |#3|)
                                                (setq |#1| (cdr
                                                            (ldiff c w)))
                                                (setq |#2| (car z))
                                                (setq |#3| (cdr w))
                                                (return
                                                 (mapcar (function
                                                          (lambda (x)
                                                                  (cond ((atom
                                                                          x)
                                                                         (selectq x
                                                                                  (|#1|
                                                                                   |#1|)
                                                                                  (|#2|
                                                                                   |#2|)
                                                                                  (|#3|
                                                                                   |#3|)
                                                                                  x))
                                                                        (t
                                                                         (eval
                                                                          x)))))
                                                         (cdr def))))))
                             (return
                              (cond ((null z) (setq unfind l) nil) (t z)))))
               l)
              l))))

(def edit!0
  (lambda nil
    (cond ((null (cdr l)) (err nil)))
    (prog nil
     lp   (setq l (cdr l))
          (cond ((tailp (car l) (cadr l)) (go lp))))))

(def editrepack
  (lambda nil
    (cond ((dtpr (car l)) (setq l (edit1f 1 l))))
    (edit: ': nil (ncons (readlist (edite (explode (car l)) nil nil))))))

(def editmakefn
  (lambda (ex args n m)
    (editbi n m (car l))
    (edloc n)
    (editbelow '/ 1)
    (mapc (function (lambda (x y) (editdsubst x y (car l)))) args (cdr ex))
    (putprop (car ex) (cons 'lambda (cons args (car l))) 'expr)
    (mark!changed (car ex))
    (edup)
    (edit2f 1 (ncons ex))))

(dv usermacros nil)

(dv editracefn nil)

(dv lastword editsfns)

(dv maxlevel 192)

(dv maxloop 24)

(dv editcomsl
    (: a
       b
       below
       bf
       bi
       bind
       bk
       bo
       change
       coms
       comsq
       copy
       cp
       delete
       e
       embed
       extract
       f
       f=
       fs
       help
       i
       if
       insert
       lc
       lcl
       li
       lo
       lp
       lpq
       m
       makefn
       mbd
       move
       mv
       n
       nex
       nth
       nx
       orf
       orr
       p
       r
       repack
       replace
       ri
       ro
       s
       second
       surround
       sw
       third
       thru
       to
       undo
       xtr
       _))

(dv autop 2)

(dv upfindflg t)
