(setq rcs-fcninfo- "$Header: fcninfo.l,v 1.2 84/01/29 23:39:00 layer Exp $")

;;
;; fcninfo.l				-[Sun Jan 29 23:38:10 1984 by layer]-
;;
;; This is normally not loaded into a lisp system but is loaded into
;; the compiler.  
;; number of arguments information for C coded functions
;; not included: evalframe evalhook wait exece
;; stopped in sysat.c after *invmod
;
;; the information is stored in such as way as to minimize the
;; amount of space required to store the informaion.


(eval-when (compile eval)
   (setq cdescrip " defined in C-coded kernel"))

(defmacro decl-fcn-info (tag fcns)
   `(let ((fcninfo ',tag))
       ,@(mapcar '(lambda (fcn) `(putprop ',fcn fcninfo 'fcn-info)) fcns)))
(defmacro zero (&rest args)
   `(decl-fcn-info ((0 . 0) ,cdescrip) ,args))	
(defmacro zero-to-one (&rest args)
   `(decl-fcn-info ((0 . 1) ,cdescrip) ,args))
(defmacro zero-to-two (&rest args)
   `(decl-fcn-info ((0 . 2) ,cdescrip) ,args))
(defmacro zero-to-inf (&rest args)
   `(decl-fcn-info (nil ,cdescrip) ,args))
(defmacro one (&rest args)
   `(decl-fcn-info ((1 . 1) ,cdescrip) ,args))
(defmacro one-to-two (&rest args)
   `(decl-fcn-info ((1 . 2) ,cdescrip) ,args))
(defmacro one-to-three (&rest args)
   `(decl-fcn-info ((1 . 3) ,cdescrip) ,args))
(defmacro one-to-inf (&rest args)
   `(decl-fcn-info ((1 . nil) ,cdescrip) ,args))
(defmacro two (&rest args)
   `(decl-fcn-info ((2 . 2) ,cdescrip) ,args))
(defmacro two-to-inf (&rest args)
   `(decl-fcn-info ((1 . nil) ,cdescrip) ,args))
(defmacro three (&rest args)
   `(decl-fcn-info ((3 . 3) ,cdescrip) ,args))
(defmacro three-to-five (&rest args)
   `(decl-fcn-info ((3 . 5) ,cdescrip) ,args))
(defmacro three-to-inf (&rest args)
   `(decl-fcn-info ((3 . nil) ,cdescrip) ,args))
(defmacro four (&rest args)
   `(decl-fcn-info ((4 . 4) ,cdescrip) ,args))
(defmacro five (&rest args)
   `(decl-fcn-info ((5 . 5) ,cdescrip) ,args))


(zero baktrace fork oblist ptime reset resetio zapline)
(zero-to-one arg close drain dumplisp exit
   gensym monitor nwritn random return terpr time-string tyipeek)
(zero-to-two err ratom read readc tyi)
(zero-to-inf + - * / and concat cond
   difference greaterp lessp list or plus product prog quotient setq
   sum times unconcat)
(one  1+ 1- absval add1
   aexplode aexplodec aexploden argv
   arrayp ascii asin acos atom bcdp
   bignum-to-list boundp car cdr chdir cos
   dtpr exp fact fake fix float frexp function get_pname getaccess getaux
   getd getdata getdelta
   getentry getenv getdisc getlength go haulong infile log 
   implode intern maknam maknum makunbound minus minusp
   not ncons null numberp onep plist pntlen portp ptr 
   quote readlist remob *rset sin sizeof stringp sub1 sqrt symbolp
   truename type valuep zerop)
(one-to-two errset flatc outfile patom print status tyo untyi)
(one-to-three fasl load process)
(one-to-inf funcall progv)
(two  allocate alphalessp
   arrayref assq atan bignum-leftshift *catch cons
   Divide eq equal freturn
   get haipart *invmod lsh
   mfunction mod *mod nthelem putaux putd
   putdata putdelta putdisc putlength
   remprop replace rot rplaca rplacd segment set setarg setplist scons
   signal sstatus sticky-bignum-leftshift *throw
   vref vrefi-byte vrefi-word vrefi-long)
   
(two-to-inf  apply def mapc mapcan mapcar mapcon maplist prog2)
(three putprop)
(three-to-five cfasl)
(three-to-inf boole)
(four Emuldiv)
(five marray)

