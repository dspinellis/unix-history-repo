; file which loads in all the object files and dumps them
; $Header: /na/franz/liszt/RCS/scmake.l,v 1.1 83/01/26 12:16:43 jkf Exp $
;				-[Wed Jan 26 12:09:58 1983 by jkf]-
(progn
   (load 'fcninfo.l)	; in /usr/lib/lisp (not normally in lisp)
   (load 'decl.l)
   (load 'array.l)
   (load 'datab.l)
   (load 'expr.l)
   (load 'io.l)
   (load 'funa.l)
   (load 'funb.l)
   (load 'func.l)
   (load 'tlev.l)
   (load 'fixnum.l)
   (load 'util.l)
   (load 'lversion.l)
   (allocate 'list 500)
   (allocate 'fixnum 10)
   (allocate 'symbol 150)
   (sstatus translink on)
   (setq displace-macros t)
   (putprop 'chead nil 'version)  ; so the compiler can compile itself
   (setq ER%tpl 'break-err-handler) ; in case we are using another tpl
   				; this is a temporary measure
   (dumplisp snliszt))
