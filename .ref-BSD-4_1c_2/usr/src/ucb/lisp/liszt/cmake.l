; file which loads in all the object files and dumps them
; $Header: /na/franz/liszt/RCS/cmake.l,v 1.1 83/01/26 12:13:03 jkf Exp $
;				-[Wed Jan 26 12:06:19 1983 by jkf]-
(progn
   (load 'fcninfo)	; in /usr/lib/lisp (not normally in lisp)
   (load 'decl)
   (load 'array)
   (load 'datab)
   (load 'expr)
   (load 'io)
   (load 'funa)
   (load 'funb)
   (load 'func)
   (load 'tlev)
   (load 'fixnum)
   (load 'util)
   (load 'lversion)
   (allocate 'list 400)
   (allocate 'fixnum 10)
   (allocate 'symbol 50)
   (sstatus translink on)
   (putprop 'chead nil 'version)  ; so the compiler can compile itself
   (setq ER%tpl 'break-err-handler) ; in case we are using another tpl
   				; this is a temporary measure
   (dumplisp nliszt))
