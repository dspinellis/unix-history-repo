

#	Makefile for /usr/lib/lisp




SRCS=  backquote.l auxfns0.l auxfns1.l toplevel.l machacks.l loop.l mystify.l  \
	ucifnc.l  fix.l step.l trace.l jkfmacs.l editor.l


.SUFFIXES: .l

.l.o:
	liszt  $* > $*.blat


${SRCS}:
	sccs get $@

sources: ${SRCS}
