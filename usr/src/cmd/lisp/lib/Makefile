#	Makefile for /usr/lib/lisp

SRCS=  backquote.l auxfns0.l auxfns1.l toplevel.l machacks.l syscall.l\
	ucifnc.l fix.l step.l trace.l jkfmacs.l editor.l ucido.l loop.l

OBJS=  backquote.o auxfns0.o auxfns1.o toplevel.o machacks.o \
	ucifnc.o trace.o jkfmacs.o ucido.o step.o fix.o

.SUFFIXES: .l

.l.o:
	liszt  $* > $*.blat 2>&1

all: ${OBJS}
	date > all

${OBJS}:/usr/lib/lisp/as

/usr/lib/lisp/as:
	cp /bin/as /usr/lib/lisp
	echo WARNING:  THIS MUST BE UCB AS TO WORK

install: all
	cp $(SRCS) $(DESTDIR)/usr/lib/lisp
	cp $(OBJS) $(DESTDIR)/usr/lib/lisp

loop.o: loop.l
	liszt -m loop  > loop.blat 2>&1

machacks.o: machacks.l
	liszt -m machacks  > machacks.blat 2>&1

clean:
	rm -f *.o *.blat all
