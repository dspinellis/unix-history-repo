#
#  Makefile for Sendmail library
#
#	@(#)Makefile.m4	3.6		2/9/83
#
include(../md/config.m4)dnl

ALL=	sendmail.hf libsys.a
SRCS=	sendmail.hf syslog.c
LIBOBJS=syslog.o

GET=	sccs get
CCONFIG=m4CONFIG
CFLAGS=	$O -I../`include' $(CCONFIG) -DEBUG
O=	-O
ASMSED=	../`include'/asm.sed

.c.o:
	cc -S ${CFLAGS} $*.c
	sed -f $(ASMSED) $*.s | as -o $*.o
	rm -f $*.s

all: $(ALL)

libsys.a: $(LIBOBJS)
	ar rv libsys.a $(LIBOBJS)
	ranlib libsys.a

ndir: /tmp
	cd libndir; make
	ar rv libsys.a libndir/*.o
	ranlib libsys.a
	rm -f ../`include'/dir.h
	cp libndir/dir.h ../`include'

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

clean:
	rm -f libsys.a core a.out
	rm -f *.o libndir/*.o
