#
#  Makefile for assorted programs related (perhaps distantly) to Sendmail.
#
#	Version:
#		@(#)Makefile.m4	3.6		2/18/83
#
include(../md/config.m4)dnl

ALL=	logger   mconnect   syslog   vacation
SRCS=	logger.c mconnect.c syslog.c vacation.c

LIBS=	../lib/libsys.a m4LIBS
DBMLIB=	-ldbm
CONVTIME=../src/convtime.o
DESTDIR=

CHOWN=	-echo chown
CHMOD=	chmod
O=	-O
COPTS=
CCONFIG=-I../`include' -DDBM -DDEBUG -DLOG m4CONFIG
CFLAGS=	$O $(COPTS) $(CCONFIG)
ASMSED=	../`include'/asm.sed
AR=	-ar
ARFLAGS=rvu
LINT=	lint
XREF=	ctags -x
CP=	cp
MV=	mv
INSTALL=install -c -s
M4=	m4
TOUCH=	touch
ABORT=	false

GET=	sccs get
DELTA=	sccs delta
WHAT=	sccs what
PRT=	sccs prt
REL=

ROOT=	root
OBJMODE=755

.c.o:
	cc -S $(CFLAGS) $*.c
	sed -f $(ASMSED) $*.s | as -o $*.o
	rm -f $*.s

all: $(ALL)

logger: logger.o
	cc $(COPTS) -o $@ $*.o $(LIBS)

mconnect: mconnect.o
	cc $(COPTS) -o $@ $*.o

praliases: praliases.o
	cc $(COPTS) -o $@ $*.o

syslog: syslog.o
	cc $(COPTS) -o $@ $*.o

vacation: vacation.o
	cc $(COPTS) $(DBMLIB) -o $@ $*.o $(CONVTIME)

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

clean:
	rm -f $(ALL) core a.out make.out lint.out
	rm -f *.o ,*
