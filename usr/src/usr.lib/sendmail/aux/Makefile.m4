#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)Makefile.m4	5.3 (Berkeley) 5/2/86
#
#
#  Makefile for assorted programs related (perhaps distantly) to Sendmail.
#
include(../md/config.m4)dnl

ALL=	mconnect
SRCS=	mconnect.c

LIBS=	m4LIBS
DBMLIB=	-ldbm
CONVTIME=../src/convtime.o
DESTDIR=

CHOWN=	-echo chown
CHMOD=	chmod
O=	-O
COPTS=
CCONFIG=-I../`include' m4CONFIG
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

mconnect: mconnect.o
	cc $(COPTS) -o $@ $*.o

mailstats: mailstats.o
	cc $(COPTS) -o $@ $*.o

praliases: praliases.o
	cc $(COPTS) -o $@ $*.o

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

clean:
	rm -f $(ALL) core a.out make.out lint.out
	rm -f *.o ,*

install: all
