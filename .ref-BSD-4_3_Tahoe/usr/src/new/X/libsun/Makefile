#
#	$Header: Makefile,v 10.4 86/12/17 20:40:35 swick Exp $
#

# Copyright 1985, Massachusetts Institute of Technology
# Copyright (c) 1986 by Sun Microsystems,  Inc.
#
# @(#)Makefile 2.1 86/01/28
#

DEFINES = -DRAW_KBD

INCPATH=-I. -I../include
OPT=-O
CFLAGS= ${OPT} ${INCPATH} ${DEFINES}
#CFLAGS= -g ${INCPATH}
C2 = /lib/c2

SRCS= bitpix.c copy.c cursor.c draw.c fill.c font.c\
	maps.c packet.c put.c text.c textutil.c tile.c util.c\
	events.c initial.c maps.c

OBJS= bitpix.o copy.o cursor.o draw.o fill.o font.o\
	put.o text.o textutil.o tile.o util.o\
	events.o initial.o maps.o

all: libsun.a

site:
	rm -f font.o

font.o: vssite.h ../libvs100/param.h
${OBJS}: Xsun.h ../X/X.h ../X/vsinput.h ../X/Xdev.h

# as this library is private to X, we should do nothing on install.

install: all

lint:
	lint -Csun *.c

libsun.a: $(OBJS)
	ar r libsun.a $?
	@ranlib libsun.a

clean:
	rm -f ERRS *.o libsun.a *~

# This make depend entry will only work with newer C compiler than 4.2
depend:
	cc -M ${INCPATH} ${SRCS} | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it
