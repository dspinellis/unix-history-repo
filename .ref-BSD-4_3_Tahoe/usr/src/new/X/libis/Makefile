#
# 	$Source: /u1/X/libis/RCS/Makefile,v $
#	$Header: Makefile,v 1.1 86/11/17 14:35:18 swick Rel $
#

# Copyright (c) 1986 by Integrated Solutions, Inc.
#
# Makefile for X Windows (Version 10) driver for Integrated Solutions
#	Optimum V WorkStations.
#

INCPATH=-I. -I../include
OPT=	-O
#OPT=	-O -DDEBUG
CFLAGS=	${OPT} ${INCPATH}

SRCS=	bitpix.c copy.c cursor.c display.c draw.c events.c fill.c font.c  \
	   misc.c put.c text.c tile.c util.c debug.c

OBJS=	bitpix.o copy.o cursor.o display.o draw.o events.o fill.o font.o  \
	   misc.o put.o text.o tile.o util.o debug.o

all:	libis.a

font.o:	 vssite.h
event.o: lk201.h
${OBJS}: Xis.h ../X/X.h ../X/vsinput.h ../X/Xdev.h

# This library is private to X; do nothing on install.
# Well ... go ahead and make the lint library.

install: all lint

lint:
	lint -Cis *.c

libis.a: $(OBJS)
	ar r libis.a $?
	@ranlib libis.a

clean:
	rm -f ERRS *.o libis.a *~ *.ln

# This make depend entry will only work with newer C compiler than 4.2.
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
