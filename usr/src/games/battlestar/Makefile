#
# Copyright (c) 1983 Regents of the University of California,
# All rights reserved.  Redistribution permitted subject to
# the terms of the Berkeley Software License Agreement.
#

# @(#)Makefile	1.9 %G%

CFLAGS=		-O
OBJS=		battlestar.o com1.o com2.o com3.o com4.o com5.o com6.o com7.o \
		init.o cypher.o getcom.o parse.o room.o save.o fly.o misc.o \
		globals.o dayfile.o nightfile.o dayobjs.o nightobjs.o words.o
CFILES=		battlestar.c com1.c com2.c com3.c com4.c com5.c com6.c com7.c \
		init.c cypher.c getcom.c parse.c room.c save.c fly.c misc.c \
		globals.c dayfile.c nightfile.c dayobjs.c nightobjs.c words.c
LIBS=		-lcurses -ltermlib
LINTFLAGS=	-h
JUNKFILES=	battlestar tags
DIR=		$(DESTDIR)/usr/games
LOGFILE=	$(DIR)/lib/battlestar.log
MAN=		$(DESTDIR)/usr/man/man6/battlestar.6

battlestar: $(OBJS)
	cc -o battlestar $(OBJS) $(LIBS)

$(OBJS): externs.h

lint: $(CFILES) 
	lint $(LINTFLAGS) $(CFILES) $(LIBS) 

install: battlestar $(MAN) $(LOGFILE)
	install -s battlestar $(DIR)/battlestar

logfile $(LOGFILE):
	cp /dev/null $(LOGFILE)
	chmod 666 $(LOGFILE)

man $(MAN):
	install -c battlestar.6 $(MAN)

clean:
	rm -f $(OBJS) $(JUNKFILES)

depend:
