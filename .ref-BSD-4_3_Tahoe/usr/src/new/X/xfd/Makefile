#
# Makefile for xfd
#
# Copyright (c) 1986 by David C. Martin, UC Berkeley
#

DESTDIR =
CONFDIR = /usr/new
PROGRAM		= xfd
INCLUDES	= -I../include
DEBUG		= 
DEFINE		=
CFILES		= xfd.c
HFILES		=
OFILES		= xfd.o
LIBS		= ../Xlib/libX.a
CFLAGS		= -O $(INCLUDES) $(DEBUG) $(DEFINE)
#CFLAGS		= -O -m68010 $(INCLUDES) $(DEBUG) $(DEFINE)
#CC		= /usr2/users/dcmartin/bin/cc
CO		= /usr/new/co
CI		= /usr/new/ci
MKINC		= /usr2/users/dcmartin/bin/mkinc
EX_DEPEND	= /usr2/users/dcmartin/usr/src/.ex_depend

all		: $(PROGRAM)

install: all
	install xfd ${DESTDIR}${CONFDIR}

$(PROGRAM)	: $(OFILES)
		$(CC) $(CFLAGS) $(OFILES) -o $(PROGRAM) $(LIBS)

.c.o		: $(HFILES) $(CFILES)
		$(CC) -c $(CFLAGS) $<

tags		: $(CFILES)
		ctags $?

$(CFILES)	:
		$(CO) $@

revision	: $(CFILES) $(HFILES)
		$(CI) -u $?

clean		:
		rm -f make.out $(OFILES) tags core

depend		: $(CFILES) $(HFILES)
		ex - Makefile < $(EX_DEPEND) > /dev/null
		$(MKINC) $(CFILES) >> Makefile

### DON'T DELETE THIS LINE -- ``make depend'' LOOKS FOR IT!
