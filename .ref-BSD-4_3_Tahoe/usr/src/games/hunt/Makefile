#
# Copyright (c) 1985 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.4 (Berkeley) 6/27/88
#
#  Hunt
#  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
#  San Francisco, California
#
HDR=		hunt.h
DSRC=		answer.c driver.c draw.c execute.c expl.c makemaze.c \
		shots.c terminal.c extern.c pathname.c
DOBJ=		answer.o driver.o draw.o execute.o expl.o makemaze.o \
		shots.o terminal.o extern.o
PSRC=		hunt.c connect.c playit.c pathname.c
POBJ=		hunt.o connect.o playit.o

#
# Flags are:
#	DEBUG	Don't trust everything in the code
#	INTERNET	Use the Internet domain IPC instead of UNIX domain
#	BROADCAST	Use internet broadcasting code when looking for driver
#	OLDIPC		Use 4.1a internet system calls (must also define
#			INTERNET but not BROADCAST)
#	RANDOM	Include doors which disperse shots randomly
#	REFLECT	Include diagonal walls that reflect shots
#	MONITOR	Include code for watching the game from the sidelines
#	OOZE	Include slime shots
#	FLY	Make people fly when walls regenerate under them
#	START_FLYING	Players enter flying (FLY must also be defined)
#	VOLCANO	Include occasional large slime explosions
#
# NOTE: if you change the domain (INTERNET vs UNIX) then "make newdomain"
#
DEFS=		-I. -DBROADCAST -DRANDOM -DREFLECT -DMONITOR -DINTERNET \
		-DOOZE -DFLY -DVOLCANO
CFLAGS=		-O $(DEFS)
LDFLAGS=
PROFLAGS=
LD=		/bin/ld
.SUFFIXES:	.uu .obj .c,v

.obj.uu:
	uuencode $*.obj < $*.obj > $*.uu

.c,v.c:
	co $*.c

standard:	hunt hunt.driver

#
# For testing
#
debug:	hunt.dbg hunt.driver.dbg

hunt.dbg:	$(POBJ) pathname.dbg.o
	$(CC) $(LDFLAGS) -o hunt.dbg $(POBJ) pathname.dbg.o -lcurses -ltermlib

hunt.driver.dbg: $(DOBJ) pathname.dbg.o
	$(CC) $(PROFLAGS) $(LDFLAGS) -o hunt.driver.dbg $(DOBJ) pathname.dbg.o

#
# Binary distribution to other sites
#
distribution:	hunt.uu hunt.driver.uu README pathname.c Makefile.dist hunt.6
	@ln Makefile.dist makefile
	shar -a README makefile pathname.c hunt.uu hunt.driver.uu hunt.6\
		> distribution
	@rm -f makefile hunt.uu hunt.driver.uu hunt.obj hunt.driver.obj

hunt.driver.obj:	$(DOBJ) pathname.o
	$(LD) -r -x -o hunt.driver.obj $(DOBJ)
	symstrip hunt.driver.obj pathname.o -lcurses -ltermcap

hunt.obj:	$(POBJ) pathname.o
	$(LD) -r -x -o hunt.obj $(POBJ)
	symstrip hunt.obj pathname.o -lcurses -ltermcap

#
# System installation
#
install:	standard
	install -s -o games -g bin -m 700 hunt.driver $(DESTDIR)/usr/games/lib/hunt.driver
	install -s -o games -g bin -m 700 hunt $(DESTDIR)/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f hunt; ln -s dm hunt; chown games.bin hunt)

hunt:	$(POBJ) pathname.o
	$(CC) $(LDFLAGS) -o hunt $(POBJ) pathname.o -lcurses -ltermlib

hunt.driver:	$(DOBJ) pathname.o
	$(CC) $(PROFLAGS) $(LDFLAGS) -o hunt.driver $(DOBJ) pathname.o

#
# Object file dependencies
#
$(POBJ): $(HDR)

$(DOBJ): $(HDR)
	$(CC) $(CFLAGS) $(PROFLAGS) -c $*.c

pathname.dbg.o: pathname.c
	@echo $(CC) $(CFLAGS) -DDEBUG -c pathname.c -o pathname.dbg.o
	@rm -f x.c
	@ln pathname.c x.c
	@$(CC) $(CFLAGS) -DDEBUG -c x.c
	@mv x.o pathname.dbg.o
	@rm -f x.c

#
# Miscellaneous functions
#
lint:	$(DSRC) $(PSRC)
	lint $(DEFS) -DSTANDARD $(DSRC) 2>&1 > driver.lint
	lint $(DEFS) -DSTANDARD $(PSRC) -lcurses 2>&1 > hunt.lint

tags:	$(DSRC) $(PSRC)
	ctags $(DSRC) $(PSRC)

newdomain:
	rm -f hunt.o extern.o driver.o

clean:
	rm -f hunt hunt.driver *.o tags errs

depend:
