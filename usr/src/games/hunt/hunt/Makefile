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
#	@(#)Makefile	5.8 (Berkeley) 7/30/89
#
#  Hunt
#  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
#  San Francisco, California
#

HDR=	hunt.h
DSRC=	answer.c driver.c draw.c execute.c expl.c makemaze.c shots.c \
	terminal.c extern.c pathname.c
DOBJ=	answer.o driver.o draw.o execute.o expl.o makemaze.o shots.o \
	terminal.o extern.o
PSRC=	hunt.c connect.c playit.c pathname.c
POBJ=	hunt.o connect.o playit.o
MAN=	hunt.0

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
DEFS=	-DBROADCAST -DRANDOM -DREFLECT -DMONITOR -DINTERNET \
	-DOOZE -DFLY -DVOLCANO
CFLAGS=	-O ${DEFS}

all: hunt hunt.driver

hunt: ${POBJ} pathname.o
	${CC} -o $@ ${POBJ} pathname.o -lcurses -ltermlib -lcompat

hunt.driver: ${DOBJ} pathname.o
	${CC} -o $@ ${DOBJ} pathname.o

debug: hunt.dbg hunt.driver.dbg

hunt.dbg: ${POBJ} pathname.dbg.o
	${CC} -o $@ ${POBJ} pathname.dbg.o -lcurses -ltermlib

hunt.driver.dbg: ${DOBJ} pathname.dbg.o
	${CC} -o $@ ${DOBJ} pathname.dbg.o

pathname.dbg.o: pathname.c
	@echo ${CC} ${CFLAGS} -DDEBUG -c pathname.c -o pathname.dbg.o
	@rm -f x.c
	@ln pathname.c x.c
	@${CC} ${CFLAGS} -DDEBUG -c x.c
	@mv x.o pathname.dbg.o
	@rm -f x.c

install: ${MAN}
	install -s -o games -g bin -m 700 hunt.driver ${DESTDIR}/usr/games/lib/hunt.driver
	install -s -o games -g bin -m 700 hunt ${DESTDIR}/usr/games/hide
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6
	(cd ${DESTDIR}/usr/games; rm -f hunt; ln -s dm hunt; chown games.bin hunt)

lint: ${DSRC} ${PSRC}
	lint -chapbx ${DEFS} ${DSRC}
	lint -chapbx ${DEFS} ${PSRC} -lcurses

tags: ${DSRC} ${PSRC}
	ctags ${DSRC} ${PSRC}

clean:
	rm -f hunt hunt.driver *.o tags

cleandir: clean
	rm -f ${MAN} tags .depend

depend:
	mkdep ${DSRC} ${PSRC}
