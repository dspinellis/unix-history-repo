#
#  Hunt
#  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
#  San Francisco, California
#
SHELL=		/bin/sh
HDR=		hunt.h bsd.h talk_ctl.h
DSRC1=		answer.c driver.c draw.c execute.c expl.c makemaze.c
DSRC2=		shots.c terminal.c extern.c \
		faketalk.c ctl.c ctl_transact.c get_names.c
DSRC=		${DSRC1} ${DSRC2}
DOBJ=		answer.o driver.o draw.o execute.o expl.o makemaze.o \
		shots.o terminal.o extern.o \
		faketalk.o ctl.o ctl_transact.o get_names.o
PSRC=		hunt.c connect.c playit.c pathname.c otto.c
POBJ=		hunt.o connect.o playit.o otto.o

#
# Installation destinations
BINDIR=		/usr/games
LIBDIR=		/usr/libexec
MANDIR=		/usr/contrib/man/cat6

#
# Game parameter flags are:
#	RANDOM	Include doors which disperse shots randomly
#	REFLECT	Include diagonal walls that reflect shots
#	MONITOR	Include code for watching the game from the sidelines
#	OOZE	Include slime shots
#	FLY	Make people fly when walls regenerate under them
#	VOLCANO	Include occasional large slime explosions
#	DRONE	Include occasional drone shots
#	BOOTS	Include boots (which makes you immune to slime)
#	OTTO	Reserved for CGL automatic player
#
GAME_PARAM=	-DRANDOM -DREFLECT -DMONITOR -DOOZE -DFLY -DVOLCANO -DBOOTS \
		-DOTTO

#
# System parameter flags are:
#	DEBUG		Don't trust everything in the code
#	INTERNET	Use the Internet domain IPC instead of UNIX domain
#	LOG		Use syslog error-logging in driver (needs SYSLOG_42 or
#			or SYSLOG_43)
#	OLDIPC		Use 4.1a internet system calls (must also define
#			INTERNET)
#	TERMINFO	Use terminfo instead of termcap
#	SYSLOG_42	Use 4.2 BSD syslog(3)
#	SYSLOG_43	Use 4.2 BSD syslog(3)
#	LOG		Use syslog(3) for logging errors
#	BSD_RELEASE	Which version of BSD distribution
#			42 is 4.2BSD (implies SYSLOG_42)
#			43 is 4.3BSD (implies BROADCAST, SYSLOG_43)
#	USE_CURSES	Use curses for everything (automatic w/TERMINFO)
#	SIGNAL_TYPE	void or int
#	HPUX		do special HPUX only hacks
#
# NOTE: if you change the domain (INTERNET vs UNIX) then "make newdomain"
#
DEFS_BSD43=	-DINTERNET -DLOG -DBSD_RELEASE=43 -DSIGNAL_TYPE=int
DEFS_BSD44=	-DINTERNET -DLOG -DBSD_RELEASE=44 -DSIGNAL_TYPE=void
DEFS_SUN=	-DINTERNET -DLOG -DBSD_RELEASE=42 -DBROADCAST -DSIGNAL_TYPE=int
DEFS_SUN4_0=	-DINTERNET -DLOG -DSYSLOG_43 -DBROADCAST -DSIGNAL_TYPE=void
DEFS_ULTRIX=	-DINTERNET -DLOG -DBSD_RELEASE=42 -DSIGNAL_TYPE=void
DEFS_HPUX=	-DHPUX -DINTERNET -DLOG -DSYSLOG_43 -DBROADCAST -DTERMINFO -DSIGNAL_TYPE=void
DEFS_SGI=	-DINTERNET -DLOG -DBSD_RELEASE=43 -DTERMINFO -DSIGNAL_TYPE=void
DEFS_NEXT=	$(DEFS_43) -bsd -traditional -Dconst= -DSIGNAL_TYPE=int
DEFS_OSF1=	-DINTERNET -DLOG -DBSD_RELEASE=43 -DSIGNAL_TYPE=void -D_BSD

#
# The following flags are used for system specific compilation arguments.
# Change them to include the appropriate arguments.  For example, on SGI
# machines, they should look like
# SYSCFLAGS=	-I/usr/include/bsd
# SYSLIBS=	-lbsd
#
SYSCFLAGS=
SYSLIBS=

#
# Generic definitions
#
DEFS=		$(GAME_PARAM) $(DEFS_BSD44)
CFLAGS=		-O2 $(SYSCFLAGS) $(DEFS)

#
# Normal targets
#
all:	hunt huntd

hunt:	$(POBJ) pathname.o
	$(CC) -o hunt $(POBJ) pathname.o -lcurses -ltermcap $(SYSLIBS)

huntd:	$(DOBJ) pathname.o
	$(CC) -o huntd $(DOBJ) pathname.o $(SYSLIBS)

debug:	hunt.dbg huntd.dbg

hunt.dbg:	$(POBJ) pathname.dbg.o
	$(CC) -o hunt.dbg $(POBJ) pathname.dbg.o -lcurses -ltermcap $(SYSLIBS)

huntd.dbg: $(DOBJ) pathname.dbg.o
	$(CC) -o huntd.dbg $(DOBJ) pathname.dbg.o $(SYSLIBS)

#
# Source distribution in three files to a particular person
#
mail.msg:
	-@if test x${MAIL} = x ; then\
		/bin/echo MAIL not set ;\
	fi
mail.quit:
	@test x$mail != x
mail:	mail.msg mail.quit
	shar -a README hunt.6 huntd.6 Makefile ${HDR} ${PSRC} |\
	Mail -s "Hunt (part 1 of 3)" ${MAIL}
	shar -a ${DSRC1} | Mail -s "Hunt (part 2 of 3)" ${MAIL}
	shar -a ${DSRC2} | Mail -s "Hunt (part 3 of 3)" ${MAIL}

mail.part2:	mail.msg mail.quit
	shar -a ${DSRC1} | Mail -s "Hunt (part 2 of 3)" ${MAIL}

#
# Single file source distribution
#
shar:
	shar README hunt.6 huntd.6 Makefile ${HDR} ${PSRC} ${DSRC1} \
	${DSRC2} | compress -c > hunt.shar.Z

#
# System installation
#
install:
	install -s -o bin -g bin -m 555 huntd ${LIBDIR}/huntd
	install -s -o bin -g bin -m 555 hunt ${BINDIR}/hunt
	nroff -man hunt.6 > ${MANDIR}/hunt.0
	nroff -man huntd.6 > ${MANDIR}/huntd.0
	chown bin.bin ${MANDIR}/hunt.0 ${MANDIR}/huntd.0
	chmod 444 ${MANDIR}/hunt.0 ${MANDIR}/huntd.0

#
# Object file dependencies
#
$(POBJ): $(HDR)

$(DOBJ): $(HDR)
	$(CC) $(CFLAGS) -c $*.c

pathname.o: pathname.c Makefile
	$(CC) $(CFLAGS) -DHUNTD=\"${LIBDIR}/huntd\" -c pathname.c

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
lint:	driver.lint hunt.lint

driver.lint: $(DSRC)
	lint $(DEFS) $(DSRC) 2>&1 > driver.lint

hunt.lint: $(PSRC)
	lint $(DEFS) $(PSRC) -lcurses 2>&1 > hunt.lint

tags:	$(DSRC) $(PSRC)
	ctags $(DSRC) $(PSRC)

clean:
	rm -f $(POBJ) $(DOBJ) pathname.o pathname.dbg.o errs hunt.dbg \
	huntd.dbg hunt huntd hunt.lint driver.lint hunt.shar.Z

newdomain:
	rm hunt.o extern.o driver.o
