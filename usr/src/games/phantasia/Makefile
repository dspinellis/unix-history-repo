# Makefile for Phantasia 3.3.2
#
# To create game:
#
# 1)  Set up a directory where the game and its support files will live.
#        (It is suggested that the source be kept somewhere else.)
#
# 2)  Set up the variables in Makefile CAREFULLY to reflect your
#        particular situation.
#
# 3)  Check out ok_to_play() at the end of main.c if you want to include
#        code to restrict access at certain times.
#
# 4)  'make install' and sit back and watch.
#
# NOTE:  'setup' MUST be executed before trying to run 'phantasia'.
#	 The install procedure does this for you.
#
#
# To convert from 3.3.1 or 3.3.1+ to 3.3.2:
#
# 1)	'make convert'  (3.3.1+: 'make PLUS=-DPHANTPLUS convert')
#
# 2)	Move 'newcharacs' to 'characs'.
#

RM =		-rm
SHAR =		/d2/estes/bin/shar
# Tell 'make' to use /bin/sh for all commands.
SHELL =		/bin/sh

# PLOTDEVICE is used for plotting the map.
# Change as appropriate.
PLOTDEVICE =	> /dev/tty

# Add '-ljobs' to LDFLAGS if 4.1bsd; add -ltermlib if not SVR2.
# Need separate i/d space on small machines.
# May need '-f' on machines without floating point hardware.
LIBS= -lm -lcurses -ltermlib

# DEST is where the program and its support files reside
DEST =		/usr/games/lib/phantasia
DESTR =		/usr/games

# The following are program constants which are implementation dependent.
#
# DEST is passed on.  Note that there is no closing quote here.
# WIZARD is the login of the one who will clean up things.
# UID is the uid of game wizard.
# define OK_TO_PLAY to restrict playing access at certain times.
#	Also see function ok_to_play() in main.c, and tailor to your own needs.
# define ENEMY to include code for checking of a 'hit list' of resricted 
#	accounts.  The list of logins goes in the file 'enemy'.
# SHELL is the default shell for shell escapes.  If SHELL is not defined,
#	shell escapes are not enabled.
# define BSD41 for 4.1bsd
# define BSD42 for 4.2bsd
# define SYS3 for System III, or similar (like Version 7)
# define SYS5 for System V
# If you don't have 'strchr()', put '-Dstrchr=index' below.
# May need '-I/usr/include/sys' for 4.2bsd.
CFLAGS = 	-O \
		-DDEST=\"${DEST} \
		-DDESTR=\"${DESTR} \
		-DWIZARD=\"games\" \
		-DUID=57 \
		-DENEMY \
		-DSHELL=\"/bin/csh\" \
		-DBSD42

SRCS=	main.c fight.c io.c interplayer.c gamesupport.c misc.c phantglobs.c
OBJS=	main.o fight.o io.o interplayer.o gamesupport.o misc.o phantglobs.o
HFILES =	phantdefs.h phantstruct.h macros.h include.h
LIBC=	/lib/libc.a

all:		phantasia setup phant.help

# Make the game.
phantasia:	${OBJS} ${LIBC}
		${CC} ${OBJS} -o $@ ${LIBS}

${OBJS}:	${HFILES}

# Make the initialization program.
setup:		phantglobs.o setup.o monsters.asc ${LIBC}
		${CC} phantglobs.o setup.o -o $@ ${LIBS}

setup.o: ${HFILES}

# Make documentation.
# The flags below on tbl and nroff are to make a line printable version.
phant.help:	phant.nr
		tbl -TX phant.nr | nroff -Ttn300 -man > phant.help

# Install game and documentation.  Initialize.
install: FRC
	-mkdir /usr/games/lib/phantasia
	chmod 700 /usr/games/lib/phantasia
	chown bin /usr/games/lib/phantasia
	install -s -o bin -g bin -m 4711 phantasia ${DESTDIR}/usr/games/phantasia
	install -c -o bin -g bin -m 444 phant.help ${DESTDIR}/usr/games/lib/phantasia/phant.help
	./setup
	chgrp bin /usr/games/lib/phantasia/*
	chown bin /usr/games/lib/phantasia/*

# Make Phantasia map.
# Change the map commands reflect your installation.
# Pre-System III installations may use 'plot' instead of 'tplot'.
map:		map.c
		cc -O map.c -lplot -o map
		map | tplot -t4014 ${PLOTDEVICE}

# Convert old character file to new format.
# 'convert' is meant to be run only once.
# PLUS = PHANTPLUS for conversion from 3.3.1+.
convert:	convert.c oldplayer.h phantstruct.h
		${CC} ${CFLAGS} ${PLUS} convert.c ${LDFLAGS} -o convert
		convert
		@echo "New data is in newcharacs."

# Clean up directory after installation.
clean:
		${RM} -f *.o core phantasia phant.help phant.pt?

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

main.o: main.c include.h /usr/include/ctype.h /usr/include/curses.h
main.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/math.h /usr/include/pwd.h /usr/include/setjmp.h
main.o: /usr/include/signal.h /usr/include/time.h macros.h phantdefs.h
main.o: phantstruct.h phantglobs.h
fight.o: fight.c include.h /usr/include/ctype.h /usr/include/curses.h
fight.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
fight.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
fight.o: /usr/include/math.h /usr/include/pwd.h /usr/include/setjmp.h
fight.o: /usr/include/signal.h /usr/include/time.h macros.h phantdefs.h
fight.o: phantstruct.h phantglobs.h
io.o: io.c include.h /usr/include/ctype.h /usr/include/curses.h
io.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
io.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h /usr/include/math.h
io.o: /usr/include/pwd.h /usr/include/setjmp.h /usr/include/signal.h
io.o: /usr/include/time.h macros.h phantdefs.h phantstruct.h phantglobs.h
interplayer.o: interplayer.c include.h /usr/include/ctype.h
interplayer.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
interplayer.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
interplayer.o: /usr/include/sys/ttydev.h /usr/include/math.h /usr/include/pwd.h
interplayer.o: /usr/include/setjmp.h /usr/include/signal.h /usr/include/time.h
interplayer.o: macros.h phantdefs.h phantstruct.h phantglobs.h
gamesupport.o: gamesupport.c include.h /usr/include/ctype.h
gamesupport.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
gamesupport.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
gamesupport.o: /usr/include/sys/ttydev.h /usr/include/math.h /usr/include/pwd.h
gamesupport.o: /usr/include/setjmp.h /usr/include/signal.h /usr/include/time.h
gamesupport.o: macros.h phantdefs.h phantstruct.h phantglobs.h
misc.o: misc.c include.h /usr/include/ctype.h /usr/include/curses.h
misc.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
misc.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
misc.o: /usr/include/math.h /usr/include/pwd.h /usr/include/setjmp.h
misc.o: /usr/include/signal.h /usr/include/time.h macros.h phantdefs.h
misc.o: phantstruct.h phantglobs.h
phantglobs.o: phantglobs.c include.h /usr/include/ctype.h /usr/include/curses.h
phantglobs.o: /usr/include/stdio.h /usr/include/sgtty.h
phantglobs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
phantglobs.o: /usr/include/sys/ttydev.h /usr/include/math.h /usr/include/pwd.h
phantglobs.o: /usr/include/setjmp.h /usr/include/signal.h /usr/include/time.h
phantglobs.o: macros.h phantdefs.h phantstruct.h phantglobs.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
