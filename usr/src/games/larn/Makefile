# Configuration options
#  
#  Updated by jgd for csd1 and bruno for csd4.
#  Check this against Omakefile for changes.
#
#  LARNHOME is the directory where the larn data files will be installed.
#  BINDIR is the directory where the larn binary will be installed.

# LARNHOME = /c/noah/public/.lfiles
LARNHOME = /usr/games/lib/larnfiles
# BINDIR = /c/noah/public
BINDIR = /usr/games
CC= cc

# -ltermlib may need to be changed to -ltermcap on some systems
# TERMLIB= -ltermlib
TERMLIB= -ltermcap

# Available compile time options:
#   WIZID=xxx  - this is the userid (or playerid) of the wizard.  Default is
#			zero (superuser), which disables all wizard functions.
#			Players must have this userid (or playerid) in order
#			to become the non-scoring wizard player.  Definition
#			of WIZID to non-zero will enable the special wizard
#			debugging commands.  For root to become wizard, use
#			WIZID= -1.
#   EXTRA      - incorporates code to gather additional performance statistics
#   TIMECHECK  - incorporates code to disable play during working hours (8-5)
#   SYSV       - use system III/V (instead of V7) type ioctl calls
#   BSD        - use BSD specific features (mostly timer and signal stuff)
#   BSD4.1     - use BSD4.1 to avoid some 4.2 dependencies (must be used with
#					BSD above; do not mix with SYSV)
#   HIDEBYLINK - if defined, the program attempts to hide from ps
#   DOCHECKPOINTS - if not defined, checkpoint files are periodically written
#                   by the larn process (no forking) if enabled in the .larnopts
#		    description file.  Checkpointing is handy on an unreliable
#	    	    system, but takes CPU. Inclusion of DOCHECKPOINTS will cause
#		    fork()ing to perform the checkpoints (again if enabled in
#		    the .larnopts file).  This usually avoids pauses in larn
#		    while the checkpointing is being done (on large machines).
#   SAVEINHOME - put save files in users HOME instead of LARNHOME the as default
#	VER        - This is the version of the software, example:  12
#	SUBVER     - This is the revision of the software, example:  1
#	FLUSHNO=#  - Set the input queue excess flushing threshold (default 5)
#	NOVARARGS  - Define for systems that don't have varargs (a default
#		     varargs will be used).
#	MACRORND   - Define to use macro version of rnd() and rund() (fast
#		     and big)
#	UIDSCORE   - Define to use user id's to manage scoreboard.  Leaving
#		     this out will cause player id's from the file ".playerids"
#		     to be used instead.  (.playerids is created upon demand).
#		     Only one entry per id # is allowed in each scoreboard
#		     (winning & non-winning).
#	VT100	  - Compile for using vt100 family of terminals.  Omission of
#		    this define will cause larn to use termcap, but it will be
#		    MUCH slower due to an extra layer of output interpretation.
#		    Also, only VT100 mode allows 2 different standout modes,
#		    inverse video, and bold video.  And only in VT100 mode is
#		    the scrolling region of the terminal used (much nicer than
#		    insert/delete line sequences to simulate it, if VT100 is
#		    omitted).
#	NONAP	  - This causes napms() to return immediately instead of
#		    delaying n milliseconds.  This define may be needed on
#		    some systems if the nap stuff does not work correctly
#		    (possible hang).  nap() is primarilly used to delay for
#		    effect when casting missile type spells.
#	NOLOG	  - Turn off logging.
#

OPTIONS = -DBSD -DVER=12 -DSUBVER=0 -DWIZID=57 -DSAVEINHOME -DNONAP -DNOLOG

# End of configurable make options
########################################################################
#
OBJS= main.o object.o create.o tok.o display.o global.o data.o io.o monster.o\
	store.o diag.o help.o config.o nap.o bill.o scores.o signal.o moreobj.o\
	movem.o regen.o fortune.o savelev.o
SRCS= main.c object.c create.c tok.c display.c global.c data.c io.c monster.c\
	store.c diag.c help.c config.c nap.c bill.c scores.c signal.c moreobj.c\
	movem.c regen.c fortune.c savelev.c

DOTFILES= .larn.help .larnmaze .larnopts .lfortune

CFLAGS= -O $(OPTIONS) -DLARNHOME=\"$(LARNHOME)/\"

all: larn

larn: $(OBJS) ${LIBC}
	$(CC) $(CFLAGS) $(OBJS) -o $@ $(TERMLIB)

install:
	-mkdir ${DESTDIR}/usr/games/lib/larnfiles
	chmod 777 ${DESTDIR}/usr/games/lib/larnfiles
	install -s -o bin -g bin -m 755 larn ${DESTDIR}/usr/games/larn
	install -c -o bin -g bin -m 444 .larn.help ${DESTDIR}/usr/games/lib/larnfiles
	install -c -o bin -g bin -m 444 .larnmaze ${DESTDIR}/usr/games/lib/larnfiles
	install -c -o bin -g bin -m 444 .larnopts ${DESTDIR}/usr/games/lib/larnfiles
	install -c -o bin -g bin -m 444 .lfortune ${DESTDIR}/usr/games/lib/larnfiles

clean: FRC
	rm -f ${OBJS} core larn

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

main.o: main.c header.h /usr/include/pwd.h
object.o: object.c header.h
create.o: create.c header.h
tok.o: tok.c /usr/include/sys/types.h /usr/include/sys/ioctl.h
tok.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h header.h
display.o: display.c header.h
global.o: global.c header.h
data.o: data.c header.h
io.o: io.c header.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
io.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
io.o: /usr/include/varargs.h
monster.o: monster.c header.h
store.o: store.c header.h
diag.o: diag.c /usr/include/sys/types.h /usr/include/sys/times.h
diag.o: /usr/include/sys/stat.h header.h
help.o: help.c header.h
config.o: config.c header.h
nap.o: nap.c /usr/include/signal.h /usr/include/sys/types.h
nap.o: /usr/include/sys/timeb.h
bill.o: bill.c header.h
scores.o: scores.c /usr/include/sys/types.h /usr/include/sys/times.h
scores.o: /usr/include/sys/stat.h header.h
signal.o: signal.c /usr/include/signal.h header.h
moreobj.o: moreobj.c header.h
movem.o: movem.c header.h
regen.o: regen.c header.h
fortune.o: fortune.c /usr/include/sys/types.h /usr/include/sys/stat.h
fortune.o: /usr/include/fcntl.h header.h
savelev.o: savelev.c header.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
