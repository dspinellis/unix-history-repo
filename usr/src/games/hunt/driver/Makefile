#	%W% (Berkeley) %G%

# Flags are:
#	DEBUG		Don't trust everything in the code
#	INTERNET	Use the Internet domain IPC instead of UNIX domain
#	BROADCAST	Use internet broadcasting code when looking for driver
#	OLDIPC		Use 4.1a internet system calls (must also define
#			INTERNET but not BROADCAST)
#	RANDOM		Include doors which disperse shots randomly
#	REFLECT		Include diagonal walls that reflect shots
#	MONITOR		Include code for watching the game from the sidelines
#	OOZE		Include slime shots
#	FLY		Make people fly when walls regenerate under them
#	START_FLYING	Players enter flying (FLY must also be defined)
#	VOLCANO		Include occasional large slime explosions
#
# NOTE: if you change the domain (INTERNET vs UNIX) then "make newdomain"

PROG=	hunt.driver
CFLAGS+=-I${.CURDIR}/../common_source -DBROADCAST -DRANDOM -DREFLECT \
	-DMONITOR -DINTERNET -DOOZE -DFLY -DVOLCANO
SRCS=	answer.c ctl_transact.c ctl.c driver.c draw.c execute.c expl.c \
	extern.c faketalk.c get_names.c makemaze.c pathname.c shots.c \
	terminal.c
.PATH:	${.CURDIR}/../common_source
MAN6=	huntd.0
BINDIR=	/usr/games/hide
BINOWN=	games

.include <bsd.prog.mk>
