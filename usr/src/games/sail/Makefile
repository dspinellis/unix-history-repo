#
# Sail - Wooden Ships And Iron Men
#
# Makefile	@(#)Makefile	2.6 84/12/03
#
CC=	cc
CTAGS=	ctags -w
CFLAGS=	-O

DFILES=		driver1.c driver2.c driver3.c grap.c boarders.c
DOBJS=		driver1.o driver2.o driver3.o grap.o boarders.o
PFILES=		player1.c player2.c player3.c player4.c player5.c \
		player6.c player7.c version.c
POBJS=		player1.o player2.o player3.o player4.o player5.o \
		player6.o player7.o version.o
COMMONFILES=	assorted.c game.c globals.c misc.c parties.c sync.c
COMMONOBJS=	assorted.o game.o globals.o misc.o parties.o sync.o
LFILES=		sail.log.c
LOBJS=		sail.log.o
HDRS=		externs.h player.h driver.h machdep.h
OTHERFILES=	Makefile sail.6
CTAGFILES=	${DFILES} ${PFILES} ${LFILES} ${COMMONFILES}
JUNKFILES=	make.out
PLIBS=		-lcurses -ltermlib
DLIBS=
LLIBS=

SAILMASTER=	daemon
DRIVER=		lib/saildriver
LOGFILE=	lib/saillog
TARGET=		$(DESTDIR)/usr/games

all: sail driver sail.log

sail: ${POBJS} ${COMMONOBJS}
	cc -O ${COMMONOBJS} ${POBJS} ${PLIBS} -o sail

driver: ${DOBJS} ${COMMONOBJS}
	cc -O ${COMMONOBJS} ${DOBJS} ${DLIBS} -o driver

sail.log: ${LOBJS} globals.o
	cc -O ${LOBJS} globals.o -o sail.log

lint:
	lint -z $(PFILES) $(COMMONFILES) -lcurses
	lint -z $(DFILES) $(COMMONFILES)

tags: ${CTAGFILES}
	${CTAGS} ${CTAGFILES}

${HDRS} ${CTAGFILES}:
	sccs get $@;

clean: 
	rm -f ${DOBJS} ${POBJS} ${LOBJS} ${COMMONOBJS} ${JUNKFILES}

install: sail driver sail.log $(TARGET)/$(LOGFILE)
	install -c -s -m 4755 -o ${SAILMASTER} sail ${TARGET}/sail
	install -c -s -m 4755 -o ${SAILMASTER} driver ${TARGET}/${DRIVER}
	install -c -s -m 755 sail.log ${TARGET}/sail.log
	install -c sail.6 $(DESTDIR)/usr/man/man6

logfile $(TARGET)/$(LOGFILE):
	cp /dev/null ${TARGET}/${LOGFILE}
	chown ${SAILMASTER} ${TARGET}/${LOGFILE}
	chmod 644 ${TARGET}/${LOGFILE}
