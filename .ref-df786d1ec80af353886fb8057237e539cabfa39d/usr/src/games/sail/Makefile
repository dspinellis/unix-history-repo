#
# Sail - Wooden Ships And Iron Men
#
# Makefile	@(#)Makefile	1.10 83/07/20
#
CC= cc
CTAGS= ctags -w
CFLAGS= -O

DFILES= driver1.c driver2.c driver3.c grap.c boarders.c
DOBJS= driver1.o driver2.o driver3.o grap.o boarders.o
PFILES= player1.c player2.c player3.c player4.c version.c
POBJS= player1.o player2.o player3.o player4.o version.o
COMMONFILES= assorted.c game.c globals.c misc.c parties.c screen.c machdep.c
COMMONOBJS= assorted.o game.o globals.o misc.o parties.o screen.o machdep.o
LFILES= sail.log.c
LOBJS= sail.log.o

HDRS= externs.h player.h driver.h machdep.h
OTHERFILES= Makefile sail.6
CTAGFILES= ${DFILES} ${PFILES} ${LFILES} ${COMMONFILES}
JUNKFILES= sail driver sail.log sail.doc
PLIBS= -lcurses -ltermlib machdep.o
DLIBS=
LLIBS=
SAILMASTER= daemon
SAIL= sail
DRIVER= lib/saildriver
SAIL.LOG= sail.log
SAIL.DOC= sail.doc
LOGFILE= lib/saillog
TARGET= /usr/games

all: sail driver sail.log sail.doc

sail: ${POBJS} ${COMMONOBJS}
	cc -O ${COMMONOBJS} ${POBJS} ${PLIBS} -o sail

driver: ${DOBJS} ${COMMONOBJS}
	cc -O ${COMMONOBJS} ${DOBJS} ${DLIBS} -o driver

sail.log: ${LOBJS} globals.o
	cc -O ${LOBJS} globals.o -o sail.log

sail.doc: sail.6
#	nroff -man sail.6 > sail.doc

lint:
	lint -z $(PFILES) $(COMMONFILES) -lcurses
	lint -z $(DFILES) $(COMMONFILES)

tags: ${CTAGFILES}
	${CTAGS} ${CTAGFILES}

${HDRS} ${CTAGFILES}:
	sccs get $@;

profile: sail.prof

driver.prof: ${COMMONOBJS} ${DOBJS}
	cc -i -p ${COMMONOBJS} ${DOBJS} ${CFLAGS} ${DLIBS} -o driver.prof

profile: sail.prof
	cc -i -p ${COMMONOBJS} ${POBJS} ${CFLAGS} ${PLIBS} -o sail.prof

clean: 
	rm -f ${DOBJS} ${POBJS} ${LOBJS} ${COMMONOBJS} ${JUNKFILES}

install: sail driver sail.log sail.doc ${TARGET}/${LOGFILE}
	install -s -m 4711 -o ${SAILMASTER} sail ${TARGET}/${SAIL}
	install -s -m 4711 -o ${SAILMASTER} driver ${TARGET}/${DRIVER}
	install -s -m 711 sail.log ${TARGET}/${SAIL.LOG}
	install sail.6 /usr/man/man6
#	cp sail.doc ${TARGET}/${SAIL.DOC}

${TARGET}/${LOGFILE}:
	cp /dev/null ${TARGET}/${LOGFILE}
	chown ${SAILMASTER} ${TARGET}/${LOGFILE}
	chmod 644 ${TARGET}/${LOGFILE}
