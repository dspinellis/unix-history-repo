#
# Sail - Wooden Ships And Iron Men
#
# Makefile	@(#)Makefile	1.2 83/03/18
#
CC= cc
CFLAGS= -O
DFILES= driver1.c driver2.c driver3.c grap.c
DOBJS= driver1.o driver2.o driver3.o grap.o
PFILES= player1.c player2.c
POBJS= player1.o player2.o
LFILES= sail.log.c
LOBJS= sail.log.o
COMMONFILES= assorted.c boarders.c game.c globals.c\
	main.c parties.c screen.c machdep.c
COMMONOBJS= assorted.o boarders.o game.o globals.o\
	main.o parties.o screen.o machdep.o
OTHERFILES= Makefile externs.h player.h machdep.h sail.m
JUNKFILES= sail driver sail.log sail.doc junk arch tags
PLIBS= -lcurses -ltermlib machdep.o
DLIBS=
LLIBS=
SAIL= SAIL
DRIVER= .driver
SAIL.LOG= SAIL.log
SAIL.DOC= SAIL.doc

..c.o:; ${CC} ${CFLAGS} -c $<

all: sail driver sail.log sail.doc

sail: ${COMMONOBJS} ${POBJS}
	cc -O ${COMMONOBJS} ${POBJS} ${PLIBS} -o sail

driver: ${COMMONOBJS} ${DOBJS}
	cc -O ${COMMONOBJS} ${DOBJS} ${DLIBS} -o driver

${DOBJS} ${POBJS} ${LOBJS} ${COMMONOBJS}: externs.h machdep.h

sail.log: ${LOBJS} globals.o
	cc -O ${LOBJS} globals.o -o sail.log

sail.doc: sail.m
	nroff -man sail.m > sail.doc

arch:	sail.ar
	ar uv sail.ar ${PFILES} ${COMMONFILES} ${OTHERFILES} \
	${DFILES} ${LFILES}

driveprofile: ${COMMONOBJS} ${DOBJS}
	cc -i -p ${COMMONOBJS} ${DOBJS} ${CFLAGS} ${DLIBS} -o DRIVE.pro

profile: ${COMMONOBJS} ${POBJS} ${DOBJS}
	cc -i -p ${COMMONOBJS} ${POBJS} ${CFLAGS} ${PLIBS} -o SAIL.pro
	cc -i -p ${COMMONOBJS} ${DOBJS} ${CFLAGS} ${DLIBS} -o DRIVE.pro

install: ${BIN}/${SAIL} ${BIN}/${DRIVER} ${BIN}/${SAIL.LOG} ${MAN}/${SAIL.DOC}
${BIN}/${SAIL}: sail
	rm -f ${BIN}/${SAIL}
	cp sail ${BIN}/${SAIL}
	chmod 711 ${BIN}/${SAIL}
${BIN}/${DRIVER}: driver
	rm -f ${BIN}/${DRIVER}
	cp driver ${BIN}/${DRIVER}
	chmod 711 ${BIN}/${DRIVER}
${BIN}/${SAIL.LOG}: sail.log
	rm -f ${BIN}/${SAIL.LOG}
	cp sail.log ${BIN}/${SAIL.LOG}
	chmod 710 ${BIN}/${SAIL.LOG}
	strip ${BIN}/${SAIL.LOG}
${MAN}/${SAIL.DOC}: sail.doc
	rm -f ${MAN}/${SAIL.DOC}
	cp sail.doc ${MAN}/${SAIL.DOC}
	chmod 640 ${MAN}/${SAIL.DOC}

clean: 
	rm -f ${DOBJS} ${POBJS} ${LOBJS} ${COMMONOBJS} ${JUNKFILES}

