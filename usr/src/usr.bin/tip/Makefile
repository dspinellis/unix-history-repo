#	Makefile	4.4	82/01/06
#
# makefile for intermachine communications package
#
# Files are:
#	/etc/remote		remote host description file
#	/etc/phones		phone number file, owned by ${OWNER} and
#				  mode 644
#	${ADM}/aculog		ACU accounting file, owned by ${OWNER} and
#				  mode 644 {if ACULOG defined}
# Presently supports:
#	BIZCOMP
#	DEC DF02-AC, DF03-AC
#	DEC DN-11/Able Quadracall
#	VENTEL intelligent modem
#
# Configuration defines:
#	DF02, DF03, DN11	ACU's supported
#	  BIZ1031, BIZ1022, VENTEL
#	ACULOG			turn on tip logging of ACU use
#	PRISTINE		no phone #'s put in ACU log file
#	CONNECT			worthless command
#	DEFBR			default baud rate to make connection at
#	DEFFS			default frame size for FTP buffering of
#				writes on local side
#	BUFSIZ			buffer sizing from stdio, must be fed
#				explicitly to remcap.c if not 1024

ADM=/usr/adm
BIN=/usr/bin
OWNER=uucp
CFLAGS=-O -DVMUNIX -DDEFBR=300 -DDEFFS=BUFSIZ -DDF02 -DDF03 -DACULOG
LDFLAGS=-z
OBJECTS=acu.o acutab.o biz22.o biz31.o cmds.o cmdtab.o cu.o df.o dn11.o hunt.o \
	lock.o log.o remote.o tip.o tipout.o value.o vars.o ventel.o
SOURCES=acu.c acutab.c biz22.c biz31.c cmds.c cmdtab.c cu.c df.c dn11.c hunt.c \
	lock.c log.c remcap.c remote.c tip.c tipout.c uucplock.c value.c  \
	vars.c ventel.c
CXREF=/usr/ucb/ctags -xw
DIST=./dist

tip:	${OBJECTS} remcap.o uucplock.o
	${CC} -o tip ${LDFLAGS} ${OBJECTS} remcap.o uucplock.o
	rm -f cu
	ln tip cu

all:	tip aculog

install: all
	cp tip ${BIN}/tip
	chown ${OWNER} ${BIN}/tip
	chmod 4711 ${BIN}/tip
	cp aculog /etc/aculog
	chown ${OWNER} /etc/aculog
	chmod 4711 /etc/aculog
#	cp /dev/null ${ADM}/aculog
	chown ${OWNER} ${ADM}/aculog
	chmod 600 ${ADM}/aculog
	@echo "create /etc/remote and /etc/phones"

aculog:	aculog.c
	${CC} -o aculog ${LDFLAGS} aculog.c

${OBJECTS}: tip.h

remcap.o: remcap.c
	${CC} -c -DBUFSIZ=1024 ${CFLAGS} remcap.c

tags:	${SOURCES} tip.h
	ctags -w ${SOURCES} tip.h

clean:
	rm -f aculog tip cu *.o

print:
	@pr README TODO makefile
	@size *.o | pr -h SIZES
	@${CXREF} ${SOURCES} tip.h | pr -h XREF
	@ls -l | pr 
	@pr tip.h ${SOURCES}

lint:
	-lint -hbacvx ${CFLAGS} ${SOURCES}

distribution:
	-if test ! -d ${DIST} ; then mkdir ${DIST} ; else rm -f ${DIST}/* ; fi
	cp ${SOURCES} tip.h makefile README TODO ${DIST}
	cp remote-file phones-file tip.1 ${DIST}
	cd ${DIST}; tar cf dist.tar README TODO tip.h ${SOURCES} makefile \
		remote-file phones-file tip.1
