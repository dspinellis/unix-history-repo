#	Makefile	4.23	87/05/30
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	ansi apl bib courier cpm dsh help hyper icon jove kermit mh \
	mkmf np100 patch rcs sunrpc tac tools
#
# Default programs that are installed
#
STDINST=bib jove mh patch rcs
#
# Special purpose software that requires custom installation,
# or may not be needed by all sites
#
OPTDIR=	B dipress emacs enet mmdf news nntp notes pathalias rn \
	spms sumacc umodem xns X

all:	${STDINST}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${OPTDIR}: FRC
	cd $@; make ${MFLAGS} all

depend:

install:
	-for i in ${STDINST}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

installall:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean:
	-for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} clean); done
	-for i in ${OPTDIR}; do (echo $$i; cd $$i; make ${MFLAGS} clean); done

FRC:
