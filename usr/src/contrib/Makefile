#	Makefile	4.9	85/08/14
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	ansi apl bib courier cpm dsh help hyper icon jove kermit learn \
	mh mkmf mmdf news notes patch rcs rn sccstorcs spms sumacc sunrpc \
	tac teft tn3270 tools xns

all:	${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean:
	-for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} clean); done

FRC:
