#	Makefile	4.13	85/09/17
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	ansi apl bib courier cpm dsh help hyper icon jove learn mh \
	mkmf patch rcs sccstorcs spms sunrpc tac tools
#
# Special purpose software that requires custom installation
#
OPTDIR=	dipress kermit mmdf news notes rn sumacc tn3270 umodem xns X

all:	${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean:
	-for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} clean); done

FRC:
