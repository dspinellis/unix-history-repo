#	%W%	(Berkeley)	%G%
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	src man

all:	src/Makefile ${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

src/Makefile:
	@echo assuming src/localize.sh is correct...
	cd src; make -f Makefile.dst Makefile

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean:
	-for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} clean); done

FRC:
