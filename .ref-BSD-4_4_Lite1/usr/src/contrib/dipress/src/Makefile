#
# Copyright (c) 1984, 1985 Xerox Corp.
#
DESTBIN = ../bin
DESTLIB = ../lib
DESTMAN = ../man
DESTINCLUDE = ../include
MFLAGS = DESTBIN=../${DESTBIN} DESTLIB=../${DESTLIB} DESTMAN=../${DESTMAN} \
	DESTINCLUDE=../${DESTINCLUDE}
CFLAGS=	-O

# Makefile call makefiles call makefiles...
#
# Must make library before compiling the programs that call it.
SUBDIR=	lib bin

all:	${SUBDIR}

# need to install library before anything else can be made

lib: FRC
	cd $@; make ${MFLAGS} install

bin: FRC
	cd $@; make ${MFLAGS}

FRC:

install:  
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} install); done

lint:  
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} lint); done

clean:
	rm -f a.out core *.s *.o
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} clean); done
