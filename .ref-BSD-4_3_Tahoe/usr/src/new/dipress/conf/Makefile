#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
DESTBIN = ../bin
DESTLIB = ../lib
DESTMAN = ../man
DESTINCLUDE = ../include
MFLAGS = DESTBIN=../${DESTBIN} DESTLIB=../${DESTLIB}\
	 DESTINCLUDE=../${DESTINCLUDE}
CFLAGS=	-O

# Makefile call makefiles call makefiles...
#
# right now, just make troff metrics
SUBDIR=	troff
#SUBDIR= troff otroff tex generic

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

clean:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} clean); done
