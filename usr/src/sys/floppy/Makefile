#
# Copyright (c) 1980, 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	Makefile	7.1	86/06/05
#
# Make a 780 rx01 floppy
# 
BOOTS=	boot
STDBOO=	defboo.cmd anyboo.cmd restar.cmd
DEFBOO=	defboo.hk defboo.hp defboo.ra defboo.up
DEVBOO=	hkmboo.cmd hksboo.cmd hpmboo.cmd hpsboo.cmd \
	ramboo.cmd rasboo.cmd upmboo.cmd upsboo.cmd
UTILS=	format copy drtest

# order is NOT random...
OBJS=	${BOOTS} ${STDBOO} ${DEVBOO} ${UTILS}
FROMDEC=du* dm* db* s[mbr]* *boo.* vmb.exe *.hlp cs* ci*

FLOPPY=	/dev/floppy
#FLOPPY= /dev/rrx0b		# for using RX device

all:	floppy

floppy:	3tracks ${OBJS}
	cp 3tracks floppy
	arff crf floppy ${OBJS}

install: floppy
	dd if=floppy of=${FLOPPY}

# build a boot floppy from a DEC console floppy
update:	${OBJS}
	-mkdir fromdec
	cd fromdec; arff xf ${FLOPPY}; rm -f ${FROMDEC} ${OBJS} ${DEFBOO}; \
		flcopy -t3 -f ${FLOPPY}; rm -f floppy; arff crf ${FLOPPY} *
	rm -rf fromdec
	arff rf ${FLOPPY} ${OBJS}
	-rm -f defboo.cmd

defboo.cmd:
	@echo You must copy one of \{${DEFBOO}\} to defboo.cmd.
	@echo See installing and operating guide for details.
	@/bin/false	#force exits

clean:
	rm -f ${BOOTS} ${UTILS} floppy defboo.cmd
	rm -rf fromdec
