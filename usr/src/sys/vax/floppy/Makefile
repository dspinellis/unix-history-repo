#	Makefile	4.1	83/07/07
#
# Make a 780 rx01 floppy
# 
BOOTS=	boot
STDBOO=	defboo.cmd anyboo.cmd restar.cmd
DEFBOO=	defboo.hk defboo.hp defboo.ra defboo.up
DEVBOO=	hkmboo.cmd hksboo.cmd hpmboo.cmd hpsboo.cmd \
	ramboo.cmd rasboo.cmd upmboo.cmd upsboo.cmd
UTILS=	format copy drtest
FLOPPY=	rrx0c

all:	floppy

# order is NOT random...
floppy:	${BOOTS} ${STDBOO} ${DEFBOO} ${DEVBOO} ${UTILS}
	cp /dev/null floppy
	arff crf floppy ${BOOTS} ${STDBOO} ${DEFBOO} ${DEVBOO} ${UTILS}

install: floppy
	dd if=floppy of=/dev/${FLOPPY}

clean:
	rm -f ${BOOTS} ${UTILS} floppy
