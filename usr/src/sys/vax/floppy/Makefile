#	Makefile	4.2	83/07/09
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
OBJS=	${BOOTS} ${STDBOO} ${DEFBOO} ${DEVBOO} ${UTILS}
FROMDEC=dm* db* s[mbr]* *boo.* vmb.exe
#FLOPPY=	/dev/floppy
FLOPPY=	/dev/rrx0b

all:	floppy

floppy:	3tracks ${OBJS}
	cp /dev/null floppy
	arff crf floppy ${OBJS}
	dd if=floppy of=x skip=6
	cat 3tracks x > floppy

install: floppy
	dd if=floppy of=${FLOPPY}

# build a boot floppy from a DEC console floppy
update:	${OBJS}
	-mkdir fromdec
	cd fromdec; arff xf ${FLOPPY}; rm -f ${FROMDEC}; \
		flcopy -t3 -f ${FLOPPY}; arff crf ${FLOPPY} *
	rm -rf fromdec
	arff rf ${FLOPPY} ${OBJS}

clean:
	rm -f ${BOOTS} ${UTILS} floppy
	rm -rf fromdec
