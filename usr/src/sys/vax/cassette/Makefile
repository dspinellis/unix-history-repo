#	Makefile	4.2	83/07/07
#
# Make a 750/730 tu58 cassette
# 
BOOTS=	boot boot.750
STDBOO=	defboo.cmd anyboo.cmd restar.cmd
DEFBOO=	defboo.rb defboo.rl defboo.hk defboo.up defboo.ra
DEVBOO=	rbmboo.cmd rbsboo.cmd rlsboo.cmd hkmboo.cmd hksboo.cmd \
	upmboo.cmd upsboo.cmd ramboo.cmd rasboo.cmd
UTILS=	format.750 format copy copy.750 drtest drtest.750
# order is NOT random...
OBJS=	${BOOTS} ${STDBOO} ${DEFBOO} ${DEVBOO} ${UTILS}
# this is ONLY for a 730...
BOOTOBJS=boot ${STDBOO} ${DEFBOO} ${DEVBOO} format copy drtest
FROMDEC=d[lmq]* cs* *boo.* boot.exe vmb.exe
CASSETTE=/dev/uu0

all:	cassette

cassette: ${OBJS}
	cp /dev/null cassette
	cp /dev/null pad
	arff mcrf cassette ${OBJS} pad
	rm -f pad

../mdec/tuboot: ../mdec/tuboot.s
	cd ../mdec; make ${MFLAGS} tuboot

install: cassette ../mdec/tuboot
	dd if=cassette of=${CASSETTE}
	dd if=../mdec/tuboot of=${CASSETTE} bs=512 count=2

# build a 730 boot cassette from a DEC console cassette
update:	${BOOTOBJS}
	-mkdir fromdec
	cd fromdec; arff mxf ${CASSETTE}; rm -f ${FROMDEC}; \
		flcopy -t3; arff mcrf ${CASSETTE} *
	rm -rf fromdec
	arff mrf ${CASSETTE} ${BOOTOBJS}

clean:
	rm -f ${BOOTS} ${UTILS} cassette form50 boot50 copy50
