#	Makefile	4.1	83/07/06
#
# Make a 750/730 tu58 cassette
# 
BOOTS=	boot boot.750
STDBOO=	defboo.cmd anyboo.cmd restar.cmd
DEFBOO=	defboo.rb defboo.rl defboo.hk defboo.up defboo.ra
DEVBOO=	rbmboo.cmd rbsboo.cmd rlsboo.cmd hkmboo.cmd hksboo.cmd \
	upmboo.cmd upsboo.cmd ramboo.cmd rasboo.cmd
UTILS=	format.750 format copy copy.750 drtest drtest.750
CASSETTE=tu0

all:	cassette

# order is NOT random...
cassette: ${BOOTS} ${STDBOO} ${DEFBOO} ${DEVBOO} ${UTILS}
	cp /dev/null cassette
	arff mcrf cassette ${BOOTS} ${STDBOO} ${DEFBOO} ${DEVBOO} ${UTILS}

../mdec/tuboot: ../mdec/tuboot.s
	cd ../mdec; make ${MFLAGS} tuboot

install: cassette ../mdec/tuboot
	dd if=cassette of=/dev/${CASSETTE}
	dd if=../mdec/tuboot of=/dev/${CASSETTE} bs=512 count=2

clean:
	rm -f ${BOOTS} ${UTILS} cassette form50 boot50 copy50
