#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.2 (Berkeley) %G%

# Make a 750/730 tu58 cassette
# 
BOOTS=	boot.730 boot.750
STDBOO=	defboo.cmd anyboo.cmd restar.cmd
DEFBOO=	defboo.rb defboo.rl defboo.hk defboo.up defboo.ra
DEVBOO=	rbmboo.cmd rbsboo.cmd rlsboo.cmd hkmboo.cmd hksboo.cmd \
	upmboo.cmd upsboo.cmd ramboo.cmd rasboo.cmd
UTILS=	format.730 format.750 copy.730 copy.750 drtest.730 drtest.750

# order is NOT random...
OBJS=	boot ${STDBOO} ${DEFBOO} ${DEVBOO} format copy drtest
FROMDEC=d[lmq]* cs* *boo.* boot.exe vmb.exe

CASSETTE=/dev/tu0

all:	cassette

cassette: boot.750 ${STDBOO} ${DEFBOO} ${DEVBOO} format.750
cassette: copy.750 drtest.750
	cp /dev/null cassette
	cp /dev/null pad
	cp boot.750 boot
	cp format.750 format
	cp copy.750 copy
	cp drtest.750 drtest
	arff mcrf cassette ${OBJS} pad
	rm -f pad

../mdec/tuboot: ../mdec/tuboot.s
	cd ../mdec; make ${MFLAGS} tuboot

install: cassette ../mdec/tuboot
	dd if=cassette of=${CASSETTE} bs=2k conv=sync
	dd if=../mdec/tuboot of=${CASSETTE} bs=512 count=2

# build a 730 boot cassette from a DEC console cassette
# DONT TRY THIS ON A 750 W/O MRSP, THE SYSTEM WILL CRASH
update:	boot.730 ${STDBOO} ${DEFBOO} ${DEVBOO} format.730
update:	copy.730 drtest.730
	-mkdir fromdec
	cd fromdec; arff mxf ${CASSETTE}; rm -f ${FROMDEC} ${OBJS}; \
		flcopy -t3 -f ${CASSETTE}; arff mcrf ${CASSETTE} *
	rm -rf fromdec
	cp boot.730 boot
	cp format.730 format
	cp copy.730 copy
	cp drtest.730 drtest
	arff mrf ${CASSETTE} ${OBJS}
	-rm -f defboo.cmd

defboo.cmd:
	@echo You must copy one of ${DEFBOO} to defboo.cmd.
	@echo See installing and operating guide for details.
	@/bin/false	#force exits

clean:
	rm -f ${BOOTS} ${UTILS} cassette form50 boot50 copy50 defboo.cmd
	rm -f boot format copy drtest
