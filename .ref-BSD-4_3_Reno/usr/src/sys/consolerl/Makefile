#
# Copyright (c) 1985, 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.1 (Berkeley) 6/5/86
# 
BOOTS=	boot
STDBOOS= defboo.com anyboo.com
DEFBOOS= defboo.hk defboo.hp defboo.ra defboo.up
DEVBOOS= hkmboo.com hksboo.com hpmboo.com hpsboo.com \
	ramboo.com rasboo.com upmboo.com upsboo.com
UTILS=	format copy drtest
# order is NOT random...
OBJS=	${BOOTS} ${STDBOOS} ${DEVBOOS} ${UTILS}
DRIVE=	/dev/crl

# build a boot console rl02 from a DEC console rl02 pack
update:	${OBJS}
	arff rmf ${DRIVE} ${OBJS}
	-rm -f defboo.com

defboo.com:
	@echo You must copy one of ${DEFBOOS} to defboo.com.
	@echo See installing and operating guide for details.
	@/bin/false	#force exits

clean:
	rm -f ${BOOTS} ${UTILS} defboo.com
