#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.5 (Berkeley) 4/16/86
#
PRINTER=-Pdp
TROFF=	ditroff ${PRINTER}
EQN=	deqn ${PRINTER}
TBL=	dtbl ${PRINTER}
LPR=	lpr -n ${PRINTER}

SRC=	intro.t gen.t h.t sys.t sys.kern.t sys.tty.t sys.ufs.t sys.uipc.t \
	sys.vm.t vax.t net.t netinet.t netns.t vaxif.t vaxmba.t vaxuba.t stand.t

kchanges: kchanges.out
	${LPR} kchanges.out

kchanges.out: ${SRC}
	${EQN} ${SRC} | ${TBL} | ${TROFF} -t -ms >kchanges.out

clean:
	rm -f kchanges.out *.t.spell errs Errs

spell:	${SRC}
	@for i in ${SRC}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
