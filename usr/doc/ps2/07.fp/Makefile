#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) 4/29/86
#
SRCS = 	manDefs.rno manCh0.rno manCh1.rno\
	manCh2.rno manCh3.rno manCh4.rno manCh5.rno\
	manCh6.rno manCh7.rno refs.rno manApp.rno
PRINTER=Pdp
TBL=dtbl -${PRINTER}
EQN=deqn -${PRINTER}
TROFF=	ditroff -${PRINTER}
MACROS= -me

paper:	paper.${PRINTER}
	lpr -${PRINTER} -n paper.${PRINTER}

paper.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${EQN} | ${TROFF} ${MACROS} -t > paper.${PRINTER}

clean:
	rm -f paper.[PT]* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
