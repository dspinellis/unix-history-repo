#	@(#)Makefile	1.3 (Berkeley) 5/7/91

PRINTER=-Plz
TROFF=	ditroff ${PRINTER}
EQN=	deqn ${PRINTER}
TBL=	dtbl ${PRINTER}
LPR=	lpr ${PRINTER}

CSRC=	../common/3.t ../common/4.t ../common/5.t ../common/6.t 
SRC=	0.t 1.t 2.t ${CSRC} a.t b.t

setup.out: ${SRC}
	${EQN} ${SRC} | ${TBL} | ${TROFF} -t -ms ${PAGES} > setup.out

setup: setup.out
	${LPR} setup.out

clean:
	rm -f setup.out ?.t.spell errs Errs

spell:	${SRC}
	@for i in ${SRC}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
