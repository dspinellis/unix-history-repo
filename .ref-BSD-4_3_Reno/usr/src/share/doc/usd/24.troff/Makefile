#	@(#)Makefile	6.4 (Berkeley) 6/30/90

DIR=	usd/24.troff
INTRO=	m0
TUTOR=	m0a
PAPER=	m1 m2 m3 m4
APNDX1=	m5
APNDX2=	table1
APNDX3=	table2
MACROS=	m.mac

paper.${PRINTER}: intro.${PRINTER} tutor.${PRINTER} apndx1.${PRINTER} \
	apndx2.${PRINTER} apndx3.${PRINTER}
	${TBL} ${PAPER} | ${ROFF} > ${.TARGET}

intro.${PRINTER}: ${INTRO}
	${ROFF} ${INTRO} > ${.TARGET}

tutor.${PRINTER}: ${TUTOR}
	${TBL} ${TUTOR} | ${ROFF} > ${.TARGET}

apndx1.${PRINTER}: ${APNDX1}
	${ROFF} ${APNDX1} > ${.TARGET}

apndx2.${PRINTER}: ${APNDX2}
	${ROFF} ${APNDX2} > ${.TARGET}

apndx3.${PRINTER}: ${APNDX3}
	${ROFF} ${APNDX3} > ${.TARGET}

clean:
	rm -f intro.${PRINTER} tutor.${PRINTER} paper.${PRINTER} \
	    apndx1.${PRINTER} apndx2.${PRINTER} apndx3.${PRINTER} \
	    *.spell errs Errs make.out

spell: ${INTRO} ${TUTOR} ${PAPER} ${APNDX1} ${APNDX2} ${APNDX3}
	@for i in ${INTRO} ${TUTOR} ${PAPER} ${APNDX1} ${APNDX2} ${APNDX3}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done

.include <bsd.doc.mk>
