#	@(#)Makefile	6.5 (Berkeley) %G%

DIR=	usd/21.troff
INTRO=	m0
TUTOR=	m0a
PAPER=	m1 m2 m3 m4
APNDX1=	m5
APNDX2=	table1
APNDX3=	table2
MACROS=	m.mac

paper.ps: intro.ps tutor.ps apndx1.ps apndx2.ps apndx3.ps
	${TBL} ${PAPER} | ${ROFF} > ${.TARGET}

intro.ps: ${INTRO}
	${ROFF} ${INTRO} > ${.TARGET}

tutor.ps: ${TUTOR}
	${TBL} ${TUTOR} | ${ROFF} > ${.TARGET}

apndx1.ps: ${APNDX1}
	${ROFF} ${APNDX1} > ${.TARGET}

apndx2.ps: ${APNDX2}
	${ROFF} ${APNDX2} > ${.TARGET}

apndx3.ps: ${APNDX3}
	${ROFF} ${APNDX3} > ${.TARGET}

clean:
	rm -f intro.ps tutor.ps paper.ps apndx1.ps apndx2.ps apndx3.ps \
	    *.spell errs Errs make.out

spell: ${INTRO} ${TUTOR} ${PAPER} ${APNDX1} ${APNDX2} ${APNDX3}
	@for i in ${INTRO} ${TUTOR} ${PAPER} ${APNDX1} ${APNDX2} ${APNDX3}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done

.include <bsd.doc.mk>
