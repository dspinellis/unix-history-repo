SCRMAPS = koi8-r2alt.scr

SCRMAPS_MK = ${SCRMAPS:R:S/$/.mk/g}
SCRDIR = /usr/share/syscons/scrnmaps
NOMAN = noman

all: ${SCRMAPS}

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${SCRMAPS} \
		${DESTDIR}${SCRDIR}

clean:
	-rm -f ${SCRMAPS} ${SCRMAPS_MK} core *.o

${SCRMAPS}: ${.TARGET:R}.mk
	./${.TARGET:R}.mk ${.TARGET}

${SCRMAPS_MK}: ${.TARGET:R} mkscrfil.c
	${CC} ${CFLAGS} -I${.CURDIR} -DFIL=\"${.TARGET:R}\" \
			-o ${.TARGET} ${.CURDIR}/mkscrfil.c

.include <bsd.prog.mk>
