#	@(#)Makefile	8.2 (Berkeley) 4/22/94

DIR=	usd/13.viref
SRCS=	vi.ref set.opt.roff
MACROS=	-man
CLEANFILES+=vi.ref.txt vi.ref.ps vi.ref.roff

paper.ps: vi.ref.roff
	${ROFF} vi.ref.roff > ${.TARGET}

vi.ref.txt: vi.ref.roff
	groff -man -Tascii < vi.ref.roff > $@

vi.ref.ps: vi.ref.roff
	groff -man < vi.ref.roff > $@

vi.ref.roff: ${SRCS}
	cpp -DREFERENCE -I. ${.CURDIR}/vi.ref | sed -e '/^#/d' -e '/^$$/d' > $@

.include <bsd.doc.mk>
