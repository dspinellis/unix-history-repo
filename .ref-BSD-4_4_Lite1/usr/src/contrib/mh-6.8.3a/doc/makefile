#	@(#)Makefile	5.3 (Berkeley) 8/9/93

DIR=    usd/08.mh/doc
SRCS=	MH.me
MACROS= -me

all: paper.ps

inst-all:
	make DESTDIR=$(DESTDIR) $(MFLAGS) -f Makefile inst-all

manpages:
	make -f Makefile

paper.ps: manpages
	${ROFF} ${SRCS} > ${.TARGET}

.include <bsd.doc.mk>
