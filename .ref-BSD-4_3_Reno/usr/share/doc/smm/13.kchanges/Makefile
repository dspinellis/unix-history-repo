#	@(#)Makefile	1.6 (Berkeley) 6/30/90

DIR=	smm/13.kchanges
SRCS=	intro.t gen.t h.t sys.t sys.kern.t sys.tty.t sys.ufs.t sys.uipc.t \
	sys.vm.t vax.t net.t netinet.t netns.t vaxif.t vaxmba.t vaxuba.t \
	stand.t
MACROS=	-ms

paper.${PRINTER}: ${SRCS}
	${EQN} ${SRCS} | ${TBL} | ${ROFF} > ${.TARGET}

.include <bsd.doc.mk>
