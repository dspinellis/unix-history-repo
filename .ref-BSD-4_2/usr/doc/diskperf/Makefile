TROFF=	itroff
FILES=	abs.ms motivation.ms equip.ms methodology.ms tests.ms results.ms \
	conclusions.ms appendix.ms

paper:	${FILES}
	@tbl ${FILES} | ${TROFF} -ms

preview:${FILES}
	tbl ${FILES} | nroff -ms | colcrt
