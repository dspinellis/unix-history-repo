#	@(#)Makefile	5.15 (Berkeley) 6/8/90

MAN3=	assert.0 bitstring.0 end.0 intro.0 monitor.0 varargs.0
MLINKS=	end.3 edata.3 end.3 etext.3 monitor.3 moncontrol.3 monitor.3 \
	monstartup.3

all: ${MAN3}

clean depend lint tags:

cleandir:
	rm -f ${MAN3}

install: maninstall

.include <bsd.prog.mk>
