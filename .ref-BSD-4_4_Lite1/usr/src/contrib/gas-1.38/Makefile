#	@(#)Makefile	6.3 (Berkeley) 5/26/93

PROG=		as
SRCS=		app.c append.c as.c atof-generic.c bignum-copy.c \
		expr.c flonum-const.c flonum-copy.c flonum-mult.c \
		frags.c hash.c hex-value.c input-file.c input-scrub.c \
		messages.c obstack.c output-file.c read.c subsegs.c \
		symbols.c version.c write.c xmalloc.c xrealloc.c
CFLAGS+=	-I$(.CURDIR) -I$(.CURDIR)/config \
		-DSIGTY=void -Derror=as_fatal
BINDIR=	/usr/bin
NOMAN=	noman
.PATH: $(.CURDIR)/config

.include "config/Makefile.$(MACHINE)"

.include <bsd.prog.mk>
