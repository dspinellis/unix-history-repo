PROG=		gcc1plus
BINDIR=		/usr/libexec
SRCS=		cp-parse.c cp-decl.c cp-decl2.c cp-typeck.c cp-type2.c \
		cp-tree.c cp-ptree.c cp-cvt.c cp-search.c cp-lex.c \
		cp-gc.c cp-call.c cp-class.c cp-init.c cp-method.c \
		cp-except.c cp-expr.c cp-pt.c cp-edsel.c cp-xref.c \
		cp-spew.c 

CFLAGS+=        -I. -I$(.CURDIR) -I$(.CURDIR)/../lib
NOMAN=		noman
YFLAGS=

.if exists(${.CURDIR}/../lib/obj)
LDADD=  -L${.CURDIR}/../lib/obj -lgcc2
.else
LDADD=  -L${.CURDIR}/../lib/ -lgcc2
.endif

.include <bsd.prog.mk>
