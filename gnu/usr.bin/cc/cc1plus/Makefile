PROG=		cc1plus
BINDIR=		/usr/libexec
SRCS=		cp-parse.c cp-decl.c cp-decl2.c cp-typeck.c cp-type2.c \
		cp-tree.c cp-ptree.c cp-cvt.c cp-search.c cp-lex.c \
		cp-gc.c cp-call.c cp-class.c cp-init.c cp-method.c \
		cp-except.c cp-expr.c cp-pt.c cp-edsel.c cp-xref.c \
		cp-spew.c $(COMMON_SRC)

CFLAGS+=        -I. -I$(.CURDIR) -I$(.CURDIR)/../common
NOMAN=		noman
YFLAGS=
.PATH:          $(.CURDIR)/../common

.include <bsd.prog.mk>
