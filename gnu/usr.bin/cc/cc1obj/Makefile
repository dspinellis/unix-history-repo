PROG=           gcc1obj
BINDIR=         /usr/libexec
SRCS=		objc-parse.c objc-act.c  \
		c-lex.c c-pragma.c c-decl.c c-typeck.c c-convert.c\
		c-aux-info.c c-iterate.c 
CFLAGS+=        -I. -I$(.CURDIR) -I$(.CURDIR)/../lib
YFLAGS=
NOMAN=          noman
.PATH:          $(.CURDIR)/../cc1

.if exists(${.CURDIR}/../lib/obj)
LDADD=  -L${.CURDIR}/../lib/obj -lgcc2
.else
LDADD=  -L${.CURDIR}/../lib/ -lgcc2
.endif

.include <bsd.prog.mk>
