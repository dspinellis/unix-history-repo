PROG=           cc1obj
BINDIR=         /usr/libexec
SRCS=		objc-parse.c objc-act.c  \
		c-lex.c c-pragma.c c-decl.c c-typeck.c c-convert.c\
		c-aux-info.c c-iterate.c \
		$(COMMON_SRC)
CFLAGS+=        -I. -I$(.CURDIR) -I$(.CURDIR)/../common
YFLAGS=
NOMAN=          noman
.PATH:          $(.CURDIR)/../common $(.CURDIR)/../cc1

.include <bsd.prog.mk>
