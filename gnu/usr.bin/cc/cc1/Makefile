#	@(#)Makefile	6.2 (Berkeley) 2/2/91

PROG=		gcc1
BINDIR=		/usr/libexec
SRCS=		c-parse.c c-lang.c  c-lex.c c-pragma.c c-decl.c \
		c-typeck.c c-convert.c c-aux-info.c c-iterate.c \
		$(COMMON_SRC)
	
CFLAGS+=	-I. -I$(.CURDIR) -I$(.CURDIR)/../common
YFLAGS=
NOMAN=		noman
.PATH:		$(.CURDIR)/../common
.include <bsd.prog.mk>
