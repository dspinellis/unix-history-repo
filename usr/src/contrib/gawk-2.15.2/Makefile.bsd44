PROG=	awk
SRCS=	main.c eval.c builtin.c msg.c iop.c io.c field.c getopt1.c \
	getopt.c array.c \
	node.c version.c missing.c re.c awk.c regex.c dfa.c
DPADD=	${LIBM}
LDADD=	-lm
CFLAGS+=-I${.CURDIR} -DGAWK
BINDIR=	/usr/bin
MANDIR=	/usr/share/man/cat
CLEANFILES+=awk.c y.tab.h awk.0

awk.0: gawk.1
	nroff -man gawk.1 > awk.0

.include <bsd.prog.mk>
