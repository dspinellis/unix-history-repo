
PROG=	tar
SRCS=	tar.c create.c extract.c buffer.c getoldopt.c update.c gnu.c mangle.c \
	version.c list.c names.c diffarch.c port.c wildmat.c getopt.c getopt1.c \
	regex.c getdate.y rtape_lib.c
CFLAGS+= -DBSD42 -DSTDC_MSG -DNEED_TZSET -DNEEDPAD
CFLAGS+= -DDEF_AR_FILE=\"/dev/rmt8\" -DDEFBLOCKING=20
NOMAN=noman

.include <bsd.prog.mk>
