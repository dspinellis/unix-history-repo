PROG=	file
MAGIC	= /etc/magic
CFLAGS+= -DMAGIC='"$(MAGIC)"'
SRCS=	file.c apprentice.c fsmagic.c softmagic.c ascmagic.c is_tar.c \
	print.c $(LOCALSRCS)
MAN5=	magic.0
MAN1=	file.0

ascmagic.o:	names.h

apprentice.o ascmagic.o file.o fsmagic.o print.o softmagic.o: file.h

afterinstall:
	install -c ${.CURDIR}/magic $(MAGIC)

.include <bsd.prog.mk>
