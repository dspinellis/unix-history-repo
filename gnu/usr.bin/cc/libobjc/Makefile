LIB=	objc
SRCS=	hash.c sarray.c class.c sendmsg.c init.c archive.c\
	selector.c objects.c misc.c Object.m Protocol.m
CFLAGS=	 -I$(.CURDIR)/../common -I$(.CURDIR) -I..
.include <bsd.lib.mk>
.SUFFIXES: .m

.m.o:
	$(CC) -c $(CFLAGS) $<
.m.po:
	$(CC) -p -c $(CFLAGS) -o ${.TARGET} $<
