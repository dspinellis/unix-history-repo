#
# retrofit - fit a v7 program onto a v6 system
#
LIBDIR=	/usr/lib
OBJS=	getenv.o getpwuid.o getpwent.o\
	_getuid.o _sgtty.o htmpg.o htmps.o ttyname.o ttyn.o isatty.o _stat.o\
	setjmp.o wait.o ilongops.o ctype.o typeof.o lseek.o
AR=	-ar
AS=	-as
CFLAGS=	-O -I/usr/include/retrofit
ED=	-ed

libretro.a: ${OBJS}
	rm -f libretro.a
	${AR} r libretro.a ${OBJS}

ctype.o: ctype.s
	${ED} <:cfix ctype.s
	${AS} -o ctype.o ctype.s

ctype.s: ctype.c
	${CC} -I/usr/include/retrofit -S ctype.c

print:
	@pr READ_ME
	@pr ../hdrs/*.h ../hdrs/sys/*.h
	@pr *.c *.s

clean:
	rm -f libretro.a ctype.s
	-rm -f *.o

install: libretro.a
	cat /dev/null >> /etc/htmp
	mv libretro.a ${LIBDIR}/libretro.a
	chmod 644 ${LIBDIR}/libretro.a
