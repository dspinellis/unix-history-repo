echo	"creating new portable C library"
cc	-c -O -f *.c
as	!cin.s
mv	a.out	!cin.o
as	!cout.s
mv	a.out	!cout.o
as	__exit.s
mv	a.out	__exit.o
as	alloc.s
mv	a.out	alloc.o
ar rv	portlib \
	scanf.o \
	gets.o \
	getchar.o \
	cgetc.o \
	ungetc.o \
	tmpnam.o \
	printf.o \
	clenf.o \
	puts.o \
	putchar.o \
	cputc.o \
	copen.o \
	__makbuf.o \
	ceof.o \
	rew.o \
	ftoa.o \
	cexit.o \
	__exit.o \
	flush.o \
	cclose.o \
	cflush.o \
	system.o \
	__error.o \
	__prtint.o \
	wdleng.o \
	portlib.o \
	__length.o \
	calloc.o \
	alloc.o \
	intss.o \
	cread.o \
	cwrite.o \
	!cin.o \
	!cout.o \

rm	*.o
echo	"new portlib in ./portlib"
echo	"move it to /lib/libP.a"
