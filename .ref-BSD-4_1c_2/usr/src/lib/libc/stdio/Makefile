# %W% (Berkeley) %G%

CFLAGS=	-O

OBJS=	clrerr.o data.o disktab.o doprnt.o doscan.o error.o \
	fdopen.o fgetc.o fgets.o filbuf.o flsbuf.o fopen.o \
	fprintf.o fputc.o fputs.o freopen.o fseek.o fstab.o ftell.o \
	gcvt.o getchar.o getgrent.o getgrgid.o getgrnam.o \
	getpass.o getpw.o getpwent.o getpwnam.o getpwuid.o gets.o getw.o \
	intss.o popen.o printf.o putchar.o puts.o putw.o rdwr.o rew.o \
	scanf.o setbuf.o setbuffer.o sprintf.o strout.o stuff.o system.o \
	tmpnam.o ungetc.o

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

stdiolib stdiolib_p: ${OBJS}
	ar cru stdiolib ${OBJS}
	cd profiled; ar cru ../stdiolib_p ${OBJS}

clean:
	rm -f stdiolib stdiolib_p ${OBJS} profiled/*.o

doprnt.o: doprnt.s
	cp doprnt.s doprnt.c
	cc -E doprnt.c | sed -f mcount.sed | as -o doprnt.o
	-ld -x -r doprnt.o
	mv a.out profiled/doprnt.o
	cc -E doprnt.c | as -o doprnt.o
	-ld -x -r doprnt.o
	mv a.out doprnt.o
	rm -f doprnt.c
