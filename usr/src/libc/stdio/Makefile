# %W% (Berkeley) %G%
CFLAGS=	-O
OBJS=	clrerr.o data.o doprnt.o doscan.o error.o \
	fdopen.o fgetc.o fgets.o filbuf.o flsbuf.o fopen.o \
	fprintf.o fputc.o fputs.o freopen.o fseek.o fstab.o ftell.o \
	gcvt.o getchar.o getgrent.o getgrgid.o getgrnam.o \
	getpass.o getpw.o getpwent.o getpwnam.o getpwuid.o gets.o getw.o \
	intss.o popen.o printf.o putchar.o puts.o putw.o \
	rdwr.o rew.o scanf.o setbuf.o sprintf.o strout.o stuff.o system.o \
	tmpnam.o ungetc.o

.c.o:
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

.s.o:
	${AS} -o $*.o $*.s
	-ld -x -r $*.o
	mv a.out $*.o

all: ${OBJS}

clean:
	rm -f ${OBJS}

doprnt.o:
	cp doprnt.s doprnt.c
	cc -E doprnt.c | as -o doprnt.o
	-ld -x -r doprnt.o
	mv a.out doprnt.o
	rm -f doprnt.c
