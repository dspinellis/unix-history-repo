CFLAGS=-O 

o=getgrgid.o getgrnam.o getgrent.o getpwnam.o getpwuid.o getpwent.o\
intss.o getchar.o putchar.o fgetc.o fputc.o freopen.o fdopen.o\
fgets.o fputs.o getpw.o fseek.o rew.o rdwr.o stuff.o\
system.o tmpnam.o popen.o fopen.o scanf.o doscan.o\
fprintf.o gets.o getw.o printf.o puts.o putw.o\
sprintf.o ungetc.o filbuf.o setbuf.o doprnt.o ffltpr.o\
strout.o flsbuf.o error.o ftell.o gcvt.o clrerr.o\
data.o    

libS.a : $o
	ar cr libS.a $o

clean :
	rm -f *.o

doprnt.o :
	cp doprnt.s doprnt.c
	cc -E doprnt.c | as -o doprnt.o
	-ld -x -r doprnt.o
	mv a.out doprnt.o
	rm doprnt.c

.c.o :
	cc $(CFLAGS) -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

.s.o :
	as -o $*.o $*.s

