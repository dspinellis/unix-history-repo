# %W% (Berkeley) %G%
CFLAGS=	-O
OBJS=	atof.o atoi.o atol.o calloc.o chrtab.o crypt.o ctime.o ctype_.o \
	ecvt.o errlst.o execvp.o frexp.o getenv.o getlogin.o \
	index.o isatty.o l3tol.o ldexp.o ltol3.o \
	malloc.o mktemp.o modf.o mon.o mpx.o nlist.o \
	perror.o qsort.o rand.o regex.o rin.o rindex.o \
	sleep.o strcat.o strcatn.o strcmp.o strcmpn.o strcpy.o \
	strcpyn.o strlen.o strncat.o strncmp.o strncpy.o stty.o swab.o \
	tell.o timezone.o ttyname.o ttyslot.o valloc.o

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

errlst.o: errlst.c
	cc -S errlst.c
	ed - <:errfix errlst.s
	as -o errlst.o errlst.s
	rm errlst.s
