# @(#)Makefile	4.17 (Berkeley) 2/23/83

CFLAGS=	-O

OBJS=	abort.o abs.o alloca.o atof.o atoi.o atol.o \
	calloc.o chrtab.o closedir.o creat.o crypt.o ctime.o ctype_.o \
	ecvt.o errlst.o execvp.o exit.o \
	fabs.o frexp.o \
	getenv.o getlogin.o getwd.o \
	index.o initgroups.o isatty.o itol.o \
	ldexp.o locv.o ltod.o ltoi.o \
	malloc.o mktemp.o modf.o \
	nargs.o nlist.o \
	opendir.o \
	perror.o psignal.o \
	qsort.o \
	rand.o random.o readdir.o regex.o reset.o rin.o rindex.o \
	scandir.o seekdir.o setjmp.o siglist.o sleep.o strcat.o strcatn.o \
	strcmp.o strcmpn.o strcpy.o strcpyn.o strlen.o strncat.o strncmp.o \
	strncpy.o stty.o swab.o \
	tell.o telldir.o time.o timezone.o ttyname.o ttyslot.o \
	udiv.o urem.o \
	valloc.o

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

.s.o:
	sed -f mcount.sed $*.s | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	${AS} -o $*.o $*.s
	-ld -x -r $*.o
	mv a.out $*.o

genlib genlib_p: ${OBJS}
	ar cru genlib ${OBJS}
	cd profiled; ar cru ../genlib_p ${OBJS}

clean:
	rm -f genlib genlib_p ${OBJS} profiled/*.o

errlst.o: errlst.c
	cc -S errlst.c
	ed - <:errfix errlst.s
	as -o errlst.o errlst.s
	cp errlst.o profiled/errlst.o
	rm errlst.s
