#	Makefile	4.2	83/07/05
#
CFLAGS=	-O
STD=	ftime.o gtty.o nice.o pause.o rand.o stty.o tell.o \
	times.o utime.o vlimit.o vtimes.o
# these should just be pitched, but in the interest of compatibility...
TRASH=	getpw.o 
OBJS=	${STD} ${TRASH}

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

compat-4.1lib compat-4.1lib_p: ${OBJS}
	@echo "building profiled compat-4.1lib"
	@cd profiled; ar cru ../compat-4.1lib_p ${OBJS}
	@echo "buiding normal compat-4.1lib"
	@ar cru compat-4.1lib ${OBJS}

clean:
	rm -f compat-4.1lib compat-4.1lib_p ${OBJS} profiled/*.o
