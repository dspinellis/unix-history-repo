DESTDIR=
CFLAGS = -O -d2

px: int.o perror.o stats.o utilities.o malloc.o px.o
	cc -O -o px int.o px.o utilities.o perror.o stats.o malloc.o -lm
int.o: h00vars.h
perror.o stats.o: h00vars.h h01errs.h
utilities.o: h00vars.h h01errs.h h02opcs.h
malloc.o: h01errs.h
	cc -O -c -d2 -Ddebug malloc.c
px.o: [0-3][0-9]*.s
	cat [0-3][0-9]*.s | as -d2 -o px.o
h01errs.h: errdata
	ed - <make.ed1
	rm -f E.s

install: px
	install -s px ${DESTDIR}/usr/ucb/px

clean:
	rm -f *.o px  

print:
	@pr READ_ME makefile
	@ls -l | pr
	@pr opcpic h00vars.h int.c
	@cat [0-3][0-9]*.s >tmp
	@ed - tmp <make.ed2
	@num <tmp | pr
	@pr perror.c stats.c utilities.c
	@rm -f tmp
