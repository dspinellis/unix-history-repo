CFLAGS = -w -g
LDFLAGS=

iconx:		init.o interp.o pstart.o pstop.o start.o \
		../functions/Lib ../operators/Lib ../lib/Lib ../rt/Lib
	ld $(LDFLAGS) -X -o iconx start.o init.o pstart.o interp.o \
	   ../functions/Lib ../operators/Lib ../lib/Lib ../rt/Lib pstop.o -lc

init.o:		../h/rt.h ../h/err.h
interp.o:	../h/defs.s
start.o:	../h/defs.s
	cat ../h/defs.s start.s >t.c
	cc -E t.c | grep -v "^#" >t.s
	as -o start.o t.s
	rm -f t.c t.s
.s.o:
	cat ../h/defs.s $< >t.c
	cc -E t.c | grep -v "^#" >t.s
	as -o $@ t.s
	rm -f t.c t.s

Listall:
	@pr *.[cs]
	@date >List

List:		init.c interp.s pstart.s pstop.s start.s
	@pr $?
	@date >List

clean:
	rm -f iconx *.o

dist-clean:
	rm -f `gcomp Makefile *.s *.c *.gen`

