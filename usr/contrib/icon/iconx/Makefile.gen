CFLAGS =
LDFLAGS=

iconx:		main.o init.o interp.o pstart.o pstop.o start.o \
		../functions/Lib ../operators/Lib ../lib/Lib ../rt/Lib
		cc $(LDFLAGS) -X -o iconx main.o start.o init.o pstart.o \
		interp.o ../functions/Lib ../operators/Lib ../lib/Lib \
		../rt/Lib pstop.o

init.o:		../h/rt.h ../h/err.h ../h/config.h ../h/pdef.h
.s.o:		../h/defs.s ../h/config.h
		cat ../h/defs.s $< >t.c
		cc -E t.c | egrep -v "(^#|^$$)" >t.s
		as -o $@ t.s
		rm -f t.c t.s

Listall:
		@pr *.[cs]
		@date >List

List:		init.c interp.s main.c pstart.s pstop.s start.s
		@pr $?
		@date >List

Clean:
		rm -f iconx *.o

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f `gcomp Makefile *.s *.c *.gen`
