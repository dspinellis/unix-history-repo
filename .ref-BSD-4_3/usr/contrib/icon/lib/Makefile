CFLAGS = -O

Lib:		bscan.o create.o \
		efail.o escan.o esusp.o field.o invoke.o keywd.o \
		limit.o llist.o lsusp.o mkrec.o pfail.o pret.o psusp.o \
		coact.o coret.o cofail.o
		ar rc Lib $?
		ranlib Lib

bscan.o:	../h/rt.h ../h/config.h
coact.o:	../h/defs.s ../h/config.h
cofail.o:	../h/defs.s ../h/config.h
coret.o:	../h/defs.s ../h/config.h
create.o:	../h/rt.h ../h/config.h
efail.o:	../h/defs.s ../h/config.h
escan.o:	../h/rt.h ../h/config.h
esusp.o:	../h/defs.s ../h/config.h
field.o:	../h/rt.h ../h/record.h ../h/config.h
invoke.o:	../h/defs.s ../h/config.h
keywd.o:	../h/rt.h ../h/keyword.h ../h/config.h
limit.o:	../h/rt.h ../h/config.h
llist.o:	../h/rt.h ../h/config.h
lsusp.o:	../h/defs.s ../h/config.h
mkrec.o:	../h/rt.h ../h/record.h ../h/config.h
pfail.o:	../h/defs.s ../h/config.h
pret.o:		../h/defs.s ../h/config.h
psusp.o:	../h/defs.s ../h/config.h

Listall:
		@pr *.[cs]
		@date >List

List:		bscan.c coact.s cofail.s coret.s create.c \
		efail.s escan.c esusp.s field.c invoke.s keywd.c \
		limit.c llist.c lsusp.s mkrec.c pfail.s pret.s psusp.s
		@pr $?
		@date >List

.s.o:
		cat ../h/defs.s $< >t.c
		cc -E t.c | egrep -v "(^#|^$$)" >t.s
		as -o $@ t.s
		rm -f t.c t.s
Clean:
		rm -f Lib *.o

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f `gcomp Makefile *.c *.s`
