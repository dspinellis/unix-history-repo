CFLAGS = -O -w

Lib:		abs.o any.o bal.o center.o close.o copy.o cset.o \
		display.o exit.o find.o get.o image.o integer.o \
		left.o list.o many.o map.o match.o move.o numeric.o \
		open.o pop.o pos.o pull.o push.o put.o read.o reads.o \
		real.o repl.o reverse.o right.o seq.o sort.o stop.o \
		string.o system.o tab.o table.o trim.o type.o upto.o \
		write.o writes.o
	ar r ../bin/Lib $?
	touch Lib

abs.o:		../h/rt.h
any.o:		../h/rt.h
bal.o:		../h/rt.h
center.o:	../h/rt.h
close.o:	../h/rt.h
copy.o:		../h/rt.h ../h/record.h
cset.o:		../h/rt.h
display.o:	../h/rt.h
exit.o:		../h/rt.h
find.o:		../h/rt.h
get.o:		../h/rt.h
image.o:	../h/rt.h ../h/record.h
integer.o:	../h/rt.h
left.o:		../h/rt.h
list.o:		../h/rt.h
many.o:		../h/rt.h
map.o:		../h/rt.h
match.o:	../h/rt.h
move.o:		../h/rt.h
numeric.o:	../h/rt.h
open.o:		../h/rt.h
pop.o:		../h/rt.h
pos.o:		../h/rt.h
pull.o:		../h/rt.h
push.o:		../h/rt.h
put.o:		../h/rt.h
read.o:		../h/rt.h
reads.o:	../h/rt.h
real.o:		../h/rt.h
repl.o:		../h/rt.h
reverse.o:	../h/rt.h
right.o:	../h/rt.h
seq.o:		../h/rt.h
sort.o:		../h/rt.h
stop.o:		../h/rt.h
string.o:	../h/rt.h
system.o:	../h/rt.h
tab.o:		../h/rt.h
table.o:	../h/rt.h
trim.o:		../h/rt.h
type.o:		../h/rt.h ../h/record.h
upto.o:		../h/rt.h
write.o:	../h/rt.h
writes.o:	../h/rt.h

Listall:
	@pr *.[cs]
	@date >List

List:		abs.c any.c bal.c center.c close.c copy.c cset.c \
		display.c exit.c find.c get.c image.c integer.c \
		left.c list.c many.c map.c match.c move.c numeric.c \
		open.c pop.c pos.c pull.c push.c put.c read.c reads.c \
		real.c repl.c reverse.c right.c sort.c stop.c \
		string.c system.c tab.c table.c trim.c type.c upto.c \
		write.c writes.c
	@pr $?
	@date >List

.s.o:
	as -o $@ ../h/defs.s $<
clean:
	rm -f *.o

dist-clean: clean
	rm -f `gcomp Makefile *.c`
