CFLAGS = -O

Lib:		abs.o any.o bal.o center.o close.o collect.o copy.o cset.o \
		delete.o display.o exit.o find.o get.o image.o insert.o integer.o \
		left.o list.o many.o map.o match.o member.o move.o numeric.o \
		open.o pop.o pos.o pull.o push.o put.o proc.o read.o reads.o \
		real.o repl.o reverse.o right.o seq.o set.o sort.o stop.o \
		string.o system.o tab.o table.o trim.o type.o upto.o \
		write.o writes.o
		ar rc Lib $?
		ranlib Lib

abs.o:		../h/rt.h ../h/config.h
any.o:		../h/rt.h ../h/config.h
bal.o:		../h/rt.h ../h/config.h
center.o:	../h/rt.h ../h/config.h
close.o:	../h/rt.h ../h/config.h
collect.o:	../h/rt.h ../h/config.h
copy.o:		../h/rt.h ../h/record.h ../h/config.h
cset.o:		../h/rt.h ../h/config.h
delete.o:	../h/rt.h ../h/config.h
display.o:	../h/rt.h ../h/config.h
exit.o:		../h/rt.h ../h/config.h
find.o:		../h/rt.h ../h/config.h
get.o:		../h/rt.h ../h/config.h
image.o:	../h/rt.h ../h/record.h ../h/config.h
insert.o:	../h/rt.h ../h/config.h
integer.o:	../h/rt.h ../h/config.h
left.o:		../h/rt.h ../h/config.h
list.o:		../h/rt.h ../h/config.h
many.o:		../h/rt.h ../h/config.h
map.o:		../h/rt.h ../h/config.h
match.o:	../h/rt.h ../h/config.h
member.o:	../h/rt.h ../h/config.h
move.o:		../h/rt.h ../h/config.h
numeric.o:	../h/rt.h ../h/config.h
open.o:		../h/rt.h ../h/config.h
pop.o:		../h/rt.h ../h/config.h
pos.o:		../h/rt.h ../h/config.h
pull.o:		../h/rt.h ../h/config.h
push.o:		../h/rt.h ../h/config.h
put.o:		../h/rt.h ../h/config.h
proc.o:		../h/rt.h ../h/config.h
read.o:		../h/rt.h ../h/config.h
reads.o:	../h/rt.h ../h/config.h
real.o:		../h/rt.h ../h/config.h
repl.o:		../h/rt.h ../h/config.h
reverse.o:	../h/rt.h ../h/config.h
right.o:	../h/rt.h ../h/config.h
seq.o:		../h/rt.h ../h/config.h
set.o:		../h/rt.h ../h/config.h
sort.o:		../h/rt.h ../h/config.h
stop.o:		../h/rt.h ../h/config.h
string.o:	../h/rt.h ../h/config.h
system.o:	../h/rt.h ../h/config.h
tab.o:		../h/rt.h ../h/config.h
table.o:	../h/rt.h ../h/config.h
trim.o:		../h/rt.h ../h/config.h
type.o:		../h/rt.h ../h/record.h ../h/config.h
upto.o:		../h/rt.h ../h/config.h
write.o:	../h/rt.h ../h/config.h
writes.o:	../h/rt.h ../h/config.h

Listall:
		@pr *.[cs]
		@date >List

List:		abs.c any.c bal.c center.c close.c collect.c copy.c cset.c \
		display.c exit.c find.c get.c image.c integer.c \
		left.c list.c many.c map.c match.c move.c numeric.c \
		open.c pop.c pos.c pull.c push.c put.c proc.c seq.c read.c reads.c \
		real.c repl.c reverse.c right.c sort.c stop.c \
		string.c system.c tab.c table.c trim.c type.c upto.c \
		write.c writes.c delete.c insert.c member.c set.c
		@pr $?
		@date >List

Clean:
		rm -f Lib *.o

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f `gcomp Makefile *.c`
