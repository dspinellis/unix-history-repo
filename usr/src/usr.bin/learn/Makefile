#	@(#)Makefile	4.5	(Berkeley)	87/05/31
#
DESTDIR=

# The vi lessons come with the user-contributed software.
LESSONS = files editor morefiles macros eqn C

FILES = lrnref.h \
	copy.c dounit.c getlesson.c learn.c list.c \
	makpipe.c maktee.c mem.c mysys.c selsub.c selunit.c \
	start.c whatnow.c wrapup.c \
	lcount.c lrntee.c \
	makefile

OBJECTS = copy.o dounit.o getlesson.o learn.o list.o mem.o \
	makpipe.o maktee.o mysys.o selsub.o selunit.o \
	start.o whatnow.o wrapup.o

# Flag BSD4_2 means 4.2 or later.
CFLAGS = -O -DBSD4_2
LIBRARY =
LLIB	= $(DESTDIR)/usr/lib/learn/bin

all:	learn lrntee lcount

install:	all
	install -c -s learn $(DESTDIR)/usr/bin
	install -s learn $(LLIB)
	install -s lrntee $(LLIB)
	install -s lcount $(LLIB)

clean:
	rm -f *.o learn lrntee lcount errs

cmp:	all
	cmp learn $(DESTDIR)/usr/bin/learn
	cmp learn $(LLIB)/learn
	cmp lrntee $(LLIB)/lrntee
	cmp lcount $(LLIB)/lcount
	rm learn lrntee lcount *.o

learn: $(OBJECTS)
	cc -o learn $(CFLAGS) $(OBJECTS) $(LIBRARY)

$(OBJECTS): lrnref.h

lcount lrntee:
	cc $(CFLAGS) $@.c -o $@ $(LIBRARY)

log:
	-rm -r $(LLIB)/../log; mkdir $(LLIB)/../log; chmod +w $(LLIB)/../log

check:
	-@test -r $(LLIB)/lrntee || echo 'lrntee not present; make lrntee'
	-@test -r $(LLIB)/lcount || echo 'lcount not present; make lcount'
	-@test -r $(LLIB)/../log || echo 'log directory not present; make log if you want logging'

depend:
