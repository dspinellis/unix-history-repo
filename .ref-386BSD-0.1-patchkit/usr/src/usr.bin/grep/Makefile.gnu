#
# Makefile for GNU e?grep
#

# Add -DUSG for System V.
CFLAGS = -O

#
# You may add getopt.o if your C library lacks getopt(); note that
# 4.3BSD getopt() is said to be somewhat broken.
#
# Add alloca.o if your machine does not support alloca().
#
OBJS = dfa.o regex.o
GOBJ = grep.o
EOBJ = egrep.o

# Space provided for machine dependent libraries.
LIBS =

all: regress

regress: egrep grep
	cd tests; sh regress.sh

egrep: $(OBJS) $(EOBJ)
	$(CC) $(CFLAGS) -o egrep $(OBJS) $(EOBJ) $(LIBS)

egrep.o: grep.c
	rm -f egrep.c; cp grep.c egrep.c
	$(CC) $(CFLAGS) -DEGREP -c egrep.c
	rm -f egrep.c

grep: $(OBJS) $(GOBJ)
	$(CC) $(CFLAGS) -o grep $(OBJS) $(GOBJ) $(LIBS)

clean:
	rm -f grep egrep *.o core tests/core tests/tmp.script \
	tests/khadafy.out egrep.c

dfa.o egrep.o grep.o: dfa.h
egrep.o grep.o regex.o: regex.h
