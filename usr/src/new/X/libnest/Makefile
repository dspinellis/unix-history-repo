#
# Copyright 1986, Massachusetts Institute of Technology
#
# Makefile for X Emulation library.
#

INCLUDES= -I../include
CFLAGS= -O ${INCLUDES}

OBJS= nest.o

all: libnest.a

# as this library is private to X, we should do nothing on install.

install: all

libnest.a: $(OBJS)
	ar r libnest.a $(OBJS)
	@ranlib libnest.a

clean:
	rm -f ERRS *.o libnest.a *~
