#
# Copyright 1985, Massachusetts Institute of Technology
#
# Makefile for Vs100 X library.
#


INCLUDES= -I../include
CFLAGS= -O ${INCLUDES}

OBJS= bitpix.o copy.o cursor.o dl.o draw.o fill.o font.o maps.o \
	   move.o packet.o put.o report.o text.o tile.o util.o vsalloc.o

all: libvs100.a

site:
	rm -f dl.o font.o

# as this library is private to X, we should do nothing on install.

install: all

lint:
	lint -Cvs100 *.c

libvs100.a: $(OBJS)
	ar r libvs100.a $(OBJS)
	@ranlib libvs100.a

clean:
	rm -f ERRS *.o libvs100.a *~
