#	Makefile for liszt v4.0



# Copyright (c) 1980 ,  The Regents of the University of California.
# All rights reserved.  
# author: j. foderaro

#	files required:
#	  caspecs - special declarations for whole file (included in all)
#	  camacs  - macros used by whole file 
#	  car.l	  - source file: startup code
#	  cadr.l  - actual compilation code
#
#	other files:
#	 uctolc.c - converts uppercase to lower case in first field of
#	            tags file.  
#	 ltags    - awk script for finding function defs
#
#	 /usr/lib/lisp/machacks.l  - maclisp compatibility functions
#
MCROS = camacs.l

MOBJ = camacs.o

FILES = car.l cadr.l cddr.l


OBJS = car.o cadr.o cddr.o


LLIB = $(DESTDIR)/usr/lib/lisp/
LSRC = /usr/src/cmd/lisp

LFILES = ${LLIB}machacks.l

LOBJS = ${LLIB}machacks.o

.SUFFIXES: .l
.l.o:
	liszt  $* > $*.blat


nliszt : ${MOBJ} ${OBJS} ${LOBJS} ${LLIB}/tackon
	echo "(progn (fasl 'car 'map)(sstatus appendmap t)\
		     (fasl 'cadr 'map)\
		     (fasl 'cddr 'map)\
	(dumplisp nliszt))" | lisp 
	$(LLIB)/tackon map nliszt
	@echo nliszt done


lxref:	lxref.o ${LLIB}jkfmacs.o 
	echo "(progn (fasl 'lxref) (dumplisp lxref))" | lisp

tags: /dev/tty uctolc
	awk -f ltags ${MCROS} ${FILES} | uctolc | sort > tags


install: nliszt lxref
	mv nliszt $(DESTDIR)/usr/ucb/liszt
	mv lxref $(DESTDIR)/usr/ucb
	chmod 1755 $(DESTDIR)/usr/ucb/liszt
	@echo liszt installed

${LLIB}machacks.o : ${LLIB}machacks.l
	liszt -m ${LLIB}machacks > ${LLIB}machacks.blat

uctolc: uctolc.c
	cc -O uctolc.c -o uctolc

clean:
	rm -f *.o *.blat map tags uctolc
