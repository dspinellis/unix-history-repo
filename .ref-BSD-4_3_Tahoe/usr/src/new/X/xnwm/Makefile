# Copyright 1985, Massachusetts Institute of Technology

INCLUDES= -I../include
DESTDIR =
CONFDIR = /usr/new
CFLAGS = -O $(INCLUDES)

.SUFFIXES: .o .h .c .a

####	Maintainance keys    #####

all: xnwm

install:
	install -s xnwm ${DESTDIR}${CONFDIR}

clean: 
	rm -f  *.CKP *.BAK core *~ *.o xnwm \#*

#####     xnwm     #####

xnwm.o: wm.h extern.h
wmsubs.o: wm.h extern.h
globals.o: wm.h extern.h

xnwm: xnwm.o wmsubs.o globals.o ../Xlib/Xlib.h
	cc ${CFLAGS} -o xnwm xnwm.o wmsubs.o globals.o ../Xlib/libX.a

profiled:
	cc -p ${CFLAGS} -o profiled xnwm.c wmsubs.c globals.c ../Xlib/libX.a

xnwmlint:
	lint -h -b -x xnwm.c wmsubs.c globals.c ../Xlib/llib-lX.ln
