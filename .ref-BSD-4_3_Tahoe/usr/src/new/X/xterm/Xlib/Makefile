INCLUDES= -I/usr/include/X -I../../Xlib
C2= /lib/c2
#
# The KEYBD define may be include to include the keyboard mods.
CFLAGS= -O ${INCLUDES} ${NETOPTIONS} -DKEYBD

.SUFFIXES: .o .h .c

.c.o:
#	${CC} -pg ${CFLAGS} -c -S $*.c
#	${C2} $*.s | ../../inline/inline | ${AS} -o $*.o
#	rm -f $*.s
#	-ld -X -r $*.o
#	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c -S $*.c
	${C2} $*.s | ../../inline/inline | ${AS} -o $*.o
	rm -f $*.s
	-ld -x -r $*.o
	mv a.out $*.o

OLIST=	XKeyBind.o

libX.a: $(OLIST)
	ar cr libX.a $(OLIST)
	@ranlib libX.a
#
#libX_p.a: $(OLIST)
#	cd profiled; ar cr ../libX_p.a $(OLIST)
#	@ranlib libX_p.a

$(OLIST): ../../Xlib/Xlib.h

XKeyBind.o: ../../Xlib/Xkeyboard.h ../../Xlib/Xkeymap.h Xdefault.h

XGetDefault.o: Xdefault.h
