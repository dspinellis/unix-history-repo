#
#	Athena X window system XMenu system makefile.
#
#		Written by:	Tony Della Fera, DEC
#

DESTDIR =
INCLUDES = -I../include

C2 = /lib/c2
CFLAGS = -O ${INCLUDES}
XLIB = ../Xlib/libX.a
XLIB_H = ../Xlib/Xlib.h
XLIB_P = ../Xlib/libX_p.a

.SUFFIXES: .o .h .c

#.c.o:
#	${CC} -pg ${CFLAGS} -c -S $*.c
#	${C2} $*.s | ../inline/inline | ${AS} -o $*.o
#	rm -f $*.s
#	-ld -X -r $*.o
#	mv a.out profiled/$*.o
#	${CC} ${CFLAGS} -c -S $*.c
#	${C2} $*.s | ../inline/inline | ${AS} -o $*.o
#	rm -f $*.s
#	-ld -x -r $*.o
#	mv a.out $*.o

.c.o:
	${CC} -pg ${CFLAGS} -c $*.c
	mv $*.o profiled/
	${CC} ${CFLAGS} -c $*.c

SRCS = 	XMenuInternal.c \
	XMenuCreate.c \
	XMenuAddPane.c \
	XMenuAddSel.c \
	XMenuInsPane.c \
	XMenuInsSel.c \
	XMenuFindPane.c \
	XMenuFindSel.c \
	XMenuChgPane.c \
	XMenuChgSel.c \
	XMenuSetPane.c \
	XMenuSetSel.c \
	XMenuRecomp.c \
	XMenuEvHand.c \
	XMenuLocate.c \
	XMenuSetFrz.c \
	XMenuSetAEQ.c \
	XMenuActivate.c \
	XMenuPost.c \
	XMenuDelPane.c \
	XMenuDelSel.c \
	XMenuDestroy.c \
	XMenuError.c

OBJS = 	XMenuInternal.o \
	XMenuCreate.o \
	XMenuAddPane.o \
	XMenuAddSel.o \
	XMenuInsPane.o \
	XMenuInsSel.o \
	XMenuFindPane.o \
	XMenuFindSel.o \
	XMenuChgPane.o \
	XMenuChgSel.o \
	XMenuSetPane.o \
	XMenuSetSel.o \
	XMenuRecomp.o \
	XMenuEvHand.o \
	XMenuLocate.o \
	XMenuSetFrz.o \
	XMenuSetAEQ.o \
	XMenuActivate.o \
	XMenuPost.o \
	XMenuDelPane.o \
	XMenuDelSel.o \
	XMenuDestroy.o \
	XMenuError.o

all: lib
sys: lib tests
lib: libXMenu.a libXMenu_p.a

#
# Library construction
#

$(OBJS): XMenu.h XMenuInternal.h

XMenuCreate.o:	../bitmaps/dimple1.bitmap \
		../bitmaps/dimple3.bitmap \
		../bitmaps/gray1.bitmap \
		../bitmaps/gray3.bitmap \
		../bitmaps/cross_weave.bitmap \
		../cursors/left_ptr.cursor \
		../cursors/left_ptr_mask.cursor\
		../cursors/right_ptr.cursor\
		../cursors/right_ptr_mask.cursor\
		../cursors/center_ptr.cursor\
		../cursors/center_ptr_mask.cursor

libXMenu.a: $(OBJS)
	ar cr libXMenu.a $(OBJS)
	@ranlib libXMenu.a

libXMenu_p.a: $(OBJS)
	cd profiled; ar cr ../libXMenu_p.a $(OBJS)
	@ranlib libXMenu_p.a

#
# Library tests
#

tests: libXMenu.a libXMenu_p.a
	cd tests; make

#
# Maintenance
#

install: all
	cp libXMenu.a ${DESTDIR}/usr/lib
	@ranlib ${DESTDIR}/usr/lib/libXMenu.a
	cp libXMenu_p.a ${DESTDIR}/usr/lib
	@ranlib ${DESTDIR}/usr/lib/libXMenu_p.a

lint:
	lint -CX *.c

ci:
	ci -u *.[ch] Makefile
	
co:
	co -l *.[ch] Makefile

consistent:
	ci -l -m"Consistency check in." *.[ch] Makefile
	cd tests; make consistent

touch:
	touch *.o profiled/*.o
	touch *.a

igrind:
	igrind *.[ch] Makefile
	cd tests; make igrind
	
clean: 
	rm -f *.o *.a core *.[ch].[0-9]* *.bak *~ #*
	rm -f profiled/*.o
	cd tests; make clean

include: XMenu.h
	install -c -m 0644 XMenu.h ${DESTDIR}/usr/include/X

#####     End of makefile     #####
