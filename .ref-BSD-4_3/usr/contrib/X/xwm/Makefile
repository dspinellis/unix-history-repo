# Copyright	Massachusetts Institute of Technology	1985
#
#	Makefile - X Window System window manager Makefile
#
#

DESTDIR =
CONFDIR = /usr/new
INCLUDES = -I../include

CFLAGS = -O $(INCLUDES)
XH = ../Xlib/Xlib.h

.SUFFIXES: .o .h .c

XLIB = ../Xlib/libX.a

SRCS = globals.c main.c StoreCursors.c Change.c GetButton.c \
	LowerIconify.c Move.c StoreZap.c StoreBox.c \
	StoreGridBox.c Error.c XError.c

OBJS = globals.o main.o StoreCursors.o Change.o GetButton.o \
	LowerIconify.o Move.o StoreZap.o StoreBox.o \
	StoreGridBox.o Error.o XError.o

all: xwm

$(OBJS): xwm.h $(XH)

StoreCursors.o: ../cursors/arrow_cross.cursor \
	../cursors/arrow_cross_mask.cursor \
	../cursors/ll_angle.cursor ../cursors/ll_angle_mask.cursor \
	../cursors/ul_angle.cursor ../cursors/ul_angle_mask.cursor \
	../cursors/lr_angle.cursor ../cursors/lr_angle_mask.cursor \
	../cursors/ur_angle.cursor ../cursors/ur_angle_mask.cursor \
	../cursors/top_tee.cursor ../cursors/top_tee_mask.cursor \
	../cursors/left_tee.cursor ../cursors/left_tee_mask.cursor \
	../cursors/bottom_tee.cursor ../cursors/bottom_tee_mask.cursor \
	../cursors/right_tee.cursor ../cursors/right_tee_mask.cursor \
	../cursors/dot.cursor ../cursors/dot_mask.cursor \
	../cursors/circle.cursor ../cursors/circle_mask.cursor \
	../cursors/tcross.cursor ../cursors/tcross_mask.cursor \
	../cursors/icon.cursor ../cursors/icon_mask.cursor

xwm: $(OBJS) $(XLIB)
	$(CC) $(CFLAGS) -o xwm $(OBJS) $(XLIB)

link:
	$(CC) $(CFLAGS) -o xwm $(OBJS) $(XLIB)

install: all
	install xwm $(DESTDIR)$(CONFDIR)

clean: 
	rm -f xwm *.o core tmp *.[ch].[0-9]* *.bak *~

igrind:
	igrind $(SRCS)
	
	
