#
# 	$Source: /u1/X/xterm/RCS/Makefile,v $
#	$Header: Makefile,v 10.2 86/12/01 17:52:22 swick Rel $
#

#
#	Makefile for X window system terminal emulator.
#	@(#)Makefile       X10/6.6B 12/26/86
#

DESTDIR=
# We put xterm in /etc so you can run on partial boot.  A link is put
# in CONFDIR so normal search paths will find xterm.
# 
# For the 4.3 distribution, the executable is put in /usr/new instead.
#
CONFDIR= /usr/new
INCLUDES= -I../include
LIBS= -lX -ltermcap
#
# The option KEYBD may be included if the keyboard mods have been done to
# XKeyBind.c in libX.a.
cflags = -O -DMODEMENU ${INCLUDES} -DUTMP -DCHANGEFONT -DALLOWUNSHIFTEDSELECTION
#
# NOWINDOWMENU	disables the window manager menu (right button)
# CHANGEFONT	allows dynamic font changes via escape sequence
#cflags = -O -DMODEMENU ${INCLUDES} -DUTMP -DNOWINDOWMENU -DCHANGEFONT
CFLAGS = -R ${cflags}
SOURCE = Makefile data.h error.h menu.h ptyx.h scrollbar.h VTparse.h \
		Tekparse.h button.c charproc.c cursor.c data.c input.c \
		main.c menu.c misc.c screen.c scrollbar.c tabs.c \
		Tekparsetable.c Tekproc.c util.c VTparsetable.c

.SUFFIXES: .o .h .c

OBJS =	main.o input.o charproc.o cursor.o util.o tabs.o \
	screen.o scrollbar.o button.o Tekproc.o misc.o \
	VTparsetable.o Tekparsetable.o data.o menu.o

all: xterm resize

xterm: $(OBJS)
	$(CC) $(CFLAGS) -o xterm $(OBJS) $(LIBS)

button.o: data.h error.h menu.h ptyx.h scrollbar.h

charproc.o: VTparse.h error.h data.h menu.h ptyx.h scrollbar.h

cursor.o: ptyx.h

data.o: data.c ptyx.h scrollbar.h
	$(CC) $(cflags) -c data.c

input.o: ptyx.h

main.o: data.h error.h main.h ptyx.h scrollbar.h

menu.o: menu.h

misc.o: error.h ptyx.h scrollbar.h gray.ic hilite.ic icon.ic wait.ic waitmask.ic

screen.o: error.h ptyx.h scrollbar.h

scrollbar.o: error.h ptyx.h scrollbar.h button.ic dark.ic light.ic upline.ic \
	downline.ic uppage.ic downpage.ic top.ic bottom.ic saveoff.ic saveon.ic

tabs.o: ptyx.h

Tekparsetable.o: Tekparse.h

Tekproc.o: Tekparse.h error.h data.h menu.h ptyx.h scrollbar.h

VTparsetable.o: VTparse.h

util.o: ptyx.h scrollbar.h

resize: resize.o
	$(CC) $(cflags) -o resize resize.o -lc -ltermcap

resize.o: resize.c
	$(CC) $(cflags) -c resize.c

install: all
#	install -m 4755 xterm ${DESTDIR}/etc
	install -m 4755 xterm ${DESTDIR}${CONFDIR}
#	rm -f ${DESTDIR}${CONFDIR}/xterm
#	ln -s /etc/xterm ${DESTDIR}${CONFDIR}/xterm
	install resize ${DESTDIR}${CONFDIR}

clean:
	rm -f xterm resize *.o a.out core errs gmon.out *.bak *~

print:
	lpr -Pln ${SOURCE}
