#
#	$Header: Makefile,v 10.4 86/11/19 16:44:22 jg Rel $
#

#/*
# *			COPYRIGHT 1985, 1986
# *		   DIGITAL EQUIPMENT CORPORATION
# *		       MAYNARD, MASSACHUSETTS
# *			ALL RIGHTS RESERVED.
# *
# * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
# * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
# * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITIBILITY OF THIS SOFTWARE FOR
# * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
# *
# * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
# * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
# * SET FORTH ABOVE.
# *
# *
# * Permission to use, copy, modify, and distribute this software and its
# * documentation for any purpose and without fee is hereby granted, provided
# * that the above copyright notice appear in all copies and that both that
# * copyright notice and this permission notice appear in supporting documentation,
# * and that the name of Digital Equipment Corporation not be used in advertising
# * or publicity pertaining to distribution of the software without specific, 
# * written prior permission.
# *
# */

#/*
#* MODIFICATION HISTORY
#*
#* 000 -- M. Gancarz, DEC Ultrix Engineering Group
#*/
#
# $Header: Makefile,v 10.4 86/11/19 16:44:22 jg Rel $
#

# SCCSID="@(#)Makefile	3.7	1/23/86"
#
#	Makefile for 'uwm' window manager (client of X windowing system)
#
#

DESTDIR=
CONFDIR= /usr/new
UWMDIR= /usr/new/lib/X/uwm
INCLUDES= -I../include
CURSORDIR= ../cursors
CFLAGS= -O $(INCLUDES) -I$(CURSORDIR)
YFLAGS= -d
LLIB= -ll
XLIB= ../Xlib/libX.a

SRCS=	gram.y lex.l globals.c Beep.c Bindings.c CircleDown.c CircleUp.c\
	Focus.c GetButton.c Iconify.c Lower.c Menu.c Move.c\
	MoveOpaque.c NewIconify.c Pause.c Push.c Error.c Raise.c Refresh.c\
	Resize.c Restart.c StoreBox.c StoreGridBox.c StoreCursors.c StoreZap.c\
	XError.c uwm.c

OBJS=	gram.o lex.o globals.o Beep.o Bindings.o CircleDown.o CircleUp.o\
	Focus.o GetButton.o Iconify.o Lower.o Menu.o Move.o\
	MoveOpaque.o NewIconify.o Pause.o Push.o Error.o Raise.o Refresh.o\
	Resize.o Restart.o StoreBox.o StoreGridBox.o StoreCursors.o StoreZap.o\
	XError.o uwm.o

all:	uwm

uwm:	$(OBJS)
	$(CC) ${CFLAGS} $(OBJS) $(LLIB) ${XLIB} -o uwm

install: uwm default.uwmrc menuless.uwmrc mono.uwmrc ${DESTDIR}${UWMDIR}
	install -c -s -m 755 uwm ${DESTDIR}${CONFDIR}
	install -c -m 644 default.uwmrc ${DESTDIR}${UWMDIR}/default.uwmrc
	install -c -m 644 menuless.uwmrc ${DESTDIR}${UWMDIR}/menuless.uwmrc
	install -c -m 644 mono.uwmrc ${DESTDIR}${UWMDIR}/mono.uwmrc
	install -c -m 644 jg.uwmrc ${DESTDIR}${UWMDIR}/jg.uwmrc

${DESTDIR}${UWMDIR}:
	mkdir ${DESTDIR}${UWMDIR}
	chmod 755 ${DESTDIR}${UWMDIR}

$(OBJS): uwm.h

lex.o: uwm.h lex.l

StoreCursors.o: uwm.h\
	$(CURSORDIR)/arrow_cross.cursor\
	$(CURSORDIR)/arrow_cross_mask.cursor\
	$(CURSORDIR)/xterm.cursor\
	$(CURSORDIR)/xterm_mask.cursor\
	$(CURSORDIR)/icon.cursor\
	$(CURSORDIR)/icon_mask.cursor\
	menu.cursor\
	menu_mask.cursor\
	leftbutton.cursor\
	middlebutton.cursor\
	rightbutton.cursor\
	button_mask.cursor

$(XLIB):
	@echo "Cannot make the Ultrix Window Manager without a valid X library."

clean:
	-rm -f *.o lex.yy.c core y.output y.tab.h uwm *.bak *.sav tmp* \#* *~
