#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	3.9 (Berkeley) %G%
#
CFLAGS=	-O -R
LIBC=	/lib/libc.a
SRCS=	char.c cmd.c cmd1.c cmd2.c cmd3.c cmd4.c cmd5.c cmd6.c cmd7.c \
	context.c error.c lcmd.c lcmd1.c lcmd2.c main.c mloop.c parser1.c \
	parser2.c parser3.c parser4.c parser5.c scanner.c startup.c string.c \
	ttf100.c ttgeneric.c tth19.c tth29.c ttinit.c ttoutput.c tttermcap.c \
	tttvi925.c ttwyse60.c ttwyse75.c ttzentec.c \
	var.c win.c wwadd.c wwalloc.c wwbox.c wwchild.c \
	wwclose.c wwclreol.c wwclreos.c wwcursor.c wwdata.c wwdelchar.c \
	wwdelete.c wwdelline.c wwdump.c wwend.c wwenviron.c wwerror.c \
	wwflush.c wwframe.c wwgets.c wwinit.c wwinschar.c wwinsline.c \
	wwiomux.c wwlabel.c wwmisc.c wwmove.c wwopen.c wwprintf.c wwpty.c \
	wwputc.c wwputs.c wwredraw.c wwredrawwin.c wwrint.c wwscroll.c \
	wwsize.c wwspawn.c wwsuspend.c wwtty.c wwunframe.c wwupdate.c \
	wwwrite.c
OBJS=	char.o cmd.o cmd1.o cmd2.o cmd3.o cmd4.o cmd5.o cmd6.o cmd7.o \
	context.o error.o lcmd.o lcmd1.o lcmd2.o main.o mloop.o parser1.o \
	parser2.o parser3.o parser4.o parser5.o scanner.o startup.o string.o \
	ttf100.o ttgeneric.o tth19.o tth29.o ttinit.o ttoutput.o tttermcap.o \
	tttvi925.o ttwyse60.o ttwyse75.o ttzentec.o \
	var.o win.o wwadd.o wwalloc.o wwbox.o wwchild.o \
	wwclose.o wwclreol.o wwclreos.o wwcursor.o wwdata.o wwdelchar.o \
	wwdelete.o wwdelline.o wwdump.o wwend.o wwenviron.o wwerror.o \
	wwflush.o wwframe.o wwgets.o wwinit.o wwinschar.o wwinsline.o \
	wwiomux.o wwlabel.o wwmisc.o wwmove.o wwopen.o wwprintf.o wwpty.o \
	wwputc.o wwputs.o wwredraw.o wwredrawwin.o wwrint.o wwscroll.o \
	wwsize.o wwspawn.o wwsuspend.o wwtty.o wwunframe.o wwupdate.o \
	wwwrite.o

all: window

window: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} -ltermcap

clean: FRC
	rm -f ${OBJS} core window make.out tags

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 window ${DESTDIR}/usr/ucb/window
#	install -c -o bin -g bin -m 755 windowrc ${DESTDIR}/usr/local/lib/windowrc

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

char.o: char.c char.h
cmd.o: cmd.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h char.h
cmd1.o: cmd1.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd1.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd1.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd1.o: char.h
cmd2.o: cmd2.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd2.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd2.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd3.o: cmd3.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd3.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd3.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd3.o: string.h
cmd4.o: cmd4.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd4.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd4.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd5.o: cmd5.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd5.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd5.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd6.o: cmd6.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd6.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd6.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd6.o: string.h char.h
cmd7.o: cmd7.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd7.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd7.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
context.o: context.c /usr/include/stdio.h value.h string.h context.h
context.o: /usr/include/stdio.h
error.o: error.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
error.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
error.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
error.o: value.h context.h /usr/include/stdio.h char.h
lcmd.o: lcmd.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
lcmd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
lcmd.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
lcmd.o: value.h lcmd.h
lcmd1.o: lcmd1.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
lcmd1.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
lcmd1.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
lcmd1.o: string.h value.h lcmd.h var.h
lcmd2.o: lcmd2.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
lcmd2.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
lcmd2.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
lcmd2.o: string.h value.h var.h lcmd.h /usr/include/sys/resource.h alias.h
main.o: main.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
main.o: /usr/include/sys/signal.h /usr/include/machine/trap.h
main.o: /usr/include/stdio.h string.h char.h local.h
mloop.o: mloop.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
mloop.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mloop.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
parser1.o: parser1.c parser.h /usr/include/stdio.h value.h context.h
parser1.o: /usr/include/stdio.h token.h string.h
parser2.o: parser2.c parser.h /usr/include/stdio.h value.h context.h
parser2.o: /usr/include/stdio.h token.h string.h var.h lcmd.h alias.h
parser3.o: parser3.c parser.h /usr/include/stdio.h value.h context.h
parser3.o: /usr/include/stdio.h token.h string.h
parser4.o: parser4.c parser.h /usr/include/stdio.h value.h context.h
parser4.o: /usr/include/stdio.h token.h string.h
parser5.o: parser5.c parser.h /usr/include/stdio.h value.h context.h
parser5.o: /usr/include/stdio.h token.h string.h var.h
scanner.o: scanner.c /usr/include/stdio.h value.h token.h context.h
scanner.o: /usr/include/stdio.h string.h
startup.o: startup.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
startup.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
startup.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
startup.o: value.h var.h char.h local.h
string.o: string.c string.h
ttf100.o: ttf100.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttf100.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttf100.o: /usr/include/setjmp.h tt.h
ttgeneric.o: ttgeneric.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttgeneric.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttgeneric.o: /usr/include/setjmp.h tt.h
tth19.o: tth19.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
tth19.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tth19.o: /usr/include/setjmp.h tt.h
tth29.o: tth29.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
tth29.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tth29.o: /usr/include/setjmp.h tt.h
ttinit.o: ttinit.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttinit.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttinit.o: /usr/include/setjmp.h tt.h
ttoutput.o: ttoutput.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttoutput.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttoutput.o: /usr/include/setjmp.h tt.h /usr/include/sys/errno.h
tttermcap.o: tttermcap.c tt.h
tttvi925.o: tttvi925.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
tttvi925.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tttvi925.o: /usr/include/setjmp.h tt.h
ttwyse60.o: ttwyse60.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttwyse60.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttwyse60.o: /usr/include/setjmp.h tt.h
ttwyse75.o: ttwyse75.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttwyse75.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttwyse75.o: /usr/include/setjmp.h tt.h
ttzentec.o: ttzentec.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttzentec.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttzentec.o: /usr/include/setjmp.h tt.h
var.o: var.c value.h var.h string.h
win.o: win.c defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
win.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
win.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h char.h
wwadd.o: wwadd.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwadd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwadd.o: /usr/include/setjmp.h
wwalloc.o: wwalloc.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwalloc.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwalloc.o: /usr/include/setjmp.h
wwbox.o: wwbox.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwbox.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwbox.o: /usr/include/setjmp.h tt.h
wwchild.o: wwchild.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwchild.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwchild.o: /usr/include/setjmp.h /usr/include/sys/types.h
wwchild.o: /usr/include/sys/wait.h /usr/include/machine/endian.h
wwclose.o: wwclose.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwclose.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwclose.o: /usr/include/setjmp.h /usr/include/signal.h
wwclose.o: /usr/include/machine/trap.h
wwclreol.o: wwclreol.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwclreol.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwclreol.o: /usr/include/setjmp.h tt.h
wwclreos.o: wwclreos.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwclreos.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwclreos.o: /usr/include/setjmp.h tt.h
wwcursor.o: wwcursor.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwcursor.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwcursor.o: /usr/include/setjmp.h
wwdata.o: wwdata.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwdata.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwdata.o: /usr/include/setjmp.h
wwdelchar.o: wwdelchar.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwdelchar.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwdelchar.o: /usr/include/setjmp.h tt.h
wwdelete.o: wwdelete.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwdelete.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwdelete.o: /usr/include/setjmp.h
wwdelline.o: wwdelline.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwdelline.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwdelline.o: /usr/include/setjmp.h
wwdump.o: wwdump.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwdump.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwdump.o: /usr/include/setjmp.h tt.h
wwend.o: wwend.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwend.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwend.o: /usr/include/setjmp.h tt.h
wwenviron.o: wwenviron.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwenviron.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwenviron.o: /usr/include/setjmp.h /usr/include/sys/signal.h
wwenviron.o: /usr/include/machine/trap.h
wwerror.o: wwerror.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwerror.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwerror.o: /usr/include/setjmp.h
wwflush.o: wwflush.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwflush.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwflush.o: /usr/include/setjmp.h tt.h
wwframe.o: wwframe.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwframe.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwframe.o: /usr/include/setjmp.h tt.h
wwgets.o: wwgets.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwgets.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwgets.o: /usr/include/setjmp.h char.h
wwinit.o: wwinit.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwinit.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwinit.o: /usr/include/setjmp.h tt.h /usr/include/sys/signal.h
wwinit.o: /usr/include/machine/trap.h /usr/include/fcntl.h char.h
wwinschar.o: wwinschar.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwinschar.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwinschar.o: /usr/include/setjmp.h tt.h
wwinsline.o: wwinsline.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwinsline.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwinsline.o: /usr/include/setjmp.h
wwiomux.o: wwiomux.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwiomux.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwiomux.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
wwiomux.o: /usr/include/sys/types.h
wwlabel.o: wwlabel.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwlabel.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwlabel.o: /usr/include/setjmp.h char.h
wwmisc.o: wwmisc.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwmisc.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwmisc.o: /usr/include/setjmp.h tt.h char.h
wwmove.o: wwmove.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwmove.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwmove.o: /usr/include/setjmp.h
wwopen.o: wwopen.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwopen.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwopen.o: /usr/include/setjmp.h /usr/include/sys/types.h
wwopen.o: /usr/include/sys/socket.h
wwprintf.o: wwprintf.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwprintf.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwprintf.o: /usr/include/setjmp.h /usr/include/varargs.h /usr/include/stdio.h
wwpty.o: wwpty.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwpty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwpty.o: /usr/include/setjmp.h
wwputc.o: wwputc.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwputc.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwputc.o: /usr/include/setjmp.h
wwputs.o: wwputs.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwputs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwputs.o: /usr/include/setjmp.h
wwredraw.o: wwredraw.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwredraw.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwredraw.o: /usr/include/setjmp.h tt.h
wwredrawwin.o: wwredrawwin.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwredrawwin.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwredrawwin.o: /usr/include/setjmp.h
wwrint.o: wwrint.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwrint.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwrint.o: /usr/include/setjmp.h /usr/include/fcntl.h /usr/include/sys/signal.h
wwrint.o: /usr/include/machine/trap.h
wwscroll.o: wwscroll.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwscroll.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwscroll.o: /usr/include/setjmp.h tt.h
wwsize.o: wwsize.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwsize.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwsize.o: /usr/include/setjmp.h
wwspawn.o: wwspawn.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwspawn.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwspawn.o: /usr/include/setjmp.h /usr/include/sys/signal.h
wwspawn.o: /usr/include/machine/trap.h
wwsuspend.o: wwsuspend.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwsuspend.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwsuspend.o: /usr/include/setjmp.h tt.h /usr/include/sys/signal.h
wwsuspend.o: /usr/include/machine/trap.h
wwtty.o: wwtty.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwtty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwtty.o: /usr/include/setjmp.h /usr/include/fcntl.h
wwunframe.o: wwunframe.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwunframe.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwunframe.o: /usr/include/setjmp.h
wwupdate.o: wwupdate.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwupdate.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwupdate.o: /usr/include/setjmp.h tt.h
wwwrite.o: wwwrite.c ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwwrite.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwwrite.o: /usr/include/setjmp.h tt.h char.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
