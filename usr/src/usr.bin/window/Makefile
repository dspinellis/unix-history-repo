#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	3.4	(Berkeley)	%G%
#
CFLAGS=	-O -R
LIBC=	/lib/libc.a
SRCS=	char.c cmd.c cmd1.c cmd2.c cmd3.c cmd4.c cmd5.c cmd6.c cmd7.c \
	context.c error.c lcmd.c lcmd1.c lcmd2.c main.c mloop.c parser1.c \
	parser2.c parser3.c parser4.c parser5.c scanner.c startup.c string.c \
	ttf100.c ttgeneric.c tth19.c tth29.c ttinit.c ttoutput.c tttermcap.c \
	tttvi925.c ttzentec.c var.c win.c wwadd.c wwalloc.c wwbox.c wwchild.c \
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
	tttvi925.o ttzentec.o var.o win.o wwadd.o wwalloc.o wwbox.o wwchild.o \
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
	rm -f ${OBJS} core window

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

char.o: char.c
char.o:char.h
cmd.o: cmd.c
cmd.o:defs.h
cmd.o:ww.h
cmd.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd.o:char.h
cmd1.o: cmd1.c
cmd1.o:defs.h
cmd1.o:ww.h
cmd1.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd1.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd1.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd1.o:char.h
cmd2.o: cmd2.c
cmd2.o:defs.h
cmd2.o:ww.h
cmd2.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd2.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd2.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd3.o: cmd3.c
cmd3.o:defs.h
cmd3.o:ww.h
cmd3.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd3.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd3.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd3.o:string.h
cmd4.o: cmd4.c
cmd4.o:defs.h
cmd4.o:ww.h
cmd4.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd4.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd4.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd5.o: cmd5.c
cmd5.o:defs.h
cmd5.o:ww.h
cmd5.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd5.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd5.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd6.o: cmd6.c
cmd6.o:defs.h
cmd6.o:ww.h
cmd6.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd6.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd6.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
cmd6.o:string.h
cmd6.o:char.h
cmd7.o: cmd7.c
cmd7.o:defs.h
cmd7.o:ww.h
cmd7.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd7.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd7.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
context.o: context.c /usr/include/stdio.h
context.o:value.h
context.o:string.h
context.o:context.h
context.o: /usr/include/stdio.h
error.o: error.c
error.o:defs.h
error.o:ww.h
error.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
error.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
error.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
error.o:value.h
error.o:context.h
error.o: /usr/include/stdio.h
error.o:char.h
lcmd.o: lcmd.c
lcmd.o:defs.h
lcmd.o:ww.h
lcmd.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
lcmd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
lcmd.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
lcmd.o:value.h
lcmd.o:lcmd.h
lcmd1.o: lcmd1.c
lcmd1.o:defs.h
lcmd1.o:ww.h
lcmd1.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
lcmd1.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
lcmd1.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
lcmd1.o:string.h
lcmd1.o:value.h
lcmd1.o:lcmd.h
lcmd1.o:var.h
lcmd2.o: lcmd2.c
lcmd2.o:defs.h
lcmd2.o:ww.h
lcmd2.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
lcmd2.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
lcmd2.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
lcmd2.o:string.h
lcmd2.o:value.h
lcmd2.o:var.h
lcmd2.o:lcmd.h
lcmd2.o: /usr/include/sys/resource.h
lcmd2.o:alias.h
main.o: main.c
main.o:defs.h
main.o:ww.h
main.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
main.o: /usr/include/sys/signal.h /usr/include/stdio.h
main.o:string.h
main.o:char.h
main.o:local.h
mloop.o: mloop.c
mloop.o:defs.h
mloop.o:ww.h
mloop.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
mloop.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mloop.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
parser1.o: parser1.c
parser1.o:parser.h
parser1.o: /usr/include/stdio.h
parser1.o:value.h
parser1.o:context.h
parser1.o: /usr/include/stdio.h
parser1.o:token.h
parser1.o:string.h
parser2.o: parser2.c
parser2.o:parser.h
parser2.o: /usr/include/stdio.h
parser2.o:value.h
parser2.o:context.h
parser2.o: /usr/include/stdio.h
parser2.o:token.h
parser2.o:string.h
parser2.o:var.h
parser2.o:lcmd.h
parser2.o:alias.h
parser3.o: parser3.c
parser3.o:parser.h
parser3.o: /usr/include/stdio.h
parser3.o:value.h
parser3.o:context.h
parser3.o: /usr/include/stdio.h
parser3.o:token.h
parser3.o:string.h
parser4.o: parser4.c
parser4.o:parser.h
parser4.o: /usr/include/stdio.h
parser4.o:value.h
parser4.o:context.h
parser4.o: /usr/include/stdio.h
parser4.o:token.h
parser4.o:string.h
parser5.o: parser5.c
parser5.o:parser.h
parser5.o: /usr/include/stdio.h
parser5.o:value.h
parser5.o:context.h
parser5.o: /usr/include/stdio.h
parser5.o:token.h
parser5.o:string.h
parser5.o:var.h
scanner.o: scanner.c /usr/include/stdio.h
scanner.o:value.h
scanner.o:token.h
scanner.o:context.h
scanner.o: /usr/include/stdio.h
scanner.o:string.h
startup.o: startup.c
startup.o:defs.h
startup.o:ww.h
startup.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
startup.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
startup.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
startup.o:value.h
startup.o:var.h
startup.o:char.h
startup.o:local.h
string.o: string.c
string.o:string.h
ttf100.o: ttf100.c
ttf100.o:ww.h
ttf100.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttf100.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttf100.o: /usr/include/setjmp.h
ttf100.o:tt.h
ttgeneric.o: ttgeneric.c
ttgeneric.o:ww.h
ttgeneric.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttgeneric.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttgeneric.o: /usr/include/setjmp.h
ttgeneric.o:tt.h
tth19.o: tth19.c
tth19.o:ww.h
tth19.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
tth19.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tth19.o: /usr/include/setjmp.h
tth19.o:tt.h
tth29.o: tth29.c
tth29.o:ww.h
tth29.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
tth29.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tth29.o: /usr/include/setjmp.h
tth29.o:tt.h
ttinit.o: ttinit.c
ttinit.o:ww.h
ttinit.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttinit.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttinit.o: /usr/include/setjmp.h
ttinit.o:tt.h
ttoutput.o: ttoutput.c
ttoutput.o:ww.h
ttoutput.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttoutput.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttoutput.o: /usr/include/setjmp.h
ttoutput.o:tt.h
ttoutput.o: /usr/include/sys/errno.h
tttermcap.o: tttermcap.c
tttermcap.o:tt.h
tttvi925.o: tttvi925.c
tttvi925.o:ww.h
tttvi925.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
tttvi925.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tttvi925.o: /usr/include/setjmp.h
tttvi925.o:tt.h
ttzentec.o: ttzentec.c
ttzentec.o:ww.h
ttzentec.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
ttzentec.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ttzentec.o: /usr/include/setjmp.h
ttzentec.o:tt.h
var.o: var.c
var.o:value.h
var.o:var.h
var.o:string.h
win.o: win.c
win.o:defs.h
win.o:ww.h
win.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
win.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
win.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
win.o:char.h
wwadd.o: wwadd.c
wwadd.o:ww.h
wwadd.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwadd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwadd.o: /usr/include/setjmp.h
wwalloc.o: wwalloc.c
wwalloc.o:ww.h
wwalloc.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwalloc.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwalloc.o: /usr/include/setjmp.h
wwbox.o: wwbox.c
wwbox.o:ww.h
wwbox.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwbox.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwbox.o: /usr/include/setjmp.h
wwbox.o:tt.h
wwchild.o: wwchild.c
wwchild.o:ww.h
wwchild.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwchild.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwchild.o: /usr/include/setjmp.h /usr/include/sys/types.h
wwchild.o: /usr/include/sys/wait.h /usr/include/machine/machparam.h
wwclose.o: wwclose.c
wwclose.o:ww.h
wwclose.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwclose.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwclose.o: /usr/include/setjmp.h /usr/include/signal.h
wwclreol.o: wwclreol.c
wwclreol.o:ww.h
wwclreol.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwclreol.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwclreol.o: /usr/include/setjmp.h
wwclreol.o:tt.h
wwclreos.o: wwclreos.c
wwclreos.o:ww.h
wwclreos.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwclreos.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwclreos.o: /usr/include/setjmp.h
wwclreos.o:tt.h
wwcursor.o: wwcursor.c
wwcursor.o:ww.h
wwcursor.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwcursor.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwcursor.o: /usr/include/setjmp.h
wwdata.o: wwdata.c
wwdata.o:ww.h
wwdata.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwdata.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwdata.o: /usr/include/setjmp.h
wwdelchar.o: wwdelchar.c
wwdelchar.o:ww.h
wwdelchar.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwdelchar.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwdelchar.o: /usr/include/setjmp.h
wwdelchar.o:tt.h
wwdelete.o: wwdelete.c
wwdelete.o:ww.h
wwdelete.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwdelete.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwdelete.o: /usr/include/setjmp.h
wwdelline.o: wwdelline.c
wwdelline.o:ww.h
wwdelline.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwdelline.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwdelline.o: /usr/include/setjmp.h
wwdump.o: wwdump.c
wwdump.o:ww.h
wwdump.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwdump.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwdump.o: /usr/include/setjmp.h
wwdump.o:tt.h
wwend.o: wwend.c
wwend.o:ww.h
wwend.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwend.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwend.o: /usr/include/setjmp.h
wwend.o:tt.h
wwenviron.o: wwenviron.c
wwenviron.o:ww.h
wwenviron.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwenviron.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwenviron.o: /usr/include/setjmp.h /usr/include/sys/signal.h
wwerror.o: wwerror.c
wwerror.o:ww.h
wwerror.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwerror.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwerror.o: /usr/include/setjmp.h
wwflush.o: wwflush.c
wwflush.o:ww.h
wwflush.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwflush.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwflush.o: /usr/include/setjmp.h
wwflush.o:tt.h
wwframe.o: wwframe.c
wwframe.o:ww.h
wwframe.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwframe.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwframe.o: /usr/include/setjmp.h
wwframe.o:tt.h
wwgets.o: wwgets.c
wwgets.o:ww.h
wwgets.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwgets.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwgets.o: /usr/include/setjmp.h
wwgets.o:char.h
wwinit.o: wwinit.c
wwinit.o:ww.h
wwinit.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwinit.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwinit.o: /usr/include/setjmp.h
wwinit.o:tt.h
wwinit.o: /usr/include/sys/signal.h /usr/include/fcntl.h
wwinit.o:char.h
wwinschar.o: wwinschar.c
wwinschar.o:ww.h
wwinschar.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwinschar.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwinschar.o: /usr/include/setjmp.h
wwinschar.o:tt.h
wwinsline.o: wwinsline.c
wwinsline.o:ww.h
wwinsline.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwinsline.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwinsline.o: /usr/include/setjmp.h
wwiomux.o: wwiomux.c
wwiomux.o:ww.h
wwiomux.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwiomux.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwiomux.o: /usr/include/setjmp.h /usr/include/sys/time.h /usr/include/time.h
wwiomux.o: /usr/include/sys/types.h
wwlabel.o: wwlabel.c
wwlabel.o:ww.h
wwlabel.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwlabel.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwlabel.o: /usr/include/setjmp.h
wwlabel.o:char.h
wwmisc.o: wwmisc.c
wwmisc.o:ww.h
wwmisc.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwmisc.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwmisc.o: /usr/include/setjmp.h
wwmisc.o:tt.h
wwmisc.o:char.h
wwmove.o: wwmove.c
wwmove.o:ww.h
wwmove.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwmove.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwmove.o: /usr/include/setjmp.h
wwopen.o: wwopen.c
wwopen.o:ww.h
wwopen.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwopen.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwopen.o: /usr/include/setjmp.h /usr/include/sys/types.h
wwopen.o: /usr/include/sys/socket.h
wwprintf.o: wwprintf.c
wwprintf.o:ww.h
wwprintf.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwprintf.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwprintf.o: /usr/include/setjmp.h /usr/include/stdio.h
wwpty.o: wwpty.c
wwpty.o:ww.h
wwpty.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwpty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwpty.o: /usr/include/setjmp.h
wwputc.o: wwputc.c
wwputc.o:ww.h
wwputc.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwputc.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwputc.o: /usr/include/setjmp.h
wwputs.o: wwputs.c
wwputs.o:ww.h
wwputs.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwputs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwputs.o: /usr/include/setjmp.h
wwredraw.o: wwredraw.c
wwredraw.o:ww.h
wwredraw.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwredraw.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwredraw.o: /usr/include/setjmp.h
wwredraw.o:tt.h
wwredrawwin.o: wwredrawwin.c
wwredrawwin.o:ww.h
wwredrawwin.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwredrawwin.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwredrawwin.o: /usr/include/setjmp.h
wwrint.o: wwrint.c
wwrint.o:ww.h
wwrint.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwrint.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwrint.o: /usr/include/setjmp.h /usr/include/fcntl.h /usr/include/sys/signal.h
wwscroll.o: wwscroll.c
wwscroll.o:ww.h
wwscroll.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwscroll.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwscroll.o: /usr/include/setjmp.h
wwscroll.o:tt.h
wwsize.o: wwsize.c
wwsize.o:ww.h
wwsize.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwsize.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwsize.o: /usr/include/setjmp.h
wwspawn.o: wwspawn.c
wwspawn.o:ww.h
wwspawn.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwspawn.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwspawn.o: /usr/include/setjmp.h /usr/include/sys/signal.h
wwsuspend.o: wwsuspend.c
wwsuspend.o:ww.h
wwsuspend.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwsuspend.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwsuspend.o: /usr/include/setjmp.h
wwsuspend.o:tt.h
wwsuspend.o: /usr/include/sys/signal.h
wwtty.o: wwtty.c
wwtty.o:ww.h
wwtty.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwtty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwtty.o: /usr/include/setjmp.h /usr/include/fcntl.h
wwunframe.o: wwunframe.c
wwunframe.o:ww.h
wwunframe.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwunframe.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwunframe.o: /usr/include/setjmp.h
wwupdate.o: wwupdate.c
wwupdate.o:ww.h
wwupdate.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwupdate.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwupdate.o: /usr/include/setjmp.h
wwupdate.o:tt.h
wwwrite.o: wwwrite.c
wwwrite.o:ww.h
wwwrite.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
wwwrite.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
wwwrite.o: /usr/include/setjmp.h
wwwrite.o:tt.h
wwwrite.o:char.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
