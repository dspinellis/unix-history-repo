#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	3.1 (Berkeley) %G%
#
#
# Window makefile, update with mkmf (or "make depend")
#
CFLAGS        = -O -R

EXTHDRS	      = /usr/include/fcntl.h \
		/usr/include/setjmp.h \
		/usr/include/sgtty.h \
		/usr/include/signal.h \
		/usr/include/stdio.h \
		/usr/include/sys/errno.h \
		/usr/include/sys/ioctl.h \
		/usr/include/sys/resource.h \
		/usr/include/sys/signal.h \
		/usr/include/sys/socket.h \
		/usr/include/sys/time.h \
		/usr/include/sys/ttychars.h \
		/usr/include/sys/ttydev.h \
		/usr/include/sys/types.h \
		/usr/include/sys/wait.h

HDRS	      = alias.h \
		char.h \
		context.h \
		defs.h \
		lcmd.h \
		local.h \
		parser.h \
		string.h \
		token.h \
		tt.h \
		value.h \
		var.h \
		ww.h

LDFLAGS	      =

LIBS	      = /usr/lib/libtermcap.a

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = char.o \
		cmd.o \
		cmd1.o \
		cmd2.o \
		cmd3.o \
		cmd4.o \
		cmd5.o \
		cmd6.o \
		cmd7.o \
		context.o \
		error.o \
		lcmd.o \
		lcmd1.o \
		lcmd2.o \
		main.o \
		mloop.o \
		parser1.o \
		parser2.o \
		parser3.o \
		parser4.o \
		parser5.o \
		scanner.o \
		startup.o \
		string.o \
		ttf100.o \
		ttgeneric.o \
		tth19.o \
		tth29.o \
		ttinit.o \
		ttoutput.o \
		tttermcap.o \
		tttvi925.o \
		var.o \
		win.o \
		wwadd.o \
		wwalloc.o \
		wwbox.o \
		wwchild.o \
		wwclose.o \
		wwclreol.o \
		wwclreos.o \
		wwcursor.o \
		wwdata.o \
		wwdelchar.o \
		wwdelete.o \
		wwdelline.o \
		wwdump.o \
		wwend.o \
		wwenviron.o \
		wwerror.o \
		wwflush.o \
		wwframe.o \
		wwgets.o \
		wwinit.o \
		wwinschar.o \
		wwinsline.o \
		wwiomux.o \
		wwlabel.o \
		wwmisc.o \
		wwmove.o \
		wwopen.o \
		wwprintf.o \
		wwpty.o \
		wwputc.o \
		wwputs.o \
		wwredraw.o \
		wwredrawwin.o \
		wwrint.o \
		wwscroll.o \
		wwsize.o \
		wwspawn.o \
		wwsuspend.o \
		wwtty.o \
		wwunframe.o \
		wwupdate.o \
		wwwrite.o

PRINT	      = pr

PROGRAM	      = a.out

SRCS	      = char.c \
		cmd.c \
		cmd1.c \
		cmd2.c \
		cmd3.c \
		cmd4.c \
		cmd5.c \
		cmd6.c \
		cmd7.c \
		context.c \
		error.c \
		lcmd.c \
		lcmd1.c \
		lcmd2.c \
		main.c \
		mloop.c \
		parser1.c \
		parser2.c \
		parser3.c \
		parser4.c \
		parser5.c \
		scanner.c \
		startup.c \
		string.c \
		ttf100.c \
		ttgeneric.c \
		tth19.c \
		tth29.c \
		ttinit.c \
		ttoutput.c \
		tttermcap.c \
		tttvi925.c \
		var.c \
		win.c \
		wwadd.c \
		wwalloc.c \
		wwbox.c \
		wwchild.c \
		wwclose.c \
		wwclreol.c \
		wwclreos.c \
		wwcursor.c \
		wwdata.c \
		wwdelchar.c \
		wwdelete.c \
		wwdelline.c \
		wwdump.c \
		wwend.c \
		wwenviron.c \
		wwerror.c \
		wwflush.c \
		wwframe.c \
		wwgets.c \
		wwinit.c \
		wwinschar.c \
		wwinsline.c \
		wwiomux.c \
		wwlabel.c \
		wwmisc.c \
		wwmove.c \
		wwopen.c \
		wwprintf.c \
		wwpty.c \
		wwputc.c \
		wwputs.c \
		wwredraw.c \
		wwredrawwin.c \
		wwrint.c \
		wwscroll.c \
		wwsize.c \
		wwspawn.c \
		wwsuspend.c \
		wwtty.c \
		wwunframe.c \
		wwupdate.c \
		wwwrite.c

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS) $(LIBS)
		@echo -n "Loading $(PROGRAM) ... "
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f $(OBJS) make.out

depend:;	@mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM)

install:	$(PROGRAM) windowrc
		install -s a.out $(DESTDIR)/usr/ucb/window
#		install -c windowrc $(DESTDIR)/usr/local/lib

print:;		@$(PRINT) $(MAKEFILE) README $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

lint:;		@lint -z $(SRCS)

wc:;		@wc $(SRCS) $(HDRS)

###
char.o: char.h
cmd.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h char.h
cmd1.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h char.h
cmd2.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h
cmd3.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h string.h
cmd4.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h
cmd5.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h
cmd6.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h string.h char.h
cmd7.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h
context.o: /usr/include/stdio.h value.h string.h context.h
error.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h value.h context.h \
	/usr/include/stdio.h char.h
lcmd.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h value.h lcmd.h
lcmd1.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h string.h value.h lcmd.h \
	var.h
lcmd2.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h string.h value.h var.h \
	lcmd.h /usr/include/sys/resource.h alias.h
main.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h \
	/usr/include/sys/signal.h /usr/include/stdio.h string.h char.h \
	local.h
mloop.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h
parser1.o: parser.h /usr/include/stdio.h value.h context.h token.h string.h
parser2.o: parser.h /usr/include/stdio.h value.h context.h token.h string.h \
	var.h lcmd.h alias.h
parser3.o: parser.h /usr/include/stdio.h value.h context.h token.h string.h
parser4.o: parser.h /usr/include/stdio.h value.h context.h token.h string.h
parser5.o: parser.h /usr/include/stdio.h value.h context.h token.h string.h \
	var.h
scanner.o: /usr/include/stdio.h value.h token.h context.h string.h
startup.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h value.h var.h char.h \
	local.h
string.o: string.h
ttf100.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
ttgeneric.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
tth19.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
tth29.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
ttinit.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
ttoutput.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h /usr/include/sys/errno.h
tttermcap.o: tt.h
tttvi925.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
var.o: value.h var.h string.h
win.o: defs.h ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h char.h
wwadd.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwalloc.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwbox.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
wwchild.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/wait.h
wwclose.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/signal.h
wwclreol.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
wwclreos.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwcursor.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwdata.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwdelchar.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
wwdelete.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwdelline.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwdump.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
wwend.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
wwenviron.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/signal.h
wwerror.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwflush.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
wwframe.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
wwgets.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h char.h
wwinit.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h /usr/include/sys/signal.h \
	/usr/include/fcntl.h char.h
wwinschar.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
wwinsline.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwiomux.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/time.h
wwlabel.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h char.h
wwmisc.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h char.h
wwmove.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwopen.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/types.h \
	/usr/include/sys/socket.h
wwprintf.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/stdio.h
wwpty.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwputc.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwputs.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwredraw.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
wwredrawwin.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwrint.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/fcntl.h /usr/include/sys/signal.h
wwscroll.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
wwsize.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwspawn.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/sys/signal.h
wwsuspend.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h /usr/include/sys/signal.h
wwtty.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h /usr/include/fcntl.h
wwunframe.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h
wwupdate.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h
wwwrite.o: ww.h /usr/include/sgtty.h /usr/include/sys/ioctl.h \
	/usr/include/sys/ttychars.h /usr/include/sys/ttydev.h \
	/usr/include/setjmp.h tt.h char.h
