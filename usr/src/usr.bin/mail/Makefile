#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
# Berkeley Mail
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	aux.c cmd1.c cmd2.c cmd3.c cmdtab.c collect.c config.c edit.c fio.c \
	getname.c head.c v7.local.c lex.c list.c main.c names.c optim.c \
	popen.c quit.c send.c strings.c temp.c tty.c vars.c version.c
OBJS=	version.o aux.o cmd1.o cmd2.o cmd3.o cmdtab.o collect.o config.o \
	edit.o fio.o getname.o head.o v7.local.o lex.o list.o main.o \
	names.o optim.o popen.o quit.o send.o strings.o temp.o tty.o \
	vars.o str.o flsbuf.o

#
# Special massaging of C files for sharing of strings
#
.c.o:
	${CC} -E ${CFLAGS} $*.c | xstr -c -
	${CC} -c -o $*.o ${CFLAGS} x.c 
	@rm -f x.c

all: Mail fmt

Mail:	${OBJS} ${LIBC}
	${CC} -o Mail ${OBJS} ${LIBES}

fmt: fmt.o xhead.o ${LIBC}
	${CC} ${CFLAGS} fmt.o xhead.o -o $@

version.o: version.c
	${CC} -c -R version.c

xhead.c:
	ln -s head.c xhead.c

xhead.o:
	${CC} -c ${CFLAGS} xhead.c

fmt.o:
	${CC} -c ${CFLAGS} fmt.c

str.o: strings
	xstr
	${CC} -R -c -o str.o xs.c
	rm -f xs.c

cmdtab.o: cmdtab.c
	${CC} -R -c ${CFLAGS} cmdtab.c

clean: FRC
	rm -f *.o Mail a.out fmt x.c xs.c tags core xhead.c
	cp /dev/null strings

depend: FRC
	mkdep ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 Mail ${DESTDIR}/usr/ucb/Mail
	cp misc/Mail.* ${DESTDIR}/usr/lib
	install -s -o bin -g bin -m 755 fmt ${DESTDIR}/usr/ucb/fmt
	rm -f ${DESTDIR}/usr/ucb/mail
	ln ${DESTDIR}/usr/ucb/Mail ${DESTDIR}/usr/ucb/mail

lint: FRC
	lint ${CFLAGS} ${SRCS}
	lint ${CFLAGS} xhead.o fmt.o

tags: xhead.c FRC
	ctags ${SRCS}
	ctags -a xhead.c fmt.c
	sort -o tags tags

FRC:
# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

aux.o: aux.c
aux.o:rcv.h
aux.o:def.h
aux.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
aux.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
aux.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
aux.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
aux.o: /usr/include/ctype.h /usr/include/strings.h
aux.o:local.h
aux.o:v7.local.h
aux.o:glob.h
aux.o: /usr/include/setjmp.h /usr/include/sys/stat.h
cmd1.o: cmd1.c
cmd1.o:rcv.h
cmd1.o:def.h
cmd1.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
cmd1.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
cmd1.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd1.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd1.o: /usr/include/ctype.h /usr/include/strings.h
cmd1.o:local.h
cmd1.o:v7.local.h
cmd1.o:glob.h
cmd1.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sys/wait.h
cmd2.o: cmd2.c
cmd2.o:rcv.h
cmd2.o:def.h
cmd2.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
cmd2.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
cmd2.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd2.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd2.o: /usr/include/ctype.h /usr/include/strings.h
cmd2.o:local.h
cmd2.o:v7.local.h
cmd2.o:glob.h
cmd2.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sys/wait.h
cmd3.o: cmd3.c
cmd3.o:rcv.h
cmd3.o:def.h
cmd3.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
cmd3.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
cmd3.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmd3.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmd3.o: /usr/include/ctype.h /usr/include/strings.h
cmd3.o:local.h
cmd3.o:v7.local.h
cmd3.o:glob.h
cmd3.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sys/wait.h
cmdtab.o: cmdtab.c
cmdtab.o:def.h
cmdtab.o: /usr/include/sys/param.h /usr/include/sys/types.h
cmdtab.o: /usr/include/signal.h /usr/include/machine/machparam.h
cmdtab.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
cmdtab.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
cmdtab.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
cmdtab.o:local.h
cmdtab.o:v7.local.h
collect.o: collect.c
collect.o:rcv.h
collect.o:def.h
collect.o: /usr/include/sys/param.h /usr/include/sys/types.h
collect.o: /usr/include/signal.h /usr/include/machine/machparam.h
collect.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
collect.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
collect.o: /usr/include/sys/ttydev.h /usr/include/ctype.h
collect.o: /usr/include/strings.h
collect.o:local.h
collect.o:v7.local.h
collect.o:glob.h
collect.o: /usr/include/setjmp.h /usr/include/sys/stat.h
collect.o: /usr/include/sys/wait.h
config.o: config.c
config.o:configdefs.h
edit.o: edit.c
edit.o:rcv.h
edit.o:def.h
edit.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
edit.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
edit.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
edit.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
edit.o: /usr/include/ctype.h /usr/include/strings.h
edit.o:local.h
edit.o:v7.local.h
edit.o:glob.h
edit.o: /usr/include/setjmp.h /usr/include/stdio.h /usr/include/sys/stat.h
edit.o: /usr/include/sys/wait.h
fio.o: fio.c
fio.o:rcv.h
fio.o:def.h
fio.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
fio.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
fio.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
fio.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
fio.o: /usr/include/ctype.h /usr/include/strings.h
fio.o:local.h
fio.o:v7.local.h
fio.o:glob.h
fio.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sys/file.h
fio.o: /usr/include/sys/wait.h /usr/include/errno.h
getname.o: getname.c /usr/include/pwd.h
getname.o:rcv.h
getname.o:def.h
getname.o: /usr/include/sys/param.h /usr/include/sys/types.h
getname.o: /usr/include/signal.h /usr/include/machine/machparam.h
getname.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
getname.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
getname.o: /usr/include/sys/ttydev.h /usr/include/ctype.h
getname.o: /usr/include/strings.h
getname.o:local.h
getname.o:v7.local.h
getname.o:glob.h
getname.o: /usr/include/setjmp.h
head.o: head.c
head.o:rcv.h
head.o:def.h
head.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
head.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
head.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
head.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
head.o: /usr/include/ctype.h /usr/include/strings.h
head.o:local.h
head.o:v7.local.h
head.o:glob.h
head.o: /usr/include/setjmp.h
v7.local.o: v7.local.c
v7.local.o:rcv.h
v7.local.o:def.h
v7.local.o: /usr/include/sys/param.h /usr/include/sys/types.h
v7.local.o: /usr/include/signal.h /usr/include/machine/machparam.h
v7.local.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
v7.local.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
v7.local.o: /usr/include/sys/ttydev.h /usr/include/ctype.h
v7.local.o: /usr/include/strings.h
v7.local.o:local.h
v7.local.o:v7.local.h
v7.local.o:glob.h
v7.local.o: /usr/include/setjmp.h
lex.o: lex.c
lex.o:rcv.h
lex.o:def.h
lex.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
lex.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
lex.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
lex.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
lex.o: /usr/include/ctype.h /usr/include/strings.h
lex.o:local.h
lex.o:v7.local.h
lex.o:glob.h
lex.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/errno.h
list.o: list.c
list.o:rcv.h
list.o:def.h
list.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
list.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
list.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
list.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
list.o: /usr/include/ctype.h /usr/include/strings.h
list.o:local.h
list.o:v7.local.h
list.o:glob.h
list.o: /usr/include/setjmp.h /usr/include/ctype.h
main.o: main.c
main.o:rcv.h
main.o:def.h
main.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
main.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
main.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/ctype.h /usr/include/strings.h
main.o:local.h
main.o:v7.local.h
main.o:glob.h
main.o: /usr/include/setjmp.h /usr/include/sys/stat.h
names.o: names.c
names.o:rcv.h
names.o:def.h
names.o: /usr/include/sys/param.h /usr/include/sys/types.h
names.o: /usr/include/signal.h /usr/include/machine/machparam.h
names.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
names.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
names.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
names.o:local.h
names.o:v7.local.h
names.o:glob.h
names.o: /usr/include/setjmp.h /usr/include/sys/wait.h
optim.o: optim.c
optim.o:rcv.h
optim.o:def.h
optim.o: /usr/include/sys/param.h /usr/include/sys/types.h
optim.o: /usr/include/signal.h /usr/include/machine/machparam.h
optim.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
optim.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
optim.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
optim.o:local.h
optim.o:v7.local.h
optim.o:glob.h
optim.o: /usr/include/setjmp.h
optim.o:configdefs.h
popen.o: popen.c /usr/include/stdio.h /usr/include/sys/signal.h
popen.o: /usr/include/sys/types.h /usr/include/sys/wait.h
popen.o: /usr/include/machine/machparam.h /usr/include/errno.h
quit.o: quit.c
quit.o:rcv.h
quit.o:def.h
quit.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
quit.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
quit.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
quit.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
quit.o: /usr/include/ctype.h /usr/include/strings.h
quit.o:local.h
quit.o:v7.local.h
quit.o:glob.h
quit.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sys/file.h
send.o: send.c
send.o:rcv.h
send.o:def.h
send.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
send.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
send.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
send.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
send.o: /usr/include/ctype.h /usr/include/strings.h
send.o:local.h
send.o:v7.local.h
send.o:glob.h
send.o: /usr/include/setjmp.h /usr/include/sys/wait.h /usr/include/sys/stat.h
strings.o: strings.c
strings.o:rcv.h
strings.o:def.h
strings.o: /usr/include/sys/param.h /usr/include/sys/types.h
strings.o: /usr/include/signal.h /usr/include/machine/machparam.h
strings.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
strings.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
strings.o: /usr/include/sys/ttydev.h /usr/include/ctype.h
strings.o: /usr/include/strings.h
strings.o:local.h
strings.o:v7.local.h
strings.o:glob.h
strings.o: /usr/include/setjmp.h
temp.o: temp.c
temp.o:rcv.h
temp.o:def.h
temp.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
temp.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
temp.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
temp.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
temp.o: /usr/include/ctype.h /usr/include/strings.h
temp.o:local.h
temp.o:v7.local.h
temp.o:glob.h
temp.o: /usr/include/setjmp.h
tty.o: tty.c
tty.o:rcv.h
tty.o:def.h
tty.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
tty.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
tty.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
tty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tty.o: /usr/include/ctype.h /usr/include/strings.h
tty.o:local.h
tty.o:v7.local.h
tty.o:glob.h
tty.o: /usr/include/setjmp.h
vars.o: vars.c
vars.o:rcv.h
vars.o:def.h
vars.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
vars.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
vars.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
vars.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
vars.o: /usr/include/ctype.h /usr/include/strings.h
vars.o:local.h
vars.o:v7.local.h
vars.o:glob.h
vars.o: /usr/include/setjmp.h
version.o: version.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
