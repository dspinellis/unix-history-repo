#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.5 (Berkeley) %G%
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
	rm -f *.o Mail a.out fmt x.c xs.c tags core xhead.c strings

depend: FRC
	mkdep ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 Mail ${DESTDIR}/usr/ucb/Mail
	install -s -o bin -g bin -m 755 fmt ${DESTDIR}/usr/ucb/fmt
	install -c -o bin -g bin -m 444 misc/Mail.* ${DESTDIR}/usr/lib
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

aux.o: aux.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
aux.o: /usr/include/signal.h /usr/include/machine/trap.h
aux.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
aux.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
aux.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
aux.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
aux.o: local.h v7.local.h glob.h /usr/include/setjmp.h /usr/include/sys/stat.h
cmd1.o: cmd1.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
cmd1.o: /usr/include/signal.h /usr/include/machine/trap.h
cmd1.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cmd1.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
cmd1.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
cmd1.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
cmd1.o: local.h v7.local.h glob.h /usr/include/setjmp.h /usr/include/sys/stat.h
cmd1.o: /usr/include/sys/wait.h
cmd2.o: cmd2.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
cmd2.o: /usr/include/signal.h /usr/include/machine/trap.h
cmd2.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cmd2.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
cmd2.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
cmd2.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
cmd2.o: local.h v7.local.h glob.h /usr/include/setjmp.h /usr/include/sys/stat.h
cmd2.o: /usr/include/sys/wait.h
cmd3.o: cmd3.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
cmd3.o: /usr/include/signal.h /usr/include/machine/trap.h
cmd3.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cmd3.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
cmd3.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
cmd3.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
cmd3.o: local.h v7.local.h glob.h /usr/include/setjmp.h /usr/include/sys/stat.h
cmd3.o: /usr/include/sys/wait.h
cmdtab.o: cmdtab.c def.h /usr/include/sys/param.h /usr/include/sys/types.h
cmdtab.o: /usr/include/signal.h /usr/include/machine/trap.h
cmdtab.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cmdtab.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
cmdtab.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
cmdtab.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
cmdtab.o: local.h v7.local.h
collect.o: collect.c rcv.h def.h /usr/include/sys/param.h
collect.o: /usr/include/sys/types.h /usr/include/signal.h
collect.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
collect.o: /usr/include/machine/endian.h /usr/include/sys/signal.h
collect.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
collect.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
collect.o: /usr/include/ctype.h /usr/include/strings.h local.h v7.local.h
collect.o: glob.h /usr/include/setjmp.h /usr/include/sys/stat.h
collect.o: /usr/include/sys/wait.h
config.o: config.c configdefs.h
edit.o: edit.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
edit.o: /usr/include/signal.h /usr/include/machine/trap.h
edit.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
edit.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
edit.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
edit.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
edit.o: local.h v7.local.h glob.h /usr/include/setjmp.h /usr/include/stdio.h
edit.o: /usr/include/sys/stat.h /usr/include/sys/wait.h
fio.o: fio.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
fio.o: /usr/include/signal.h /usr/include/machine/trap.h
fio.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
fio.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
fio.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
fio.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
fio.o: local.h v7.local.h glob.h /usr/include/setjmp.h /usr/include/sys/stat.h
fio.o: /usr/include/sys/file.h /usr/include/sys/wait.h /usr/include/errno.h
getname.o: getname.c /usr/include/pwd.h rcv.h def.h /usr/include/sys/param.h
getname.o: /usr/include/sys/types.h /usr/include/signal.h
getname.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
getname.o: /usr/include/machine/endian.h /usr/include/sys/signal.h
getname.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
getname.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
getname.o: /usr/include/ctype.h /usr/include/strings.h local.h v7.local.h
getname.o: glob.h /usr/include/setjmp.h
head.o: head.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
head.o: /usr/include/signal.h /usr/include/machine/trap.h
head.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
head.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
head.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
head.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
head.o: local.h v7.local.h glob.h /usr/include/setjmp.h
v7.local.o: v7.local.c rcv.h def.h /usr/include/sys/param.h
v7.local.o: /usr/include/sys/types.h /usr/include/signal.h
v7.local.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
v7.local.o: /usr/include/machine/endian.h /usr/include/sys/signal.h
v7.local.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
v7.local.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
v7.local.o: /usr/include/ctype.h /usr/include/strings.h local.h v7.local.h
v7.local.o: glob.h /usr/include/setjmp.h
lex.o: lex.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
lex.o: /usr/include/signal.h /usr/include/machine/trap.h
lex.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
lex.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
lex.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
lex.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
lex.o: local.h v7.local.h glob.h /usr/include/setjmp.h /usr/include/sys/stat.h
lex.o: /usr/include/errno.h
list.o: list.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
list.o: /usr/include/signal.h /usr/include/machine/trap.h
list.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
list.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
list.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
list.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
list.o: local.h v7.local.h glob.h /usr/include/setjmp.h /usr/include/ctype.h
main.o: main.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
main.o: /usr/include/signal.h /usr/include/machine/trap.h
main.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
main.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
main.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
main.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
main.o: local.h v7.local.h glob.h /usr/include/setjmp.h /usr/include/sys/stat.h
names.o: names.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
names.o: /usr/include/signal.h /usr/include/machine/trap.h
names.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
names.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
names.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
names.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
names.o: local.h v7.local.h glob.h /usr/include/setjmp.h
names.o: /usr/include/sys/wait.h
optim.o: optim.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
optim.o: /usr/include/signal.h /usr/include/machine/trap.h
optim.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
optim.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
optim.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
optim.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
optim.o: local.h v7.local.h glob.h /usr/include/setjmp.h configdefs.h
popen.o: popen.c /usr/include/stdio.h /usr/include/sys/signal.h
popen.o: /usr/include/machine/trap.h /usr/include/sys/types.h
popen.o: /usr/include/sys/wait.h /usr/include/machine/endian.h
popen.o: /usr/include/errno.h
quit.o: quit.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
quit.o: /usr/include/signal.h /usr/include/machine/trap.h
quit.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
quit.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
quit.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
quit.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
quit.o: local.h v7.local.h glob.h /usr/include/setjmp.h /usr/include/sys/stat.h
quit.o: /usr/include/sys/file.h
send.o: send.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
send.o: /usr/include/signal.h /usr/include/machine/trap.h
send.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
send.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
send.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
send.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
send.o: local.h v7.local.h glob.h /usr/include/setjmp.h /usr/include/sys/wait.h
send.o: /usr/include/sys/stat.h
strings.o: strings.c rcv.h def.h /usr/include/sys/param.h
strings.o: /usr/include/sys/types.h /usr/include/signal.h
strings.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
strings.o: /usr/include/machine/endian.h /usr/include/sys/signal.h
strings.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
strings.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
strings.o: /usr/include/ctype.h /usr/include/strings.h local.h v7.local.h
strings.o: glob.h /usr/include/setjmp.h
temp.o: temp.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
temp.o: /usr/include/signal.h /usr/include/machine/trap.h
temp.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
temp.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
temp.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
temp.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
temp.o: local.h v7.local.h glob.h /usr/include/setjmp.h
tty.o: tty.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
tty.o: /usr/include/signal.h /usr/include/machine/trap.h
tty.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
tty.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
tty.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
tty.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
tty.o: local.h v7.local.h glob.h /usr/include/setjmp.h
vars.o: vars.c rcv.h def.h /usr/include/sys/param.h /usr/include/sys/types.h
vars.o: /usr/include/signal.h /usr/include/machine/trap.h
vars.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
vars.o: /usr/include/sys/signal.h /usr/include/stdio.h /usr/include/sgtty.h
vars.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
vars.o: /usr/include/sys/ttydev.h /usr/include/ctype.h /usr/include/strings.h
vars.o: local.h v7.local.h glob.h /usr/include/setjmp.h
version.o: version.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
