#
# Copyright (c) 1988 Regents of the University of California.
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
#	@(#)Makefile	5.6 (Berkeley) 6/29/88
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	cmds.c cmdtab.c ftp.c getpass.c glob.c main.c pclose.c ruserpass.c              domacro.c
OBJS=	cmds.o cmdtab.o ftp.o getpass.o glob.o main.o pclose.o ruserpass.o              domacro.o

all: ftp

ftp:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core ftp

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 ftp ${DESTDIR}/usr/ucb/ftp

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

cmds.o: cmds.c ftp_var.h /usr/include/sys/param.h /usr/include/sys/types.h
cmds.o: /usr/include/signal.h /usr/include/machine/trap.h
cmds.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cmds.o: /usr/include/setjmp.h /usr/include/sys/socket.h /usr/include/arpa/ftp.h
cmds.o: /usr/include/signal.h /usr/include/stdio.h /usr/include/errno.h
cmds.o: /usr/include/netdb.h /usr/include/ctype.h /usr/include/sys/wait.h
cmdtab.o: cmdtab.c ftp_var.h /usr/include/sys/param.h /usr/include/sys/types.h
cmdtab.o: /usr/include/signal.h /usr/include/machine/trap.h
cmdtab.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cmdtab.o: /usr/include/setjmp.h
ftp.o: ftp.c ftp_var.h /usr/include/sys/param.h /usr/include/sys/types.h
ftp.o: /usr/include/signal.h /usr/include/machine/trap.h
ftp.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
ftp.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sys/ioctl.h
ftp.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ftp.o: /usr/include/sys/socket.h /usr/include/sys/time.h /usr/include/time.h
ftp.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
ftp.o: /usr/include/machine/machparam.h /usr/include/netinet/in.h
ftp.o: /usr/include/arpa/ftp.h /usr/include/arpa/telnet.h /usr/include/stdio.h
ftp.o: /usr/include/signal.h /usr/include/errno.h /usr/include/netdb.h
ftp.o: /usr/include/fcntl.h /usr/include/pwd.h /usr/include/ctype.h
getpass.o: getpass.c /usr/include/stdio.h /usr/include/signal.h
getpass.o: /usr/include/machine/trap.h /usr/include/sgtty.h
getpass.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
getpass.o: /usr/include/sys/ttydev.h
glob.o: glob.c /usr/include/sys/param.h /usr/include/sys/types.h
glob.o: /usr/include/signal.h /usr/include/machine/trap.h
glob.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
glob.o: /usr/include/sys/stat.h /usr/include/sys/dir.h /usr/include/stdio.h
glob.o: /usr/include/errno.h /usr/include/pwd.h
main.o: main.c ftp_var.h /usr/include/sys/param.h /usr/include/sys/types.h
main.o: /usr/include/signal.h /usr/include/machine/trap.h
main.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
main.o: /usr/include/setjmp.h /usr/include/sys/socket.h
main.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
main.o: /usr/include/sys/ttydev.h /usr/include/sys/types.h
main.o: /usr/include/arpa/ftp.h /usr/include/signal.h /usr/include/stdio.h
main.o: /usr/include/errno.h /usr/include/ctype.h /usr/include/netdb.h
main.o: /usr/include/pwd.h
pclose.o: pclose.c /usr/include/stdio.h /usr/include/signal.h
pclose.o: /usr/include/machine/trap.h /usr/include/sys/param.h
pclose.o: /usr/include/sys/types.h /usr/include/signal.h
pclose.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
pclose.o: /usr/include/sys/wait.h
ruserpass.o: ruserpass.c /usr/include/stdio.h /usr/include/utmp.h
ruserpass.o: /usr/include/ctype.h /usr/include/sys/types.h
ruserpass.o: /usr/include/sys/stat.h /usr/include/errno.h
domacro.o: domacro.c ftp_var.h /usr/include/sys/param.h
domacro.o: /usr/include/sys/types.h /usr/include/signal.h
domacro.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
domacro.o: /usr/include/machine/endian.h /usr/include/setjmp.h
domacro.o: /usr/include/signal.h /usr/include/stdio.h /usr/include/errno.h
domacro.o: /usr/include/ctype.h /usr/include/sys/ttychars.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
