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
#	@(#)Makefile	5.6 (Berkeley) %G%
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

cmds.o: cmds.c
cmds.o:ftp_var.h
cmds.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
cmds.o: /usr/include/machine/machparam.h /usr/include/setjmp.h
cmds.o: /usr/include/sys/socket.h /usr/include/arpa/ftp.h /usr/include/signal.h
cmds.o: /usr/include/stdio.h /usr/include/errno.h /usr/include/netdb.h
cmds.o: /usr/include/ctype.h /usr/include/sys/wait.h
cmdtab.o: cmdtab.c
cmdtab.o:ftp_var.h
cmdtab.o: /usr/include/sys/param.h /usr/include/sys/types.h
cmdtab.o: /usr/include/signal.h /usr/include/machine/machparam.h
cmdtab.o: /usr/include/setjmp.h
ftp.o: ftp.c
ftp.o:ftp_var.h
ftp.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
ftp.o: /usr/include/machine/machparam.h /usr/include/setjmp.h
ftp.o: /usr/include/sys/stat.h /usr/include/sys/ioctl.h
ftp.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ftp.o: /usr/include/sys/socket.h /usr/include/sys/time.h /usr/include/time.h
ftp.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
ftp.o: /usr/include/machine/machparam.h /usr/include/netinet/in.h
ftp.o: /usr/include/arpa/ftp.h /usr/include/arpa/telnet.h /usr/include/stdio.h
ftp.o: /usr/include/signal.h /usr/include/errno.h /usr/include/netdb.h
ftp.o: /usr/include/fcntl.h /usr/include/pwd.h /usr/include/ctype.h
getpass.o: getpass.c /usr/include/stdio.h /usr/include/signal.h
getpass.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
getpass.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
glob.o: glob.c /usr/include/sys/param.h /usr/include/sys/types.h
glob.o: /usr/include/signal.h /usr/include/machine/machparam.h
glob.o: /usr/include/sys/stat.h /usr/include/sys/dir.h /usr/include/stdio.h
glob.o: /usr/include/errno.h /usr/include/pwd.h
main.o: main.c
main.o:ftp_var.h
main.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
main.o: /usr/include/machine/machparam.h /usr/include/setjmp.h
main.o: /usr/include/sys/socket.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/sys/types.h /usr/include/arpa/ftp.h /usr/include/signal.h
main.o: /usr/include/stdio.h /usr/include/errno.h /usr/include/ctype.h
main.o: /usr/include/netdb.h /usr/include/pwd.h
pclose.o: pclose.c /usr/include/stdio.h /usr/include/signal.h
pclose.o: /usr/include/sys/param.h /usr/include/sys/types.h
pclose.o: /usr/include/signal.h /usr/include/machine/machparam.h
pclose.o: /usr/include/sys/wait.h
ruserpass.o: ruserpass.c /usr/include/stdio.h /usr/include/utmp.h
ruserpass.o: /usr/include/ctype.h /usr/include/sys/types.h
ruserpass.o: /usr/include/sys/stat.h /usr/include/errno.h
domacro.o: domacro.c
domacro.o:ftp_var.h
domacro.o: /usr/include/sys/param.h /usr/include/sys/types.h
domacro.o: /usr/include/signal.h /usr/include/machine/machparam.h
domacro.o: /usr/include/setjmp.h /usr/include/signal.h /usr/include/stdio.h
domacro.o: /usr/include/errno.h /usr/include/ctype.h
domacro.o: /usr/include/sys/ttychars.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
