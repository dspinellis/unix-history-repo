#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LIBS=	-lcurses -ltermlib -lm
SRCS=	cmds.c cmdtab.c disks.c fetch.c iostat.c keyboard.c vmstat.c main.c \
	mbufs.c netcmds.c netstat.c pigs.c swap.c
OBJS=	cmds.o cmdtab.o disks.o fetch.o iostat.o keyboard.o vmstat.o main.o \
	mbufs.o netcmds.o netstat.o pigs.o swap.o

all: systat

systat:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} ${LIBS}

clean: FRC
	rm -f ${OBJS} core systat

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g kmem -m 2755 systat ${DESTDIR}/usr/ucb/systat

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

cmds.o: cmds.c
cmds.o:systat.h
cmds.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
cmds.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
cmds.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
cmds.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
cmds.o: /usr/include/sys/types.h /usr/include/signal.h
cmds.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
cmds.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
cmds.o: /usr/include/arpa/inet.h /usr/include/ctype.h
cmdtab.o: cmdtab.c
cmdtab.o:systat.h
cmdtab.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
cmdtab.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
cmdtab.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
cmdtab.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
cmdtab.o: /usr/include/sys/types.h /usr/include/signal.h
cmdtab.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
cmdtab.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
cmdtab.o: /usr/include/arpa/inet.h
disks.o: disks.c
disks.o:systat.h
disks.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
disks.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
disks.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
disks.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
disks.o: /usr/include/sys/types.h /usr/include/signal.h
disks.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
disks.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
disks.o: /usr/include/arpa/inet.h /usr/include/sys/buf.h /usr/include/ctype.h
disks.o: /usr/include/tahoevba/vbavar.h
fetch.o: fetch.c
fetch.o:systat.h
fetch.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
fetch.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
fetch.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
fetch.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
fetch.o: /usr/include/sys/types.h /usr/include/signal.h
fetch.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
fetch.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
fetch.o: /usr/include/arpa/inet.h /usr/include/sys/dir.h
fetch.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
fetch.o: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
fetch.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
fetch.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
fetch.o: /usr/include/sys/vmmac.h /usr/include/machine/pte.h /usr/include/pwd.h
iostat.o: iostat.c
iostat.o:systat.h
iostat.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
iostat.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
iostat.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
iostat.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
iostat.o: /usr/include/sys/types.h /usr/include/signal.h
iostat.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
iostat.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
iostat.o: /usr/include/arpa/inet.h /usr/include/sys/buf.h
keyboard.o: keyboard.c
keyboard.o:systat.h
keyboard.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
keyboard.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
keyboard.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
keyboard.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
keyboard.o: /usr/include/sys/types.h /usr/include/signal.h
keyboard.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
keyboard.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
keyboard.o: /usr/include/arpa/inet.h /usr/include/ctype.h
vmstat.o: vmstat.c
vmstat.o:systat.h
vmstat.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
vmstat.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
vmstat.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
vmstat.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
vmstat.o: /usr/include/sys/types.h /usr/include/signal.h
vmstat.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
vmstat.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
vmstat.o: /usr/include/arpa/inet.h /usr/include/ctype.h /usr/include/utmp.h
vmstat.o: /usr/include/sys/vm.h /usr/include/sys/vmparam.h
vmstat.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
vmstat.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h
vmstat.o: /usr/include/sys/buf.h /usr/include/sys/stat.h /usr/include/sys/dir.h
vmstat.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
vmstat.o: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
vmstat.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
vmstat.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
vmstat.o: /usr/include/sys/namei.h /usr/include/machine/pte.h
main.o: main.c
main.o:systat.h
main.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
main.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
main.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
main.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
main.o: /usr/include/sys/types.h /usr/include/signal.h
main.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
main.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
main.o: /usr/include/arpa/inet.h
mbufs.o: mbufs.c
mbufs.o:systat.h
mbufs.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
mbufs.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
mbufs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
mbufs.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
mbufs.o: /usr/include/sys/types.h /usr/include/signal.h
mbufs.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
mbufs.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
mbufs.o: /usr/include/arpa/inet.h /usr/include/sys/mbuf.h
netcmds.o: netcmds.c
netcmds.o:systat.h
netcmds.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
netcmds.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
netcmds.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
netcmds.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
netcmds.o: /usr/include/sys/types.h /usr/include/signal.h
netcmds.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
netcmds.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
netcmds.o: /usr/include/arpa/inet.h /usr/include/ctype.h
netcmds.o: /usr/include/sys/socket.h /usr/include/sys/socketvar.h
netcmds.o: /usr/include/sys/mbuf.h /usr/include/sys/protosw.h
netcmds.o: /usr/include/net/route.h /usr/include/netinet/in_systm.h
netcmds.o: /usr/include/netinet/in_pcb.h
netstat.o: netstat.c
netstat.o:systat.h
netstat.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
netstat.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
netstat.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
netstat.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
netstat.o: /usr/include/sys/types.h /usr/include/signal.h
netstat.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
netstat.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
netstat.o: /usr/include/arpa/inet.h /usr/include/sys/socket.h
netstat.o: /usr/include/sys/socketvar.h /usr/include/sys/mbuf.h
netstat.o: /usr/include/sys/protosw.h /usr/include/net/route.h
netstat.o: /usr/include/netinet/in_systm.h /usr/include/netinet/in_pcb.h
netstat.o: /usr/include/netinet/ip.h /usr/include/netinet/ip_icmp.h
netstat.o: /usr/include/netinet/icmp_var.h /usr/include/netinet/ip_var.h
netstat.o: /usr/include/netinet/tcp.h /usr/include/netinet/tcpip.h
netstat.o: /usr/include/netinet/tcp_seq.h /usr/include/netinet/tcp_fsm.h
netstat.o: /usr/include/netinet/tcp_timer.h /usr/include/netinet/tcp_var.h
netstat.o: /usr/include/netinet/tcp_debug.h /usr/include/netinet/udp.h
netstat.o: /usr/include/netinet/udp_var.h
pigs.o: pigs.c
pigs.o:systat.h
pigs.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
pigs.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
pigs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
pigs.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
pigs.o: /usr/include/sys/types.h /usr/include/signal.h
pigs.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
pigs.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
pigs.o: /usr/include/arpa/inet.h /usr/include/sys/dir.h /usr/include/sys/time.h
pigs.o: /usr/include/time.h /usr/include/sys/proc.h /usr/include/pwd.h
swap.o: swap.c
swap.o:systat.h
swap.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/signal.h
swap.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
swap.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
swap.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
swap.o: /usr/include/sys/types.h /usr/include/signal.h
swap.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
swap.o: /usr/include/sys/dkstat.h /usr/include/netinet/in.h
swap.o: /usr/include/arpa/inet.h /usr/include/sys/dir.h /usr/include/sys/user.h
swap.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
swap.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/resource.h
swap.o: /usr/include/sys/namei.h /usr/include/sys/uio.h /usr/include/errno.h
swap.o: /usr/include/sys/proc.h /usr/include/sys/text.h /usr/include/sys/conf.h
swap.o: /usr/include/sys/vmmac.h /usr/include/machine/pte.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
