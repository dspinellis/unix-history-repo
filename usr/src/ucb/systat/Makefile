#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4	(Berkeley)	6/16/87
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

cmds.o: cmds.c systat.h /usr/include/netdb.h /usr/include/nlist.h
cmds.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/curses.h
cmds.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmds.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmds.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
cmds.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cmds.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
cmds.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h /usr/include/ctype.h
cmdtab.o: cmdtab.c systat.h /usr/include/netdb.h /usr/include/nlist.h
cmdtab.o: /usr/include/signal.h /usr/include/machine/trap.h
cmdtab.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
cmdtab.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
cmdtab.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
cmdtab.o: /usr/include/sys/types.h /usr/include/signal.h
cmdtab.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cmdtab.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
cmdtab.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
disks.o: disks.c systat.h /usr/include/netdb.h /usr/include/nlist.h
disks.o: /usr/include/signal.h /usr/include/machine/trap.h
disks.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
disks.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
disks.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
disks.o: /usr/include/sys/types.h /usr/include/signal.h
disks.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
disks.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
disks.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
disks.o: /usr/include/sys/buf.h /usr/include/ctype.h
disks.o: /usr/include/tahoevba/vbavar.h
fetch.o: fetch.c systat.h /usr/include/netdb.h /usr/include/nlist.h
fetch.o: /usr/include/signal.h /usr/include/machine/trap.h
fetch.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
fetch.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
fetch.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
fetch.o: /usr/include/sys/types.h /usr/include/signal.h
fetch.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
fetch.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
fetch.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
fetch.o: /usr/include/sys/dir.h /usr/include/sys/user.h
fetch.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
fetch.o: /usr/include/sys/time.h /usr/include/time.h
fetch.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
fetch.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
fetch.o: /usr/include/sys/vmmac.h /usr/include/machine/pte.h /usr/include/pwd.h
iostat.o: iostat.c systat.h /usr/include/netdb.h /usr/include/nlist.h
iostat.o: /usr/include/signal.h /usr/include/machine/trap.h
iostat.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
iostat.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
iostat.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
iostat.o: /usr/include/sys/types.h /usr/include/signal.h
iostat.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
iostat.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
iostat.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
iostat.o: /usr/include/sys/buf.h
keyboard.o: keyboard.c systat.h /usr/include/netdb.h /usr/include/nlist.h
keyboard.o: /usr/include/signal.h /usr/include/machine/trap.h
keyboard.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
keyboard.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
keyboard.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
keyboard.o: /usr/include/sys/types.h /usr/include/signal.h
keyboard.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
keyboard.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
keyboard.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
keyboard.o: /usr/include/ctype.h
vmstat.o: vmstat.c systat.h /usr/include/netdb.h /usr/include/nlist.h
vmstat.o: /usr/include/signal.h /usr/include/machine/trap.h
vmstat.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
vmstat.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
vmstat.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
vmstat.o: /usr/include/sys/types.h /usr/include/signal.h
vmstat.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
vmstat.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
vmstat.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
vmstat.o: /usr/include/ctype.h /usr/include/utmp.h /usr/include/sys/vm.h
vmstat.o: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
vmstat.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
vmstat.o: /usr/include/sys/vmsystm.h /usr/include/sys/buf.h
vmstat.o: /usr/include/sys/stat.h /usr/include/sys/dir.h
vmstat.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
vmstat.o: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
vmstat.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
vmstat.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
vmstat.o: /usr/include/sys/namei.h /usr/include/machine/pte.h
main.o: main.c systat.h /usr/include/netdb.h /usr/include/nlist.h
main.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/curses.h
main.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
main.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
main.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
main.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
mbufs.o: mbufs.c systat.h /usr/include/netdb.h /usr/include/nlist.h
mbufs.o: /usr/include/signal.h /usr/include/machine/trap.h
mbufs.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
mbufs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
mbufs.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
mbufs.o: /usr/include/sys/types.h /usr/include/signal.h
mbufs.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
mbufs.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
mbufs.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
mbufs.o: /usr/include/sys/mbuf.h
netcmds.o: netcmds.c systat.h /usr/include/netdb.h /usr/include/nlist.h
netcmds.o: /usr/include/signal.h /usr/include/machine/trap.h
netcmds.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
netcmds.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
netcmds.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
netcmds.o: /usr/include/sys/types.h /usr/include/signal.h
netcmds.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
netcmds.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
netcmds.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
netcmds.o: /usr/include/ctype.h /usr/include/sys/socket.h
netcmds.o: /usr/include/sys/socketvar.h /usr/include/sys/mbuf.h
netcmds.o: /usr/include/sys/protosw.h /usr/include/net/route.h
netcmds.o: /usr/include/netinet/in_systm.h /usr/include/netinet/in_pcb.h
netstat.o: netstat.c systat.h /usr/include/netdb.h /usr/include/nlist.h
netstat.o: /usr/include/signal.h /usr/include/machine/trap.h
netstat.o: /usr/include/curses.h /usr/include/stdio.h /usr/include/sgtty.h
netstat.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
netstat.o: /usr/include/sys/ttydev.h /usr/include/sys/param.h
netstat.o: /usr/include/sys/types.h /usr/include/signal.h
netstat.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
netstat.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
netstat.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
netstat.o: /usr/include/sys/socket.h /usr/include/sys/socketvar.h
netstat.o: /usr/include/sys/mbuf.h /usr/include/sys/protosw.h
netstat.o: /usr/include/net/route.h /usr/include/netinet/in_systm.h
netstat.o: /usr/include/netinet/in_pcb.h /usr/include/netinet/ip.h
netstat.o: /usr/include/netinet/ip_icmp.h /usr/include/netinet/icmp_var.h
netstat.o: /usr/include/netinet/ip_var.h /usr/include/netinet/tcp.h
netstat.o: /usr/include/netinet/tcpip.h /usr/include/netinet/tcp_seq.h
netstat.o: /usr/include/netinet/tcp_fsm.h /usr/include/netinet/tcp_timer.h
netstat.o: /usr/include/netinet/tcp_var.h /usr/include/netinet/tcp_debug.h
netstat.o: /usr/include/netinet/udp.h /usr/include/netinet/udp_var.h
pigs.o: pigs.c systat.h /usr/include/netdb.h /usr/include/nlist.h
pigs.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/curses.h
pigs.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
pigs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
pigs.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
pigs.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
pigs.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
pigs.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
pigs.o: /usr/include/sys/dir.h /usr/include/sys/time.h /usr/include/time.h
pigs.o: /usr/include/sys/proc.h /usr/include/pwd.h
swap.o: swap.c systat.h /usr/include/netdb.h /usr/include/nlist.h
swap.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/curses.h
swap.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
swap.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
swap.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
swap.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
swap.o: /usr/include/sys/file.h /usr/include/sys/dkstat.h
swap.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
swap.o: /usr/include/sys/dir.h /usr/include/sys/user.h
swap.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
swap.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/resource.h
swap.o: /usr/include/sys/namei.h /usr/include/sys/uio.h /usr/include/errno.h
swap.o: /usr/include/sys/proc.h /usr/include/sys/text.h /usr/include/sys/conf.h
swap.o: /usr/include/sys/vmmac.h /usr/include/machine/pte.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
