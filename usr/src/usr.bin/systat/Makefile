#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
DESTDIR=
INCPATH=
CFLAGS= -O
SRCS=	cmds.c cmdtab.c disks.c fetch.c iostat.c \
	keyboard.c vmstat.c main.c mbufs.c netcmds.c netstat.c pigs.c swap.c
OBJS=	cmds.o cmdtab.o disks.o fetch.o iostat.o \
	keyboard.o vmstat.o main.o mbufs.o netcmds.o netstat.o pigs.o swap.o
LIBS=   -lcurses -ltermlib -lm

systat: ${OBJS}
	${CC} -o systat -O ${OBJS} ${LIBS}

install: systat
	install -s -g kmem -m 2755 systat ${DESTDIR}/usr/local/systat

clean:
	rm -f ${OBJS} systat core a.out errs

depend:
	cc -M ${INCPATH} ${SRCS} | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it


cmds.o: cmds.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
cmds.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
cmds.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmds.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmds.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
cmds.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/file.h
cmds.o: /usr/include/sys/dk.h /usr/include/netinet/in.h
cmds.o: /usr/include/arpa/inet.h /usr/include/ctype.h
cmdtab.o: cmdtab.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
cmdtab.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
cmdtab.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmdtab.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmdtab.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
cmdtab.o: /usr/include/signal.h /usr/include/sys/types.h
cmdtab.o: /usr/include/sys/file.h /usr/include/sys/dk.h
cmdtab.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
disks.o: disks.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
disks.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
disks.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
disks.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
disks.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
disks.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/file.h
disks.o: /usr/include/sys/dk.h /usr/include/netinet/in.h
disks.o: /usr/include/arpa/inet.h /usr/include/sys/buf.h /usr/include/ctype.h
disks.o: /usr/include/vaxuba/ubavar.h /usr/include/vaxmba/mbavar.h
fetch.o: fetch.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
fetch.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
fetch.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
fetch.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
fetch.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
fetch.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/file.h
fetch.o: /usr/include/sys/dk.h /usr/include/netinet/in.h
fetch.o: /usr/include/arpa/inet.h /usr/include/sys/dir.h
fetch.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
fetch.o: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
fetch.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
fetch.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
fetch.o: /usr/include/sys/vmmac.h /usr/include/machine/pte.h /usr/include/pwd.h
iostat.o: iostat.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
iostat.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
iostat.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
iostat.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
iostat.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
iostat.o: /usr/include/signal.h /usr/include/sys/types.h
iostat.o: /usr/include/sys/file.h /usr/include/sys/dk.h
iostat.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
iostat.o: /usr/include/sys/buf.h
keyboard.o: keyboard.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
keyboard.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
keyboard.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
keyboard.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
keyboard.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
keyboard.o: /usr/include/signal.h /usr/include/sys/types.h
keyboard.o: /usr/include/sys/file.h /usr/include/sys/dk.h
keyboard.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
keyboard.o: /usr/include/ctype.h
vmstat.o: vmstat.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
vmstat.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
vmstat.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
vmstat.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
vmstat.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
vmstat.o: /usr/include/signal.h /usr/include/sys/types.h
vmstat.o: /usr/include/sys/file.h /usr/include/sys/dk.h
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
vmstat.o: /usr/include/sys/namei.h /usr/include/vax/pte.h
main.o: main.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
main.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
main.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
main.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/file.h
main.o: /usr/include/sys/dk.h /usr/include/netinet/in.h
main.o: /usr/include/arpa/inet.h
mbufs.o: mbufs.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
mbufs.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
mbufs.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
mbufs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mbufs.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
mbufs.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/file.h
mbufs.o: /usr/include/sys/dk.h /usr/include/netinet/in.h
mbufs.o: /usr/include/arpa/inet.h /usr/include/sys/mbuf.h
netcmds.o: netcmds.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
netcmds.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
netcmds.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
netcmds.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
netcmds.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
netcmds.o: /usr/include/signal.h /usr/include/sys/types.h
netcmds.o: /usr/include/sys/file.h /usr/include/sys/dk.h
netcmds.o: /usr/include/netinet/in.h /usr/include/arpa/inet.h
netcmds.o: /usr/include/ctype.h /usr/include/sys/socket.h
netcmds.o: /usr/include/sys/socketvar.h /usr/include/sys/mbuf.h
netcmds.o: /usr/include/sys/protosw.h /usr/include/net/route.h
netcmds.o: /usr/include/netinet/in_systm.h /usr/include/netinet/in_pcb.h
netstat.o: netstat.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
netstat.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
netstat.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
netstat.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
netstat.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
netstat.o: /usr/include/signal.h /usr/include/sys/types.h
netstat.o: /usr/include/sys/file.h /usr/include/sys/dk.h
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
pigs.o: pigs.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
pigs.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
pigs.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
pigs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
pigs.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
pigs.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/file.h
pigs.o: /usr/include/sys/dk.h /usr/include/netinet/in.h
pigs.o: /usr/include/arpa/inet.h /usr/include/sys/dir.h /usr/include/sys/time.h
pigs.o: /usr/include/time.h /usr/include/sys/proc.h /usr/include/pwd.h
swap.o: swap.c ./systat.h /usr/include/netdb.h /usr/include/nlist.h
swap.o: /usr/include/signal.h /usr/include/curses.h /usr/include/stdio.h
swap.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
swap.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
swap.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
swap.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/file.h
swap.o: /usr/include/sys/dk.h /usr/include/netinet/in.h
swap.o: /usr/include/arpa/inet.h /usr/include/sys/dir.h /usr/include/sys/user.h
swap.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
swap.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/resource.h
swap.o: /usr/include/sys/namei.h /usr/include/sys/uio.h /usr/include/errno.h
swap.o: /usr/include/sys/proc.h /usr/include/sys/text.h /usr/include/sys/conf.h
swap.o: /usr/include/sys/vmmac.h /usr/include/machine/pte.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
