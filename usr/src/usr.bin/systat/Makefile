#       Makefile     1.9     85/05/22
DESTDIR=
INCPATH=
CFLAGS= -O
SRCS=	cmds.c cmdtab.c disks.c fetch.c iostat.c \
	keyboard.c kre.c main.c mbufs.c netcmds.c netstat.c pigs.c swap.c
OBJS=	cmds.o cmdtab.o disks.o fetch.o iostat.o \
	keyboard.o kre.o main.o mbufs.o netcmds.o netstat.o pigs.o swap.o
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


cmds.o: cmds.c ./systat.h /usr/include/sys/dk.h /usr/include/curses.h
cmds.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmds.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmds.o: /usr/include/sys/jioctl.h /usr/include/sys/ioctl.h
cmds.o: /usr/include/signal.h /usr/include/ctype.h
cmdtab.o: cmdtab.c ./systat.h /usr/include/sys/dk.h /usr/include/curses.h
cmdtab.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmdtab.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmdtab.o: /usr/include/sys/jioctl.h /usr/include/sys/ioctl.h
disks.o: disks.c ./systat.h /usr/include/sys/dk.h /usr/include/curses.h
disks.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
disks.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
disks.o: /usr/include/sys/jioctl.h /usr/include/sys/ioctl.h
disks.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
disks.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/buf.h
disks.o: /usr/include/sys/file.h /usr/include/vaxuba/ubavar.h
disks.o: /usr/include/vaxmba/mbavar.h /usr/include/ctype.h
fetch.o: fetch.c ./systat.h /usr/include/sys/dk.h /usr/include/curses.h
fetch.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
fetch.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
fetch.o: /usr/include/sys/jioctl.h /usr/include/sys/ioctl.h
fetch.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
fetch.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/dir.h
fetch.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
fetch.o: /usr/include/sys/dmap.h /usr/include/sys/time.h
fetch.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
fetch.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
fetch.o: /usr/include/sys/file.h /usr/include/sys/vmmac.h
fetch.o: /usr/include/machine/pte.h /usr/include/pwd.h
iostat.o: iostat.c ./systat.h /usr/include/sys/dk.h /usr/include/curses.h
iostat.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
iostat.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
iostat.o: /usr/include/sys/jioctl.h /usr/include/sys/ioctl.h
iostat.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
iostat.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/buf.h
iostat.o: /usr/include/sys/file.h /usr/include/nlist.h
keyboard.o: keyboard.c ./systat.h /usr/include/sys/dk.h /usr/include/curses.h
keyboard.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
keyboard.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
keyboard.o: /usr/include/sys/jioctl.h /usr/include/sys/ioctl.h
keyboard.o: /usr/include/signal.h /usr/include/ctype.h
main.o: main.c ./systat.h /usr/include/sys/dk.h /usr/include/curses.h
main.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/sys/jioctl.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/file.h /usr/include/nlist.h /usr/include/signal.h
mbufs.o: mbufs.c ./systat.h /usr/include/sys/dk.h /usr/include/curses.h
mbufs.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
mbufs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mbufs.o: /usr/include/sys/jioctl.h /usr/include/sys/ioctl.h
mbufs.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
mbufs.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/mbuf.h
mbufs.o: /usr/include/sys/file.h /usr/include/nlist.h
pigs.o: pigs.c ./systat.h /usr/include/sys/dk.h /usr/include/curses.h
pigs.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
pigs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
pigs.o: /usr/include/sys/jioctl.h /usr/include/sys/ioctl.h
pigs.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
pigs.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/dir.h
pigs.o: /usr/include/sys/time.h /usr/include/sys/proc.h /usr/include/sys/file.h
pigs.o: /usr/include/nlist.h /usr/include/pwd.h
swap.o: swap.c ./systat.h /usr/include/sys/dk.h /usr/include/curses.h
swap.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
swap.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
swap.o: /usr/include/sys/jioctl.h /usr/include/sys/ioctl.h
swap.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
swap.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/dir.h
swap.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
swap.o: /usr/include/sys/dmap.h /usr/include/sys/time.h
swap.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
swap.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/sys/proc.h
swap.o: /usr/include/sys/text.h /usr/include/sys/conf.h /usr/include/sys/file.h
swap.o: /usr/include/sys/vmmac.h /usr/include/machine/pte.h
swap.o: /usr/include/nlist.h
vmstat.o: vmstat.c ./systat.h /usr/include/sys/dk.h /usr/include/curses.h
vmstat.o: /usr/include/stdio.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
vmstat.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
vmstat.o: /usr/include/sys/jioctl.h /usr/include/sys/ioctl.h
vmstat.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
vmstat.o: /usr/include/signal.h /usr/include/sys/types.h
vmstat.o: /usr/include/sys/vmmeter.h /usr/include/sys/file.h
vmstat.o: /usr/include/nlist.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
