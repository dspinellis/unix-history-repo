#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.18 (Berkeley) %G%
#
#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.18 (Berkeley) %G%
#
#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.17 (Berkeley) 6/27/87
#
VERSION=3.7
#
# Ex skeletal makefile for VAX VM/Unix 4.2BSD
#
# Ex is very large - this version will not fit on PDP-11's without overlay
# software.  Things that can be turned off to save
# space include LISPCODE (-l flag, showmatch and lisp options), UCVISUAL
# (visual \ nonsense on upper case only terminals), CHDIR (the undocumented
# chdir command.)  VMUNIX makes ex considerably larger, raising many limits
# and improving speed and simplicity of maintenance.  It is suitable only
# for a VAX or other large machine, and then probably only in a paged system.
#
# Don't define VFORK unless your system has the VFORK system call,
# which is like fork but the two processes have only one data space until the
# child execs. This speeds up ex by saving the memory copy.
#
# If your system expands tabs to 4 spaces you should -DTABS=4 below
#
BINDIR=		/usr/ucb
NBINDIR=	/usr/new
LIBDIR=		/usr/lib
OPTIONS=	-DCRYPT -DLISPCODE -DCHDIR -DUCVISUAL -DVFORK -DVMUNIX \
		-DFASTTAG -DUNIX_SBRK -DFLOCKFILE
CFLAGS=		-DTABS=8 -O ${OPTIONS}
LDFLAGS=	-z				# or -i or -n
LIBC=		/lib/libc.a

OBJS=	ex.o ex_addr.o ex_cmds.o ex_cmds2.o ex_cmdsub.o ex_data.o ex_extern.o \
	ex_get.o ex_io.o ex_put.o ex_re.o ex_set.o ex_subr.o ex_tagio.o \
	ex_temp.o ex_tty.o ex_unix.o ex_v.o ex_vadj.o ex_vget.o ex_vmain.o \
	ex_voper.o ex_vops.o ex_vops2.o ex_vops3.o ex_vput.o ex_vwind.o \
	printf.o bcopy.o strings.o
HDRS=	ex.h ex_argv.h ex_re.h ex_temp.h ex_tty.h ex_tune.h ex_vars.h ex_vis.h
SRCS=	ex.c ex_addr.c ex_cmds.c ex_cmds2.c ex_cmdsub.c ex_data.c ex_get.c \
	ex_io.c ex_put.c ex_re.c ex_set.c ex_subr.c ex_tagio.c ex_temp.c \
	ex_tty.c ex_unix.c ex_v.c ex_vadj.c ex_vget.c ex_vmain.c ex_voper.c \
	ex_vops.c ex_vops2.c ex_vops3.c ex_vput.c ex_vwind.c printf.c bcopy.c

.c.o:
# ifdef VMUNIX
	${CC} -E ${CFLAGS} $*.c | xstr -c -
# else
#	mkstr - ex${VERSION}strings x $*.c
#	${CC} -E ${CFLAGS} x$*.c | xstr -c -
#	rm -f x$*.c
# endif
	${CC} ${CFLAGS} -c x.c 
	mv x.o $*.o

all:	ex exrecover expreserve

ex: ${OBJS} ${LIBC}
	${CC} ${LDFLAGS} ${OBJS} -ltermcap

ex_vars.h: ex_data.c
	csh makeoptions ${CFLAGS}

bcopy.o: bcopy.c
	${CC} -c ${CFLAGS} bcopy.c

# xstr: hands off!
strings.o: strings
	xstr
	${CC} -c -S xs.c
	ed - <rofix xs.s
	${AS} -o strings.o xs.s
	rm xs.s

exrecover: exrecover.o ex_extern.o
	${CC} ${CFLAGS} exrecover.o ex_extern.o -o $@

exrecover.o: exrecover.c
	${CC} ${CFLAGS} -c -O exrecover.c

expreserve: expreserve.c
	${CC} ${CFLAGS} expreserve.c -o $@

expreserve.o: expreserve.c
	${CC} ${CFLAGS} -c -O expreserve.c

clean:
#	If we dont have ex we cant make it so don't rm ex_vars.h
	rm -f a.out exrecover expreserve strings core errs trace
	rm -f *.o x*.[cs]

depend:
	mkdep ${CFLAGS} ${SRCS}

# install in standard place (/usr/ucb)
install: a.out exrecover expreserve
	rm -f ${DESTDIR}/${BINDIR}/ex ${DESTDIR}/${BINDIR}/vi \
		${DESTDIR}/${BINDIR}/view ${DESTDIR}/${BINDIR}/edit \
		${DESTDIR}/${BINDIR}/e ${DESTDIR}/usr/bin/ex
	install -s -o bin -g bin -m 1755 a.out ${DESTDIR}/${BINDIR}/ex
	install -s -o root -m 4755 exrecover ${DESTDIR}/${LIBDIR}/ex${VERSION}recover
	install -s -o root -m 4755 expreserve ${DESTDIR}/${LIBDIR}/ex${VERSION}preserve
#	cp ex${VERSION}strings ${DESTDIR}/${LIBDIR}/ex${VERSION}strings
	ln ${DESTDIR}/${BINDIR}/ex ${DESTDIR}/${BINDIR}/edit
	ln ${DESTDIR}/${BINDIR}/ex ${DESTDIR}/${BINDIR}/e
	ln ${DESTDIR}/${BINDIR}/ex ${DESTDIR}/${BINDIR}/vi
	ln ${DESTDIR}/${BINDIR}/ex ${DESTDIR}/${BINDIR}/view
	ln ${DESTDIR}/${BINDIR}/ex ${DESTDIR}/usr/bin/ex
# The following line normally fails.  This is OK.
	-mkdir ${DESTDIR}/usr/preserve

lint: FRC
	lint ${CFLAGS} ${SRCS}
	lint ${CFLAGS} exrecover.c ex_extern.c
	lint ${CFLAGS} expreserve.c

tags: FRC
	ctags ${SRCS}
	ctags -a exrecover.c
	ctags -a expreserve.c
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ex.o: ex.c
ex.o:ex.h
ex.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
ex.o: /usr/include/machine/machparam.h /usr/include/ctype.h
ex.o: /usr/include/errno.h /usr/include/signal.h /usr/include/setjmp.h
ex.o: /usr/include/sys/stat.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ex.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ex.o:ex_tune.h
ex.o:ex_vars.h
ex.o:ex_argv.h
ex.o:ex_temp.h
ex.o:ex_tty.h
ex_addr.o: ex_addr.c
ex_addr.o:ex.h
ex_addr.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_addr.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_addr.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_addr.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_addr.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_addr.o: /usr/include/sys/ttydev.h
ex_addr.o:ex_tune.h
ex_addr.o:ex_vars.h
ex_addr.o:ex_re.h
ex_cmds.o: ex_cmds.c
ex_cmds.o:ex.h
ex_cmds.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_cmds.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_cmds.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_cmds.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_cmds.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_cmds.o: /usr/include/sys/ttydev.h
ex_cmds.o:ex_tune.h
ex_cmds.o:ex_vars.h
ex_cmds.o:ex_argv.h
ex_cmds.o:ex_temp.h
ex_cmds.o:ex_tty.h
ex_cmds.o:ex_vis.h
ex_cmds2.o: ex_cmds2.c
ex_cmds2.o:ex.h
ex_cmds2.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_cmds2.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_cmds2.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_cmds2.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_cmds2.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_cmds2.o: /usr/include/sys/ttydev.h
ex_cmds2.o:ex_tune.h
ex_cmds2.o:ex_vars.h
ex_cmds2.o:ex_argv.h
ex_cmds2.o:ex_temp.h
ex_cmds2.o:ex_tty.h
ex_cmds2.o:ex_vis.h
ex_cmdsub.o: ex_cmdsub.c
ex_cmdsub.o:ex.h
ex_cmdsub.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_cmdsub.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_cmdsub.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_cmdsub.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_cmdsub.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_cmdsub.o: /usr/include/sys/ttydev.h
ex_cmdsub.o:ex_tune.h
ex_cmdsub.o:ex_vars.h
ex_cmdsub.o:ex_argv.h
ex_cmdsub.o:ex_temp.h
ex_cmdsub.o:ex_tty.h
ex_cmdsub.o:ex_vis.h
ex_data.o: ex_data.c
ex_data.o:ex.h
ex_data.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_data.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_data.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_data.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_data.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_data.o: /usr/include/sys/ttydev.h
ex_data.o:ex_tune.h
ex_data.o:ex_vars.h
ex_data.o:ex_tty.h
ex_get.o: ex_get.c
ex_get.o:ex.h
ex_get.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_get.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_get.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_get.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_get.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_get.o: /usr/include/sys/ttydev.h
ex_get.o:ex_tune.h
ex_get.o:ex_vars.h
ex_get.o:ex_tty.h
ex_io.o: ex_io.c
ex_io.o:ex.h
ex_io.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_io.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_io.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_io.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_io.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_io.o: /usr/include/sys/ttydev.h
ex_io.o:ex_tune.h
ex_io.o:ex_vars.h
ex_io.o:ex_argv.h
ex_io.o:ex_temp.h
ex_io.o:ex_tty.h
ex_io.o:ex_vis.h
ex_put.o: ex_put.c
ex_put.o:ex.h
ex_put.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_put.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_put.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_put.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_put.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_put.o: /usr/include/sys/ttydev.h
ex_put.o:ex_tune.h
ex_put.o:ex_vars.h
ex_put.o:ex_tty.h
ex_put.o:ex_vis.h
ex_re.o: ex_re.c
ex_re.o:ex.h
ex_re.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_re.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_re.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_re.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_re.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_re.o: /usr/include/sys/ttydev.h
ex_re.o:ex_tune.h
ex_re.o:ex_vars.h
ex_re.o:ex_re.h
ex_set.o: ex_set.c
ex_set.o:ex.h
ex_set.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_set.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_set.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_set.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_set.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_set.o: /usr/include/sys/ttydev.h
ex_set.o:ex_tune.h
ex_set.o:ex_vars.h
ex_set.o:ex_temp.h
ex_set.o:ex_tty.h
ex_subr.o: ex_subr.c
ex_subr.o:ex.h
ex_subr.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_subr.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_subr.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_subr.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_subr.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_subr.o: /usr/include/sys/ttydev.h
ex_subr.o:ex_tune.h
ex_subr.o:ex_vars.h
ex_subr.o:ex_re.h
ex_subr.o:ex_tty.h
ex_subr.o:ex_vis.h
ex_tagio.o: ex_tagio.c /usr/include/sys/file.h
ex_tagio.o:ex.h
ex_tagio.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_tagio.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_tagio.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_tagio.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_tagio.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_tagio.o: /usr/include/sys/ttydev.h
ex_tagio.o:ex_tune.h
ex_tagio.o:ex_vars.h
ex_temp.o: ex_temp.c
ex_temp.o:ex.h
ex_temp.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_temp.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_temp.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_temp.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_temp.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_temp.o: /usr/include/sys/ttydev.h
ex_temp.o:ex_tune.h
ex_temp.o:ex_vars.h
ex_temp.o:ex_temp.h
ex_temp.o:ex_vis.h
ex_temp.o:ex_tty.h
ex_tty.o: ex_tty.c
ex_tty.o:ex.h
ex_tty.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_tty.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_tty.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_tty.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_tty.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_tty.o: /usr/include/sys/ttydev.h
ex_tty.o:ex_tune.h
ex_tty.o:ex_vars.h
ex_tty.o:ex_tty.h
ex_unix.o: ex_unix.c
ex_unix.o:ex.h
ex_unix.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_unix.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_unix.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_unix.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_unix.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_unix.o: /usr/include/sys/ttydev.h
ex_unix.o:ex_tune.h
ex_unix.o:ex_vars.h
ex_unix.o:ex_temp.h
ex_unix.o:ex_tty.h
ex_unix.o:ex_vis.h
ex_v.o: ex_v.c
ex_v.o:ex.h
ex_v.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
ex_v.o: /usr/include/machine/machparam.h /usr/include/ctype.h
ex_v.o: /usr/include/errno.h /usr/include/signal.h /usr/include/setjmp.h
ex_v.o: /usr/include/sys/stat.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
ex_v.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ex_v.o:ex_tune.h
ex_v.o:ex_vars.h
ex_v.o:ex_re.h
ex_v.o:ex_tty.h
ex_v.o:ex_vis.h
ex_vadj.o: ex_vadj.c
ex_vadj.o:ex.h
ex_vadj.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_vadj.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_vadj.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_vadj.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_vadj.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_vadj.o: /usr/include/sys/ttydev.h
ex_vadj.o:ex_tune.h
ex_vadj.o:ex_vars.h
ex_vadj.o:ex_tty.h
ex_vadj.o:ex_vis.h
ex_vget.o: ex_vget.c
ex_vget.o:ex.h
ex_vget.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_vget.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_vget.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_vget.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_vget.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_vget.o: /usr/include/sys/ttydev.h
ex_vget.o:ex_tune.h
ex_vget.o:ex_vars.h
ex_vget.o:ex_tty.h
ex_vget.o:ex_vis.h
ex_vmain.o: ex_vmain.c
ex_vmain.o:ex.h
ex_vmain.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_vmain.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_vmain.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_vmain.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_vmain.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_vmain.o: /usr/include/sys/ttydev.h
ex_vmain.o:ex_tune.h
ex_vmain.o:ex_vars.h
ex_vmain.o:ex_tty.h
ex_vmain.o:ex_vis.h
ex_voper.o: ex_voper.c
ex_voper.o:ex.h
ex_voper.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_voper.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_voper.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_voper.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_voper.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_voper.o: /usr/include/sys/ttydev.h
ex_voper.o:ex_tune.h
ex_voper.o:ex_vars.h
ex_voper.o:ex_tty.h
ex_voper.o:ex_vis.h
ex_vops.o: ex_vops.c
ex_vops.o:ex.h
ex_vops.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_vops.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_vops.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_vops.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_vops.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_vops.o: /usr/include/sys/ttydev.h
ex_vops.o:ex_tune.h
ex_vops.o:ex_vars.h
ex_vops.o:ex_tty.h
ex_vops.o:ex_vis.h
ex_vops2.o: ex_vops2.c
ex_vops2.o:ex.h
ex_vops2.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_vops2.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_vops2.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_vops2.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_vops2.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_vops2.o: /usr/include/sys/ttydev.h
ex_vops2.o:ex_tune.h
ex_vops2.o:ex_vars.h
ex_vops2.o:ex_tty.h
ex_vops2.o:ex_vis.h
ex_vops3.o: ex_vops3.c
ex_vops3.o:ex.h
ex_vops3.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_vops3.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_vops3.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_vops3.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_vops3.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_vops3.o: /usr/include/sys/ttydev.h
ex_vops3.o:ex_tune.h
ex_vops3.o:ex_vars.h
ex_vops3.o:ex_tty.h
ex_vops3.o:ex_vis.h
ex_vput.o: ex_vput.c
ex_vput.o:ex.h
ex_vput.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_vput.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_vput.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_vput.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_vput.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_vput.o: /usr/include/sys/ttydev.h
ex_vput.o:ex_tune.h
ex_vput.o:ex_vars.h
ex_vput.o:ex_tty.h
ex_vput.o:ex_vis.h
ex_vwind.o: ex_vwind.c
ex_vwind.o:ex.h
ex_vwind.o: /usr/include/sys/param.h /usr/include/sys/types.h
ex_vwind.o: /usr/include/signal.h /usr/include/machine/machparam.h
ex_vwind.o: /usr/include/ctype.h /usr/include/errno.h /usr/include/signal.h
ex_vwind.o: /usr/include/setjmp.h /usr/include/sys/stat.h /usr/include/sgtty.h
ex_vwind.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ex_vwind.o: /usr/include/sys/ttydev.h
ex_vwind.o:ex_tune.h
ex_vwind.o:ex_vars.h
ex_vwind.o:ex_tty.h
ex_vwind.o:ex_vis.h
printf.o: printf.c /usr/include/varargs.h
bcopy.o: bcopy.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
