#
# Copyright (c) 1987 Regents of the University of California.
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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.3 (Berkeley) %G%
#
MDIR=	${DESTDIR}/usr/man/cat3f
OBJS=	abort.0 access.0 alarm.0 bessel.0 bit.0 chdir.0 chmod.0 etime.0 \
	exit.0 fdate.0 flmin.0 flush.0 fork.0 fseek.0 getarg.0 getc.0 \
	getcwd.0 getenv.0 getlog.0 getpid.0 getuid.0 getuid.0 hostnm.0 \
	idate.0 index.0 intro.0 ioinit.0 kill.0 link.0 loc.0 long.0 \
	malloc.0 perror.0 plot.0 putc.0 qsort.0 rand.0 random.0 rename.0 \
	signal.0 sleep.0 stat.0 system.0 time.0 topen.0 traper.0 trapov.0 \
	trpfpe.0 ttynam.0 unlink.0 wait.0

all clean depent lint tags:

cleandir:
	rm -f ${OBJS}

install: ${OBJS}
	install -c -o bin -g bin -m 444 ${OBJS} ${MDIR}
	rm -f ${MDIR}/and.0; ln ${MDIR}/bit.0 ${MDIR}/and.0
	rm -f ${MDIR}/lshift.0; ln ${MDIR}/bit.0 ${MDIR}/lshift.0
	rm -f ${MDIR}/not.0; ln ${MDIR}/bit.0 ${MDIR}/not.0
	rm -f ${MDIR}/or.0; ln ${MDIR}/bit.0 ${MDIR}/or.0
	rm -f ${MDIR}/rshift.0; ln ${MDIR}/bit.0 ${MDIR}/rshift.0
	rm -f ${MDIR}/xor.0; ln ${MDIR}/bit.0 ${MDIR}/xor.0
	rm -f ${MDIR}/box.0; ln ${MDIR}/plot.0 ${MDIR}/box.0
	rm -f ${MDIR}/ctime.0; ln ${MDIR}/time.0 ${MDIR}/ctime.0
	rm -f ${MDIR}/drand.0; ln ${MDIR}/rand.0 ${MDIR}/drand.0
	rm -f ${MDIR}/fgetc.0; ln ${MDIR}/getc.0 ${MDIR}/fgetc.0
	rm -f ${MDIR}/fputc.0; ln ${MDIR}/putc.0 ${MDIR}/fputc.0
	rm -f ${MDIR}/fstat.0; ln ${MDIR}/stat.0 ${MDIR}/fstat.0
	rm -f ${MDIR}/gmtime.0; ln ${MDIR}/time.0 ${MDIR}/gmtime.0
	rm -f ${MDIR}/irand.0; ln ${MDIR}/rand.0 ${MDIR}/irand.0
	rm -f ${MDIR}/lstat.0; ln ${MDIR}/stat.0 ${MDIR}/lstat.0
	rm -f ${MDIR}/ltime.0; ln ${MDIR}/time.0 ${MDIR}/ltime.0
	rm -f ${MDIR}/short.0; ln ${MDIR}/long.0 ${MDIR}/short.0
	rm -f ${MDIR}/symlnk.0; ln ${MDIR}/link.0 ${MDIR}/symlnk.0
	rm -f ${MDIR}/dffrac.0; ln ${MDIR}/flmin.0 ${MDIR}/dffrac.0
	rm -f ${MDIR}/dflmax.0; ln ${MDIR}/flmin.0 ${MDIR}/dflmax.0
	rm -f ${MDIR}/dflmin.0; ln ${MDIR}/flmin.0 ${MDIR}/dflmin.0
	rm -f ${MDIR}/dtime.0; ln ${MDIR}/etime.0 ${MDIR}/dtime.0
	rm -f ${MDIR}/ffrac.0; ln ${MDIR}/flmin.0 ${MDIR}/ffrac.0
	rm -f ${MDIR}/flmax.0; ln ${MDIR}/flmin.0 ${MDIR}/flmax.0
	rm -f ${MDIR}/fortran.0; ln ${MDIR}/intro.0 ${MDIR}/fortran.0
	rm -f ${MDIR}/ftell.0; ln ${MDIR}/fseek.0 ${MDIR}/ftell.0
	rm -f ${MDIR}/inmax.0; ln ${MDIR}/flmin.0 ${MDIR}/inmax.0
	rm -f ${MDIR}/itime.0; ln ${MDIR}/idate.0 ${MDIR}/itime.0
	rm -f ${MDIR}/len.0; ln ${MDIR}/index.0 ${MDIR}/len.0
	rm -f ${MDIR}/lnblnk.0; ln ${MDIR}/index.0 ${MDIR}/lnblnk.0
	rm -f ${MDIR}/rindex.0; ln ${MDIR}/index.0 ${MDIR}/rindex.0
	rm -f ${MDIR}/tclose.0; ln ${MDIR}/topen.0 ${MDIR}/tclose.0
	rm -f ${MDIR}/tread.0; ln ${MDIR}/topen.0 ${MDIR}/tread.0
	rm -f ${MDIR}/trewin.0; ln ${MDIR}/topen.0 ${MDIR}/trewin.0
	rm -f ${MDIR}/tskipf.0; ln ${MDIR}/topen.0 ${MDIR}/tskipf.0
	rm -f ${MDIR}/tstate.0; ln ${MDIR}/topen.0 ${MDIR}/tstate.0
	rm -f ${MDIR}/twrite.0; ln ${MDIR}/topen.0 ${MDIR}/twrite.0
	rm -f ${MDIR}/drandm.0; ln ${MDIR}/random.0 ${MDIR}/drandm.0
	rm -f ${MDIR}/falloc.0; ln ${MDIR}/malloc.0 ${MDIR}/falloc.0
	rm -f ${MDIR}/fpecnt.0; ln ${MDIR}/trpfpe.0 ${MDIR}/fpecnt.0
	rm -f ${MDIR}/gerror.0; ln ${MDIR}/perror.0 ${MDIR}/gerror.0
	rm -f ${MDIR}/getgid.0; ln ${MDIR}/getuid.0 ${MDIR}/getgid.0
	rm -f ${MDIR}/iargc.0; ln ${MDIR}/getarg.0 ${MDIR}/iargc.0
	rm -f ${MDIR}/ierrno.0; ln ${MDIR}/perror.0 ${MDIR}/ierrno.0
	rm -f ${MDIR}/irandm.0; ln ${MDIR}/random.0 ${MDIR}/irandm.0
	rm -f ${MDIR}/isatty.0; ln ${MDIR}/ttynam.0 ${MDIR}/isatty.0
	rm -f ${MDIR}/besj0.0; ln ${MDIR}/bessel.0 ${MDIR}/besj0.0
	rm -f ${MDIR}/besj1.0; ln ${MDIR}/bessel.0 ${MDIR}/besj1.0
	rm -f ${MDIR}/besjn.0; ln ${MDIR}/bessel.0 ${MDIR}/besjn.0
	rm -f ${MDIR}/besy0.0; ln ${MDIR}/bessel.0 ${MDIR}/besy0.0
	rm -f ${MDIR}/besy1.0; ln ${MDIR}/bessel.0 ${MDIR}/besy1.0
	rm -f ${MDIR}/besyn.0; ln ${MDIR}/bessel.0 ${MDIR}/besyn.0
	rm -f ${MDIR}/dbesj0.0; ln ${MDIR}/bessel.0 ${MDIR}/dbesj0.0
	rm -f ${MDIR}/dbesj1.0; ln ${MDIR}/bessel.0 ${MDIR}/dbesj1.0
	rm -f ${MDIR}/dbesjn.0; ln ${MDIR}/bessel.0 ${MDIR}/dbesjn.0
	rm -f ${MDIR}/dbesy0.0; ln ${MDIR}/bessel.0 ${MDIR}/dbesy0.0
	rm -f ${MDIR}/dbesy1.0; ln ${MDIR}/bessel.0 ${MDIR}/dbesy1.0
	rm -f ${MDIR}/dbesyn.0; ln ${MDIR}/bessel.0 ${MDIR}/dbesyn.0
