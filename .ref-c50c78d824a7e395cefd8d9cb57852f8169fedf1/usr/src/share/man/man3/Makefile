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
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.11 (Berkeley) %G%
#
MDIR=	/usr/man/cat3
OBJS=	abort.0 abs.0 alarm.0 asinh.0 assert.0 atof.0 bstring.0 byteorder.0 \
	compat-sys5.0 crypt.0 ctime.0 ctype.0 curses.0 dbm.0 \
	directory.0 ecvt.0 end.0 erf.0 execl.0 exit.0 exp.0 fclose.0 \
	ferror.0 floor.0 fopen.0 fread.0 frexp.0 fseek.0 getc.0 \
	getdiskbyname.0 getenv.0 getfsent.0 getgrent.0 gethostbyname.0 \
	getlogin.0 getnetent.0 getopt.0 getpass.0 getprotoent.0 getpw.0 \
	getpwent.0 gets.0 getservent.0 getttyent.0 getusershell.0 getwd.0 \
	hypot.0 ieee.0 inet.0 infnan.0 initgroups.0 insque.0 intro.0 \
	j0.0 lgamma.0 lib2648.0 malloc.0 math.0 mktemp.0 monitor.0 \
	mp.0 ndbm.0 nice.0 nlist.0 ns.0 pause.0 perror.0 plot.0 popen.0 \
	printf.0 psignal.0 putc.0 puts.0 qsort.0 rand.0 random.0 rcmd.0 \
	regex.0 regexp.0 resolver.0 rexec.0 scandir.0 scanf.0 setbuf.0 \
	setjmp.0 setuid.0 siginterrupt.0 signal.0 sin.0 sinh.0 sleep.0 \
	sqrt.0 stdio.0 string.0 stty.0 swab.0 syslog.0 system.0 termcap.0 \
	time.0 times.0 treewalk.0 ttyname.0 ualarm.0 ungetc.0 usleep.0 \
	utime.0 valloc.0 varargs.0 vlimit.0 vtimes.0

all depend lint tags:

clean cleandir:
	rm -f ${OBJS}

install: ${OBJS}
	(cd ${DESTDIR}${MDIR}; rm -f *)
	install -c -o bin -g bin -m 444 ${OBJS} ${DESTDIR}${MDIR}
	ln ${DESTDIR}${MDIR}/end.0 ${DESTDIR}${MDIR}/edata.0
	ln ${DESTDIR}${MDIR}/end.0 ${DESTDIR}${MDIR}/etext.0
	ln ${DESTDIR}${MDIR}/j0.0 ${DESTDIR}${MDIR}/j1.0
	ln ${DESTDIR}${MDIR}/j0.0 ${DESTDIR}${MDIR}/jn.0
	ln ${DESTDIR}${MDIR}/j0.0 ${DESTDIR}${MDIR}/y0.0
	ln ${DESTDIR}${MDIR}/j0.0 ${DESTDIR}${MDIR}/y1.0
	ln ${DESTDIR}${MDIR}/j0.0 ${DESTDIR}${MDIR}/yn.0
	ln ${DESTDIR}${MDIR}/ns.0 ${DESTDIR}${MDIR}/ns_addr.0
	ln ${DESTDIR}${MDIR}/ns.0 ${DESTDIR}${MDIR}/ns_ntoa.0
	ln ${DESTDIR}${MDIR}/sin.0 ${DESTDIR}${MDIR}/acos.0
	ln ${DESTDIR}${MDIR}/sin.0 ${DESTDIR}${MDIR}/asin.0
	ln ${DESTDIR}${MDIR}/sin.0 ${DESTDIR}${MDIR}/atan.0
	ln ${DESTDIR}${MDIR}/sin.0 ${DESTDIR}${MDIR}/atan2.0
	ln ${DESTDIR}${MDIR}/atof.0 ${DESTDIR}${MDIR}/atoi.0
	ln ${DESTDIR}${MDIR}/atof.0 ${DESTDIR}${MDIR}/atol.0
	ln ${DESTDIR}${MDIR}/sin.0 ${DESTDIR}${MDIR}/cos.0
	ln ${DESTDIR}${MDIR}/ndbm.0 ${DESTDIR}${MDIR}/dbm_clearerr.0
	ln ${DESTDIR}${MDIR}/ndbm.0 ${DESTDIR}${MDIR}/dbm_close.0
	ln ${DESTDIR}${MDIR}/ndbm.0 ${DESTDIR}${MDIR}/dbm_delete.0
	ln ${DESTDIR}${MDIR}/ndbm.0 ${DESTDIR}${MDIR}/dbm_error.0
	ln ${DESTDIR}${MDIR}/ndbm.0 ${DESTDIR}${MDIR}/dbm_fetch.0
	ln ${DESTDIR}${MDIR}/ndbm.0 ${DESTDIR}${MDIR}/dbm_firstkey.0
	ln ${DESTDIR}${MDIR}/ndbm.0 ${DESTDIR}${MDIR}/dbm_nextkey.0
	ln ${DESTDIR}${MDIR}/ndbm.0 ${DESTDIR}${MDIR}/dbm_open.0
	ln ${DESTDIR}${MDIR}/ndbm.0 ${DESTDIR}${MDIR}/dbm_store.0
	ln ${DESTDIR}${MDIR}/dbm.0 ${DESTDIR}${MDIR}/dbminit.0
	ln ${DESTDIR}${MDIR}/dbm.0 ${DESTDIR}${MDIR}/delete.0
	ln ${DESTDIR}${MDIR}/erf.0 ${DESTDIR}${MDIR}/erfc.0
	ln ${DESTDIR}${MDIR}/exp.0 ${DESTDIR}${MDIR}/expm1.0
	ln ${DESTDIR}${MDIR}/ecvt.0 ${DESTDIR}${MDIR}/fcvt.0
	ln ${DESTDIR}${MDIR}/dbm.0 ${DESTDIR}${MDIR}/fetch.0
	ln ${DESTDIR}${MDIR}/dbm.0 ${DESTDIR}${MDIR}/firstkey.0
	ln ${DESTDIR}${MDIR}/ecvt.0 ${DESTDIR}${MDIR}/gcvt.0
	ln ${DESTDIR}${MDIR}/exp.0 ${DESTDIR}${MDIR}/log.0
	ln ${DESTDIR}${MDIR}/exp.0 ${DESTDIR}${MDIR}/log10.0
	ln ${DESTDIR}${MDIR}/exp.0 ${DESTDIR}${MDIR}/log1p.0
	ln ${DESTDIR}${MDIR}/dbm.0 ${DESTDIR}${MDIR}/nextkey.0
	ln ${DESTDIR}${MDIR}/exp.0 ${DESTDIR}${MDIR}/pow.0
	ln ${DESTDIR}${MDIR}/rcmd.0 ${DESTDIR}${MDIR}/rresvport.0
	ln ${DESTDIR}${MDIR}/rcmd.0 ${DESTDIR}${MDIR}/ruserok.0
	ln ${DESTDIR}${MDIR}/dbm.0 ${DESTDIR}${MDIR}/store.0
	ln ${DESTDIR}${MDIR}/sin.0 ${DESTDIR}${MDIR}/tan.0
	ln ${DESTDIR}${MDIR}/execl.0 ${DESTDIR}${MDIR}/exec.0
	ln ${DESTDIR}${MDIR}/inet.0 ${DESTDIR}${MDIR}/addr.0
	ln ${DESTDIR}${MDIR}/plot.0 ${DESTDIR}${MDIR}/arc.0
	ln ${DESTDIR}${MDIR}/ctime.0 ${DESTDIR}${MDIR}/asctime.0
	ln ${DESTDIR}${MDIR}/sqrt.0 ${DESTDIR}${MDIR}/cbrt.0
	ln ${DESTDIR}${MDIR}/plot.0 ${DESTDIR}${MDIR}/circle.0
	ln ${DESTDIR}${MDIR}/plot.0 ${DESTDIR}${MDIR}/cont.0
	ln ${DESTDIR}${MDIR}/ieee.0 ${DESTDIR}${MDIR}/copysign.0
	ln ${DESTDIR}${MDIR}/sinh.0 ${DESTDIR}${MDIR}/cosh.0
	ln ${DESTDIR}${MDIR}/ieee.0 ${DESTDIR}${MDIR}/drem.0
	ln ${DESTDIR}${MDIR}/crypt.0 ${DESTDIR}${MDIR}/encrypt.0
	ln ${DESTDIR}${MDIR}/plot.0 ${DESTDIR}${MDIR}/erase.0
	ln ${DESTDIR}${MDIR}/execl.0 ${DESTDIR}${MDIR}/execle.0
	ln ${DESTDIR}${MDIR}/execl.0 ${DESTDIR}${MDIR}/execlp.0
	ln ${DESTDIR}${MDIR}/execl.0 ${DESTDIR}${MDIR}/exect.0
	ln ${DESTDIR}${MDIR}/execl.0 ${DESTDIR}${MDIR}/execv.0
	ln ${DESTDIR}${MDIR}/execl.0 ${DESTDIR}${MDIR}/execve.0
	ln ${DESTDIR}${MDIR}/execl.0 ${DESTDIR}${MDIR}/execvp.0
	ln ${DESTDIR}${MDIR}/getc.0 ${DESTDIR}${MDIR}/fgetc.0
	ln ${DESTDIR}${MDIR}/gets.0 ${DESTDIR}${MDIR}/fgets.0
	ln ${DESTDIR}${MDIR}/ieee.0 ${DESTDIR}${MDIR}/finite.0
	ln ${DESTDIR}${MDIR}/putc.0 ${DESTDIR}${MDIR}/fputc.0
	ln ${DESTDIR}${MDIR}/puts.0 ${DESTDIR}${MDIR}/fputs.0
	ln ${DESTDIR}${MDIR}/time.0 ${DESTDIR}${MDIR}/ftime.0
	ln ${DESTDIR}${MDIR}/getc.0 ${DESTDIR}${MDIR}/getchar.0
	ln ${DESTDIR}${MDIR}/getc.0 ${DESTDIR}${MDIR}/getw.0
	ln ${DESTDIR}${MDIR}/ctime.0 ${DESTDIR}${MDIR}/gmtime.0
	ln ${DESTDIR}${MDIR}/stty.0 ${DESTDIR}${MDIR}/gtty.0
	ln ${DESTDIR}${MDIR}/inet.0 ${DESTDIR}${MDIR}/inet_addr.0
	ln ${DESTDIR}${MDIR}/inet.0 ${DESTDIR}${MDIR}/inet_lnaof.0
	ln ${DESTDIR}${MDIR}/inet.0 ${DESTDIR}${MDIR}/inet_makeaddr.0
	ln ${DESTDIR}${MDIR}/inet.0 ${DESTDIR}${MDIR}/inet_netof.0
	ln ${DESTDIR}${MDIR}/inet.0 ${DESTDIR}${MDIR}/inet_network.0
	ln ${DESTDIR}${MDIR}/inet.0 ${DESTDIR}${MDIR}/inet_ntoa.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/isalnum.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/isalpha.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/isascii.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/iscntrl.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/isdigit.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/islower.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/isprint.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/ispunct.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/isspace.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/isupper.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/isxdigit.0
	ln ${DESTDIR}${MDIR}/plot.0 ${DESTDIR}${MDIR}/label.0
	ln ${DESTDIR}${MDIR}/frexp.0 ${DESTDIR}${MDIR}/ldexp.0
	ln ${DESTDIR}${MDIR}/plot.0 ${DESTDIR}${MDIR}/line.0
	ln ${DESTDIR}${MDIR}/plot.0 ${DESTDIR}${MDIR}/linemod.0
	ln ${DESTDIR}${MDIR}/ctime.0 ${DESTDIR}${MDIR}/localtime.0
	ln ${DESTDIR}${MDIR}/ieee.0 ${DESTDIR}${MDIR}/logb.0
	ln ${DESTDIR}${MDIR}/frexp.0 ${DESTDIR}${MDIR}/modf.0
	ln ${DESTDIR}${MDIR}/plot.0 ${DESTDIR}${MDIR}/move.0
	ln ${DESTDIR}${MDIR}/inet.0 ${DESTDIR}${MDIR}/network.0
	ln ${DESTDIR}${MDIR}/inet.0 ${DESTDIR}${MDIR}/ntoa.0
	ln ${DESTDIR}${MDIR}/plot.0 ${DESTDIR}${MDIR}/openpl.0
	ln ${DESTDIR}${MDIR}/popen.0 ${DESTDIR}${MDIR}/pclose.0
	ln ${DESTDIR}${MDIR}/plot.0 ${DESTDIR}${MDIR}/point.0
	ln ${DESTDIR}${MDIR}/putc.0 ${DESTDIR}${MDIR}/putchar.0
	ln ${DESTDIR}${MDIR}/putc.0 ${DESTDIR}${MDIR}/putw.0
	ln ${DESTDIR}${MDIR}/regex.0 ${DESTDIR}${MDIR}/re_comp.0
	ln ${DESTDIR}${MDIR}/regex.0 ${DESTDIR}${MDIR}/re_exec.0
	ln ${DESTDIR}${MDIR}/regexp.0 ${DESTDIR}${MDIR}/regcomp.0
	ln ${DESTDIR}${MDIR}/regexp.0 ${DESTDIR}${MDIR}/regexec.0
	ln ${DESTDIR}${MDIR}/regexp.0 ${DESTDIR}${MDIR}/regsub.0
	ln ${DESTDIR}${MDIR}/regexp.0 ${DESTDIR}${MDIR}/regerror.0
	ln ${DESTDIR}${MDIR}/ieee.0 ${DESTDIR}${MDIR}/scalb.0
	ln ${DESTDIR}${MDIR}/crypt.0 ${DESTDIR}${MDIR}/setkey.0
	ln ${DESTDIR}${MDIR}/plot.0 ${DESTDIR}${MDIR}/space.0
	ln ${DESTDIR}${MDIR}/rand.0 ${DESTDIR}${MDIR}/srand.0
	ln ${DESTDIR}${MDIR}/sinh.0 ${DESTDIR}${MDIR}/tanh.0
	ln ${DESTDIR}${MDIR}/ctime.0 ${DESTDIR}${MDIR}/timezone.0
	ln ${DESTDIR}${MDIR}/ctime.0 ${DESTDIR}${MDIR}/tzset.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/toascii.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/tolower.0
	ln ${DESTDIR}${MDIR}/ctype.0 ${DESTDIR}${MDIR}/toupper.0
	ln ${DESTDIR}${MDIR}/setjmp.0 ${DESTDIR}${MDIR}/_longjmp.0
	ln ${DESTDIR}${MDIR}/setjmp.0 ${DESTDIR}${MDIR}/_setjmp.0
	ln ${DESTDIR}${MDIR}/asinh.0 ${DESTDIR}${MDIR}/acosh.0
	ln ${DESTDIR}${MDIR}/malloc.0 ${DESTDIR}${MDIR}/alloca.0
	ln ${DESTDIR}${MDIR}/asinh.0 ${DESTDIR}${MDIR}/atanh.0
	ln ${DESTDIR}${MDIR}/hypot.0 ${DESTDIR}${MDIR}/cabs.0
	ln ${DESTDIR}${MDIR}/malloc.0 ${DESTDIR}${MDIR}/calloc.0
	ln ${DESTDIR}${MDIR}/floor.0 ${DESTDIR}${MDIR}/ceil.0
	ln ${DESTDIR}${MDIR}/syslog.0 ${DESTDIR}${MDIR}/closelog.0
	ln ${DESTDIR}${MDIR}/perror.0 ${DESTDIR}${MDIR}/errlist.0
	ln ${DESTDIR}${MDIR}/floor.0 ${DESTDIR}${MDIR}/fabs.0
	ln ${DESTDIR}${MDIR}/fopen.0 ${DESTDIR}${MDIR}/fdopen.0
	ln ${DESTDIR}${MDIR}/malloc.0 ${DESTDIR}${MDIR}/free.0
	ln ${DESTDIR}${MDIR}/fopen.0 ${DESTDIR}${MDIR}/freopen.0
	ln ${DESTDIR}${MDIR}/scanf.0 ${DESTDIR}${MDIR}/fscanf.0
	ln ${DESTDIR}${MDIR}/fseek.0 ${DESTDIR}${MDIR}/ftell.0
	ln ${DESTDIR}${MDIR}/fread.0 ${DESTDIR}${MDIR}/fwrite.0
	ln ${DESTDIR}${MDIR}/string.0 ${DESTDIR}${MDIR}/index.0
	ln ${DESTDIR}${MDIR}/random.0 ${DESTDIR}${MDIR}/initstate.0
	ln ${DESTDIR}${MDIR}/setjmp.0 ${DESTDIR}${MDIR}/longjmp.0
	ln ${DESTDIR}${MDIR}/syslog.0 ${DESTDIR}${MDIR}/openlog.0
	ln ${DESTDIR}${MDIR}/malloc.0 ${DESTDIR}${MDIR}/realloc.0
	ln ${DESTDIR}${MDIR}/insque.0 ${DESTDIR}${MDIR}/remque.0
	ln ${DESTDIR}${MDIR}/fseek.0 ${DESTDIR}${MDIR}/rewind.0
	ln ${DESTDIR}${MDIR}/string.0 ${DESTDIR}${MDIR}/rindex.0
	ln ${DESTDIR}${MDIR}/setuid.0 ${DESTDIR}${MDIR}/setegid.0
	ln ${DESTDIR}${MDIR}/setuid.0 ${DESTDIR}${MDIR}/seteuid.0
	ln ${DESTDIR}${MDIR}/setuid.0 ${DESTDIR}${MDIR}/setgid.0
	ln ${DESTDIR}${MDIR}/syslog.0 ${DESTDIR}${MDIR}/setlogmask.0
	ln ${DESTDIR}${MDIR}/setuid.0 ${DESTDIR}${MDIR}/setrgid.0
	ln ${DESTDIR}${MDIR}/setuid.0 ${DESTDIR}${MDIR}/setruid.0
	ln ${DESTDIR}${MDIR}/random.0 ${DESTDIR}${MDIR}/setstate.0
	ln ${DESTDIR}${MDIR}/random.0 ${DESTDIR}${MDIR}/srandom.0
	ln ${DESTDIR}${MDIR}/scanf.0 ${DESTDIR}${MDIR}/sscanf.0
	ln ${DESTDIR}${MDIR}/string.0 ${DESTDIR}${MDIR}/strcat.0
	ln ${DESTDIR}${MDIR}/string.0 ${DESTDIR}${MDIR}/strcmp.0
	ln ${DESTDIR}${MDIR}/string.0 ${DESTDIR}${MDIR}/strcpy.0
	ln ${DESTDIR}${MDIR}/string.0 ${DESTDIR}${MDIR}/strlen.0
	ln ${DESTDIR}${MDIR}/string.0 ${DESTDIR}${MDIR}/strncat.0
	ln ${DESTDIR}${MDIR}/string.0 ${DESTDIR}${MDIR}/strncmp.0
	ln ${DESTDIR}${MDIR}/string.0 ${DESTDIR}${MDIR}/strcasecmp.0
	ln ${DESTDIR}${MDIR}/string.0 ${DESTDIR}${MDIR}/strncasecmp.0
	ln ${DESTDIR}${MDIR}/string.0 ${DESTDIR}${MDIR}/strncpy.0
	ln ${DESTDIR}${MDIR}/perror.0 ${DESTDIR}${MDIR}/sys.0
	ln ${DESTDIR}${MDIR}/perror.0 ${DESTDIR}${MDIR}/sys_errlist.0
	ln ${DESTDIR}${MDIR}/perror.0 ${DESTDIR}${MDIR}/sys_nerr.0
	ln ${DESTDIR}${MDIR}/mktemp.0 ${DESTDIR}${MDIR}/mkstemp.0
	ln ${DESTDIR}${MDIR}/floor.0 ${DESTDIR}${MDIR}/rint.0
	ln ${DESTDIR}${MDIR}/getenv.0 ${DESTDIR}${MDIR}/setenv.0
	ln ${DESTDIR}${MDIR}/getenv.0 ${DESTDIR}${MDIR}/unsetenv.0
	ln ${DESTDIR}${MDIR}/scandir.0 ${DESTDIR}${MDIR}/alphasort.0
	ln ${DESTDIR}${MDIR}/bstring.0 ${DESTDIR}${MDIR}/bcmp.0
	ln ${DESTDIR}${MDIR}/bstring.0 ${DESTDIR}${MDIR}/bcopy.0
	ln ${DESTDIR}${MDIR}/bstring.0 ${DESTDIR}${MDIR}/bzero.0
	ln ${DESTDIR}${MDIR}/ferror.0 ${DESTDIR}${MDIR}/clearerr.0
	ln ${DESTDIR}${MDIR}/ferror.0 ${DESTDIR}${MDIR}/feof.0
	ln ${DESTDIR}${MDIR}/fclose.0 ${DESTDIR}${MDIR}/fflush.0
	ln ${DESTDIR}${MDIR}/bstring.0 ${DESTDIR}${MDIR}/ffs.0
	ln ${DESTDIR}${MDIR}/ferror.0 ${DESTDIR}${MDIR}/fileno.0
	ln ${DESTDIR}${MDIR}/printf.0 ${DESTDIR}${MDIR}/fprintf.0
	ln ${DESTDIR}${MDIR}/ttyname.0 ${DESTDIR}${MDIR}/isatty.0
	ln ${DESTDIR}${MDIR}/monitor.0 ${DESTDIR}${MDIR}/moncontrol.0
	ln ${DESTDIR}${MDIR}/monitor.0 ${DESTDIR}${MDIR}/monstartup.0
	ln ${DESTDIR}${MDIR}/setbuf.0 ${DESTDIR}${MDIR}/setbuffer.0
	ln ${DESTDIR}${MDIR}/setbuf.0 ${DESTDIR}${MDIR}/setlinebuf.0
	ln ${DESTDIR}${MDIR}/printf.0 ${DESTDIR}${MDIR}/sprintf.0
	ln ${DESTDIR}${MDIR}/psignal.0 ${DESTDIR}${MDIR}/sys_siglist.0
	ln ${DESTDIR}${MDIR}/ttyname.0 ${DESTDIR}${MDIR}/ttyslot.0
	ln ${DESTDIR}${MDIR}/lgamma.0 ${DESTDIR}${MDIR}/gamma.0
	ln ${DESTDIR}${MDIR}/getfsent.0 ${DESTDIR}${MDIR}/endfsent.0
	ln ${DESTDIR}${MDIR}/getgrent.0 ${DESTDIR}${MDIR}/endgrent.0
	ln ${DESTDIR}${MDIR}/getpwent.0 ${DESTDIR}${MDIR}/endpwent.0
	ln ${DESTDIR}${MDIR}/getfsent.0 ${DESTDIR}${MDIR}/getfsfile.0
	ln ${DESTDIR}${MDIR}/getfsent.0 ${DESTDIR}${MDIR}/getfsspec.0
	ln ${DESTDIR}${MDIR}/getfsent.0 ${DESTDIR}${MDIR}/getfstype.0
	ln ${DESTDIR}${MDIR}/getgrent.0 ${DESTDIR}${MDIR}/getgrgid.0
	ln ${DESTDIR}${MDIR}/getgrent.0 ${DESTDIR}${MDIR}/getgrnam.0
	ln ${DESTDIR}${MDIR}/getpwent.0 ${DESTDIR}${MDIR}/getpwnam.0
	ln ${DESTDIR}${MDIR}/getpwent.0 ${DESTDIR}${MDIR}/getpwuid.0
	ln ${DESTDIR}${MDIR}/getfsent.0 ${DESTDIR}${MDIR}/setfsent.0
	ln ${DESTDIR}${MDIR}/getgrent.0 ${DESTDIR}${MDIR}/setgrent.0
	ln ${DESTDIR}${MDIR}/getgrent.0 ${DESTDIR}${MDIR}/setgrfile.0
	ln ${DESTDIR}${MDIR}/getpwent.0 ${DESTDIR}${MDIR}/setpwent.0
	ln ${DESTDIR}${MDIR}/getpwent.0 ${DESTDIR}${MDIR}/setpwfile.0
	ln ${DESTDIR}${MDIR}/termcap.0 ${DESTDIR}${MDIR}/tgetent.0
	ln ${DESTDIR}${MDIR}/termcap.0 ${DESTDIR}${MDIR}/tgetflag.0
	ln ${DESTDIR}${MDIR}/termcap.0 ${DESTDIR}${MDIR}/tgetnum.0
	ln ${DESTDIR}${MDIR}/termcap.0 ${DESTDIR}${MDIR}/tgetstr.0
	ln ${DESTDIR}${MDIR}/termcap.0 ${DESTDIR}${MDIR}/tgoto.0
	ln ${DESTDIR}${MDIR}/termcap.0 ${DESTDIR}${MDIR}/tputs.0
	ln ${DESTDIR}${MDIR}/directory.0 ${DESTDIR}${MDIR}/closedir.0
	ln ${DESTDIR}${MDIR}/directory.0 ${DESTDIR}${MDIR}/dirfd.0
	ln ${DESTDIR}${MDIR}/getttyent.0 ${DESTDIR}${MDIR}/endttyent.0
	ln ${DESTDIR}${MDIR}/getttyent.0 ${DESTDIR}${MDIR}/getttynam.0
	ln ${DESTDIR}${MDIR}/directory.0 ${DESTDIR}${MDIR}/opendir.0
	ln ${DESTDIR}${MDIR}/directory.0 ${DESTDIR}${MDIR}/readdir.0
	ln ${DESTDIR}${MDIR}/directory.0 ${DESTDIR}${MDIR}/rewinddir.0
	ln ${DESTDIR}${MDIR}/directory.0 ${DESTDIR}${MDIR}/seekdir.0
	ln ${DESTDIR}${MDIR}/getttyent.0 ${DESTDIR}${MDIR}/setttyent.0
	ln ${DESTDIR}${MDIR}/directory.0 ${DESTDIR}${MDIR}/telldir.0
	ln ${DESTDIR}${MDIR}/getnetent.0 ${DESTDIR}${MDIR}/endnetent.0
	ln ${DESTDIR}${MDIR}/getnetent.0 ${DESTDIR}${MDIR}/getnetbyaddr.0
	ln ${DESTDIR}${MDIR}/getnetent.0 ${DESTDIR}${MDIR}/getnetbyname.0
	ln ${DESTDIR}${MDIR}/byteorder.0 ${DESTDIR}${MDIR}/htonl.0
	ln ${DESTDIR}${MDIR}/byteorder.0 ${DESTDIR}${MDIR}/htons.0
	ln ${DESTDIR}${MDIR}/byteorder.0 ${DESTDIR}${MDIR}/ntohl.0
	ln ${DESTDIR}${MDIR}/byteorder.0 ${DESTDIR}${MDIR}/ntohs.0
	ln ${DESTDIR}${MDIR}/getnetent.0 ${DESTDIR}${MDIR}/setnetent.0
	ln ${DESTDIR}${MDIR}/getservent.0 ${DESTDIR}${MDIR}/endservent.0
	ln ${DESTDIR}${MDIR}/getservent.0 ${DESTDIR}${MDIR}/getservbyname.0
	ln ${DESTDIR}${MDIR}/getservent.0 ${DESTDIR}${MDIR}/getservbyport.0
	ln ${DESTDIR}${MDIR}/getservent.0 ${DESTDIR}${MDIR}/setservent.0
	ln ${DESTDIR}${MDIR}/getprotoent.0 ${DESTDIR}${MDIR}/endprotoent.0
	ln ${DESTDIR}${MDIR}/getusershell.0 ${DESTDIR}${MDIR}/endusershell.0
	ln ${DESTDIR}${MDIR}/getprotoent.0 ${DESTDIR}${MDIR}/getprotobyname.0
	ln ${DESTDIR}${MDIR}/getprotoent.0 ${DESTDIR}${MDIR}/getprotobynumber.0
	ln ${DESTDIR}${MDIR}/getprotoent.0 ${DESTDIR}${MDIR}/setprotoent.0
	ln ${DESTDIR}${MDIR}/getusershell.0 ${DESTDIR}${MDIR}/setusershell.0
	ln ${DESTDIR}${MDIR}/gethostbyname.0 ${DESTDIR}${MDIR}/endhostent.0
	ln ${DESTDIR}${MDIR}/gethostbyname.0 ${DESTDIR}${MDIR}/gethostbyaddr.0
	ln ${DESTDIR}${MDIR}/gethostbyname.0 ${DESTDIR}${MDIR}/sethostent.0
	ln ${DESTDIR}${MDIR}/gethostbyname.0 ${DESTDIR}${MDIR}/sethostfile.0
	ln ${DESTDIR}${MDIR}/gethostbyname.0 ${DESTDIR}${MDIR}/gethostent.0
	ln ${DESTDIR}${MDIR}/treewalk.0 ${DESTDIR}${MDIR}/ftw.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/memccpy.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/memchr.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/memcmp.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/memcpy.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/memset.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/strchr.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/strcspn.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/strpbrk.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/strrchr.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/strspn.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/strtok.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/strtol.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/tmpnam.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/tmpfile.0
	ln ${DESTDIR}${MDIR}/compat-sys5.0 ${DESTDIR}${MDIR}/tempnam.0
