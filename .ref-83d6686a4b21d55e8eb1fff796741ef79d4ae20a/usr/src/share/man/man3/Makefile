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
#	@(#)Makefile	5.12 (Berkeley) %G%
#
MDIR=	${DESTDIR}/usr/man/cat3
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
	sqrt.0 stdio.0 string.0 strtok.0 stty.0 swab.0 syslog.0 system.0 \
	termcap.0 \
	time.0 times.0 treewalk.0 ttyname.0 ualarm.0 ungetc.0 usleep.0 \
	utime.0 valloc.0 varargs.0 vlimit.0 vtimes.0

all clean depend lint tags:

cleandir:
	rm -f ${OBJS}

install: ${OBJS}
	install -c -o bin -g bin -m 444 ${OBJS} ${MDIR}
	rm -f ${MDIR}/edata.0; ln ${MDIR}/end.0 ${MDIR}/edata.0
	rm -f ${MDIR}/etext.0; ln ${MDIR}/end.0 ${MDIR}/etext.0
	rm -f ${MDIR}/j1.0; ln ${MDIR}/j0.0 ${MDIR}/j1.0
	rm -f ${MDIR}/jn.0; ln ${MDIR}/j0.0 ${MDIR}/jn.0
	rm -f ${MDIR}/y0.0; ln ${MDIR}/j0.0 ${MDIR}/y0.0
	rm -f ${MDIR}/y1.0; ln ${MDIR}/j0.0 ${MDIR}/y1.0
	rm -f ${MDIR}/yn.0; ln ${MDIR}/j0.0 ${MDIR}/yn.0
	rm -f ${MDIR}/ns_addr.0; ln ${MDIR}/ns.0 ${MDIR}/ns_addr.0
	rm -f ${MDIR}/ns_ntoa.0; ln ${MDIR}/ns.0 ${MDIR}/ns_ntoa.0
	rm -f ${MDIR}/acos.0; ln ${MDIR}/sin.0 ${MDIR}/acos.0
	rm -f ${MDIR}/asin.0; ln ${MDIR}/sin.0 ${MDIR}/asin.0
	rm -f ${MDIR}/atan.0; ln ${MDIR}/sin.0 ${MDIR}/atan.0
	rm -f ${MDIR}/atan2.0; ln ${MDIR}/sin.0 ${MDIR}/atan2.0
	rm -f ${MDIR}/atoi.0; ln ${MDIR}/atof.0 ${MDIR}/atoi.0
	rm -f ${MDIR}/atol.0; ln ${MDIR}/atof.0 ${MDIR}/atol.0
	rm -f ${MDIR}/cos.0; ln ${MDIR}/sin.0 ${MDIR}/cos.0
	rm -f ${MDIR}/dbm_clearerr.0; ln ${MDIR}/ndbm.0 ${MDIR}/dbm_clearerr.0
	rm -f ${MDIR}/dbm_close.0; ln ${MDIR}/ndbm.0 ${MDIR}/dbm_close.0
	rm -f ${MDIR}/dbm_delete.0; ln ${MDIR}/ndbm.0 ${MDIR}/dbm_delete.0
	rm -f ${MDIR}/dbm_error.0; ln ${MDIR}/ndbm.0 ${MDIR}/dbm_error.0
	rm -f ${MDIR}/dbm_fetch.0; ln ${MDIR}/ndbm.0 ${MDIR}/dbm_fetch.0
	rm -f ${MDIR}/dbm_firstkey.0; ln ${MDIR}/ndbm.0 ${MDIR}/dbm_firstkey.0
	rm -f ${MDIR}/dbm_nextkey.0; ln ${MDIR}/ndbm.0 ${MDIR}/dbm_nextkey.0
	rm -f ${MDIR}/dbm_open.0; ln ${MDIR}/ndbm.0 ${MDIR}/dbm_open.0
	rm -f ${MDIR}/dbm_store.0; ln ${MDIR}/ndbm.0 ${MDIR}/dbm_store.0
	rm -f ${MDIR}/dbminit.0; ln ${MDIR}/dbm.0 ${MDIR}/dbminit.0
	rm -f ${MDIR}/delete.0; ln ${MDIR}/dbm.0 ${MDIR}/delete.0
	rm -f ${MDIR}/erfc.0; ln ${MDIR}/erf.0 ${MDIR}/erfc.0
	rm -f ${MDIR}/expm1.0; ln ${MDIR}/exp.0 ${MDIR}/expm1.0
	rm -f ${MDIR}/fcvt.0; ln ${MDIR}/ecvt.0 ${MDIR}/fcvt.0
	rm -f ${MDIR}/fetch.0; ln ${MDIR}/dbm.0 ${MDIR}/fetch.0
	rm -f ${MDIR}/firstkey.0; ln ${MDIR}/dbm.0 ${MDIR}/firstkey.0
	rm -f ${MDIR}/gcvt.0; ln ${MDIR}/ecvt.0 ${MDIR}/gcvt.0
	rm -f ${MDIR}/log.0; ln ${MDIR}/exp.0 ${MDIR}/log.0
	rm -f ${MDIR}/log10.0; ln ${MDIR}/exp.0 ${MDIR}/log10.0
	rm -f ${MDIR}/log1p.0; ln ${MDIR}/exp.0 ${MDIR}/log1p.0
	rm -f ${MDIR}/nextkey.0; ln ${MDIR}/dbm.0 ${MDIR}/nextkey.0
	rm -f ${MDIR}/pow.0; ln ${MDIR}/exp.0 ${MDIR}/pow.0
	rm -f ${MDIR}/rresvport.0; ln ${MDIR}/rcmd.0 ${MDIR}/rresvport.0
	rm -f ${MDIR}/ruserok.0; ln ${MDIR}/rcmd.0 ${MDIR}/ruserok.0
	rm -f ${MDIR}/store.0; ln ${MDIR}/dbm.0 ${MDIR}/store.0
	rm -f ${MDIR}/tan.0; ln ${MDIR}/sin.0 ${MDIR}/tan.0
	rm -f ${MDIR}/exec.0; ln ${MDIR}/execl.0 ${MDIR}/exec.0
	rm -f ${MDIR}/addr.0; ln ${MDIR}/inet.0 ${MDIR}/addr.0
	rm -f ${MDIR}/arc.0; ln ${MDIR}/plot.0 ${MDIR}/arc.0
	rm -f ${MDIR}/asctime.0; ln ${MDIR}/ctime.0 ${MDIR}/asctime.0
	rm -f ${MDIR}/cbrt.0; ln ${MDIR}/sqrt.0 ${MDIR}/cbrt.0
	rm -f ${MDIR}/circle.0; ln ${MDIR}/plot.0 ${MDIR}/circle.0
	rm -f ${MDIR}/cont.0; ln ${MDIR}/plot.0 ${MDIR}/cont.0
	rm -f ${MDIR}/copysign.0; ln ${MDIR}/ieee.0 ${MDIR}/copysign.0
	rm -f ${MDIR}/cosh.0; ln ${MDIR}/sinh.0 ${MDIR}/cosh.0
	rm -f ${MDIR}/drem.0; ln ${MDIR}/ieee.0 ${MDIR}/drem.0
	rm -f ${MDIR}/encrypt.0; ln ${MDIR}/crypt.0 ${MDIR}/encrypt.0
	rm -f ${MDIR}/erase.0; ln ${MDIR}/plot.0 ${MDIR}/erase.0
	rm -f ${MDIR}/execle.0; ln ${MDIR}/execl.0 ${MDIR}/execle.0
	rm -f ${MDIR}/execlp.0; ln ${MDIR}/execl.0 ${MDIR}/execlp.0
	rm -f ${MDIR}/exect.0; ln ${MDIR}/execl.0 ${MDIR}/exect.0
	rm -f ${MDIR}/execv.0; ln ${MDIR}/execl.0 ${MDIR}/execv.0
	rm -f ${MDIR}/execve.0; ln ${MDIR}/execl.0 ${MDIR}/execve.0
	rm -f ${MDIR}/execvp.0; ln ${MDIR}/execl.0 ${MDIR}/execvp.0
	rm -f ${MDIR}/fgetc.0; ln ${MDIR}/getc.0 ${MDIR}/fgetc.0
	rm -f ${MDIR}/fgets.0; ln ${MDIR}/gets.0 ${MDIR}/fgets.0
	rm -f ${MDIR}/finite.0; ln ${MDIR}/ieee.0 ${MDIR}/finite.0
	rm -f ${MDIR}/fputc.0; ln ${MDIR}/putc.0 ${MDIR}/fputc.0
	rm -f ${MDIR}/fputs.0; ln ${MDIR}/puts.0 ${MDIR}/fputs.0
	rm -f ${MDIR}/ftime.0; ln ${MDIR}/time.0 ${MDIR}/ftime.0
	rm -f ${MDIR}/getchar.0; ln ${MDIR}/getc.0 ${MDIR}/getchar.0
	rm -f ${MDIR}/getw.0; ln ${MDIR}/getc.0 ${MDIR}/getw.0
	rm -f ${MDIR}/gmtime.0; ln ${MDIR}/ctime.0 ${MDIR}/gmtime.0
	rm -f ${MDIR}/gtty.0; ln ${MDIR}/stty.0 ${MDIR}/gtty.0
	rm -f ${MDIR}/inet_addr.0; ln ${MDIR}/inet.0 ${MDIR}/inet_addr.0
	rm -f ${MDIR}/inet_lnaof.0; ln ${MDIR}/inet.0 ${MDIR}/inet_lnaof.0
	rm -f ${MDIR}/inet_makeaddr.0; ln ${MDIR}/inet.0 ${MDIR}/inet_makeaddr.0
	rm -f ${MDIR}/inet_netof.0; ln ${MDIR}/inet.0 ${MDIR}/inet_netof.0
	rm -f ${MDIR}/inet_network.0; ln ${MDIR}/inet.0 ${MDIR}/inet_network.0
	rm -f ${MDIR}/inet_ntoa.0; ln ${MDIR}/inet.0 ${MDIR}/inet_ntoa.0
	rm -f ${MDIR}/isalnum.0; ln ${MDIR}/ctype.0 ${MDIR}/isalnum.0
	rm -f ${MDIR}/isalpha.0; ln ${MDIR}/ctype.0 ${MDIR}/isalpha.0
	rm -f ${MDIR}/isascii.0; ln ${MDIR}/ctype.0 ${MDIR}/isascii.0
	rm -f ${MDIR}/iscntrl.0; ln ${MDIR}/ctype.0 ${MDIR}/iscntrl.0
	rm -f ${MDIR}/isdigit.0; ln ${MDIR}/ctype.0 ${MDIR}/isdigit.0
	rm -f ${MDIR}/islower.0; ln ${MDIR}/ctype.0 ${MDIR}/islower.0
	rm -f ${MDIR}/isprint.0; ln ${MDIR}/ctype.0 ${MDIR}/isprint.0
	rm -f ${MDIR}/ispunct.0; ln ${MDIR}/ctype.0 ${MDIR}/ispunct.0
	rm -f ${MDIR}/isspace.0; ln ${MDIR}/ctype.0 ${MDIR}/isspace.0
	rm -f ${MDIR}/isupper.0; ln ${MDIR}/ctype.0 ${MDIR}/isupper.0
	rm -f ${MDIR}/isxdigit.0; ln ${MDIR}/ctype.0 ${MDIR}/isxdigit.0
	rm -f ${MDIR}/label.0; ln ${MDIR}/plot.0 ${MDIR}/label.0
	rm -f ${MDIR}/ldexp.0; ln ${MDIR}/frexp.0 ${MDIR}/ldexp.0
	rm -f ${MDIR}/line.0; ln ${MDIR}/plot.0 ${MDIR}/line.0
	rm -f ${MDIR}/linemod.0; ln ${MDIR}/plot.0 ${MDIR}/linemod.0
	rm -f ${MDIR}/localtime.0; ln ${MDIR}/ctime.0 ${MDIR}/localtime.0
	rm -f ${MDIR}/logb.0; ln ${MDIR}/ieee.0 ${MDIR}/logb.0
	rm -f ${MDIR}/modf.0; ln ${MDIR}/frexp.0 ${MDIR}/modf.0
	rm -f ${MDIR}/move.0; ln ${MDIR}/plot.0 ${MDIR}/move.0
	rm -f ${MDIR}/network.0; ln ${MDIR}/inet.0 ${MDIR}/network.0
	rm -f ${MDIR}/ntoa.0; ln ${MDIR}/inet.0 ${MDIR}/ntoa.0
	rm -f ${MDIR}/openpl.0; ln ${MDIR}/plot.0 ${MDIR}/openpl.0
	rm -f ${MDIR}/pclose.0; ln ${MDIR}/popen.0 ${MDIR}/pclose.0
	rm -f ${MDIR}/point.0; ln ${MDIR}/plot.0 ${MDIR}/point.0
	rm -f ${MDIR}/putchar.0; ln ${MDIR}/putc.0 ${MDIR}/putchar.0
	rm -f ${MDIR}/putw.0; ln ${MDIR}/putc.0 ${MDIR}/putw.0
	rm -f ${MDIR}/re_comp.0; ln ${MDIR}/regex.0 ${MDIR}/re_comp.0
	rm -f ${MDIR}/re_exec.0; ln ${MDIR}/regex.0 ${MDIR}/re_exec.0
	rm -f ${MDIR}/regcomp.0; ln ${MDIR}/regexp.0 ${MDIR}/regcomp.0
	rm -f ${MDIR}/regexec.0; ln ${MDIR}/regexp.0 ${MDIR}/regexec.0
	rm -f ${MDIR}/regsub.0; ln ${MDIR}/regexp.0 ${MDIR}/regsub.0
	rm -f ${MDIR}/regerror.0; ln ${MDIR}/regexp.0 ${MDIR}/regerror.0
	rm -f ${MDIR}/scalb.0; ln ${MDIR}/ieee.0 ${MDIR}/scalb.0
	rm -f ${MDIR}/setkey.0; ln ${MDIR}/crypt.0 ${MDIR}/setkey.0
	rm -f ${MDIR}/space.0; ln ${MDIR}/plot.0 ${MDIR}/space.0
	rm -f ${MDIR}/srand.0; ln ${MDIR}/rand.0 ${MDIR}/srand.0
	rm -f ${MDIR}/tanh.0; ln ${MDIR}/sinh.0 ${MDIR}/tanh.0
	rm -f ${MDIR}/timezone.0; ln ${MDIR}/ctime.0 ${MDIR}/timezone.0
	rm -f ${MDIR}/tzset.0; ln ${MDIR}/ctime.0 ${MDIR}/tzset.0
	rm -f ${MDIR}/toascii.0; ln ${MDIR}/ctype.0 ${MDIR}/toascii.0
	rm -f ${MDIR}/tolower.0; ln ${MDIR}/ctype.0 ${MDIR}/tolower.0
	rm -f ${MDIR}/toupper.0; ln ${MDIR}/ctype.0 ${MDIR}/toupper.0
	rm -f ${MDIR}/_longjmp.0; ln ${MDIR}/setjmp.0 ${MDIR}/_longjmp.0
	rm -f ${MDIR}/_setjmp.0; ln ${MDIR}/setjmp.0 ${MDIR}/_setjmp.0
	rm -f ${MDIR}/acosh.0; ln ${MDIR}/asinh.0 ${MDIR}/acosh.0
	rm -f ${MDIR}/alloca.0; ln ${MDIR}/malloc.0 ${MDIR}/alloca.0
	rm -f ${MDIR}/atanh.0; ln ${MDIR}/asinh.0 ${MDIR}/atanh.0
	rm -f ${MDIR}/cabs.0; ln ${MDIR}/hypot.0 ${MDIR}/cabs.0
	rm -f ${MDIR}/calloc.0; ln ${MDIR}/malloc.0 ${MDIR}/calloc.0
	rm -f ${MDIR}/ceil.0; ln ${MDIR}/floor.0 ${MDIR}/ceil.0
	rm -f ${MDIR}/closelog.0; ln ${MDIR}/syslog.0 ${MDIR}/closelog.0
	rm -f ${MDIR}/fabs.0; ln ${MDIR}/floor.0 ${MDIR}/fabs.0
	rm -f ${MDIR}/fdopen.0; ln ${MDIR}/fopen.0 ${MDIR}/fdopen.0
	rm -f ${MDIR}/free.0; ln ${MDIR}/malloc.0 ${MDIR}/free.0
	rm -f ${MDIR}/freopen.0; ln ${MDIR}/fopen.0 ${MDIR}/freopen.0
	rm -f ${MDIR}/fscanf.0; ln ${MDIR}/scanf.0 ${MDIR}/fscanf.0
	rm -f ${MDIR}/ftell.0; ln ${MDIR}/fseek.0 ${MDIR}/ftell.0
	rm -f ${MDIR}/fwrite.0; ln ${MDIR}/fread.0 ${MDIR}/fwrite.0
	rm -f ${MDIR}/index.0; ln ${MDIR}/string.0 ${MDIR}/index.0
	rm -f ${MDIR}/initstate.0; ln ${MDIR}/random.0 ${MDIR}/initstate.0
	rm -f ${MDIR}/longjmp.0; ln ${MDIR}/setjmp.0 ${MDIR}/longjmp.0
	rm -f ${MDIR}/openlog.0; ln ${MDIR}/syslog.0 ${MDIR}/openlog.0
	rm -f ${MDIR}/realloc.0; ln ${MDIR}/malloc.0 ${MDIR}/realloc.0
	rm -f ${MDIR}/remque.0; ln ${MDIR}/insque.0 ${MDIR}/remque.0
	rm -f ${MDIR}/rewind.0; ln ${MDIR}/fseek.0 ${MDIR}/rewind.0
	rm -f ${MDIR}/rindex.0; ln ${MDIR}/string.0 ${MDIR}/rindex.0
	rm -f ${MDIR}/setegid.0; ln ${MDIR}/setuid.0 ${MDIR}/setegid.0
	rm -f ${MDIR}/seteuid.0; ln ${MDIR}/setuid.0 ${MDIR}/seteuid.0
	rm -f ${MDIR}/setgid.0; ln ${MDIR}/setuid.0 ${MDIR}/setgid.0
	rm -f ${MDIR}/setlogmask.0; ln ${MDIR}/syslog.0 ${MDIR}/setlogmask.0
	rm -f ${MDIR}/vsyslog.0; ln ${MDIR}/syslog.0 ${MDIR}/vsyslog.0
	rm -f ${MDIR}/setrgid.0; ln ${MDIR}/setuid.0 ${MDIR}/setrgid.0
	rm -f ${MDIR}/setruid.0; ln ${MDIR}/setuid.0 ${MDIR}/setruid.0
	rm -f ${MDIR}/setstate.0; ln ${MDIR}/random.0 ${MDIR}/setstate.0
	rm -f ${MDIR}/srandom.0; ln ${MDIR}/random.0 ${MDIR}/srandom.0
	rm -f ${MDIR}/sscanf.0; ln ${MDIR}/scanf.0 ${MDIR}/sscanf.0
	rm -f ${MDIR}/strcat.0; ln ${MDIR}/string.0 ${MDIR}/strcat.0
	rm -f ${MDIR}/strcmp.0; ln ${MDIR}/string.0 ${MDIR}/strcmp.0
	rm -f ${MDIR}/strcpy.0; ln ${MDIR}/string.0 ${MDIR}/strcpy.0
	rm -f ${MDIR}/strlen.0; ln ${MDIR}/string.0 ${MDIR}/strlen.0
	rm -f ${MDIR}/strncat.0; ln ${MDIR}/string.0 ${MDIR}/strncat.0
	rm -f ${MDIR}/strncmp.0; ln ${MDIR}/string.0 ${MDIR}/strncmp.0
	rm -f ${MDIR}/strcasecmp.0; ln ${MDIR}/string.0 ${MDIR}/strcasecmp.0
	rm -f ${MDIR}/strncasecmp.0; ln ${MDIR}/string.0 ${MDIR}/strncasecmp.0
	rm -f ${MDIR}/strncpy.0; ln ${MDIR}/string.0 ${MDIR}/strncpy.0
	rm -f ${MDIR}/strerror.0; ln ${MDIR}/perror.0 ${MDIR}/strerror.0
	rm -f ${MDIR}/sys_errlist.0; ln ${MDIR}/perror.0 ${MDIR}/sys_errlist.0
	rm -f ${MDIR}/sys_nerr.0; ln ${MDIR}/perror.0 ${MDIR}/sys_nerr.0
	rm -f ${MDIR}/mkstemp.0; ln ${MDIR}/mktemp.0 ${MDIR}/mkstemp.0
	rm -f ${MDIR}/rint.0; ln ${MDIR}/floor.0 ${MDIR}/rint.0
	rm -f ${MDIR}/setenv.0; ln ${MDIR}/getenv.0 ${MDIR}/setenv.0
	rm -f ${MDIR}/unsetenv.0; ln ${MDIR}/getenv.0 ${MDIR}/unsetenv.0
	rm -f ${MDIR}/putenv.0; ln ${MDIR}/getenv.0 ${MDIR}/putenv.0
	rm -f ${MDIR}/alphasort.0; ln ${MDIR}/scandir.0 ${MDIR}/alphasort.0
	rm -f ${MDIR}/bcmp.0; ln ${MDIR}/bstring.0 ${MDIR}/bcmp.0
	rm -f ${MDIR}/bcopy.0; ln ${MDIR}/bstring.0 ${MDIR}/bcopy.0
	rm -f ${MDIR}/bzero.0; ln ${MDIR}/bstring.0 ${MDIR}/bzero.0
	rm -f ${MDIR}/clearerr.0; ln ${MDIR}/ferror.0 ${MDIR}/clearerr.0
	rm -f ${MDIR}/feof.0; ln ${MDIR}/ferror.0 ${MDIR}/feof.0
	rm -f ${MDIR}/fflush.0; ln ${MDIR}/fclose.0 ${MDIR}/fflush.0
	rm -f ${MDIR}/ffs.0; ln ${MDIR}/bstring.0 ${MDIR}/ffs.0
	rm -f ${MDIR}/fileno.0; ln ${MDIR}/ferror.0 ${MDIR}/fileno.0
	rm -f ${MDIR}/isatty.0; ln ${MDIR}/ttyname.0 ${MDIR}/isatty.0
	rm -f ${MDIR}/moncontrol.0; ln ${MDIR}/monitor.0 ${MDIR}/moncontrol.0
	rm -f ${MDIR}/monstartup.0; ln ${MDIR}/monitor.0 ${MDIR}/monstartup.0
	rm -f ${MDIR}/setbuffer.0; ln ${MDIR}/setbuf.0 ${MDIR}/setbuffer.0
	rm -f ${MDIR}/setlinebuf.0; ln ${MDIR}/setbuf.0 ${MDIR}/setlinebuf.0
	rm -f ${MDIR}/sprintf.0; ln ${MDIR}/printf.0 ${MDIR}/sprintf.0
	rm -f ${MDIR}/fprintf.0; ln ${MDIR}/printf.0 ${MDIR}/fprintf.0
	rm -f ${MDIR}/vprintf.0; ln ${MDIR}/printf.0 ${MDIR}/vprintf.0
	rm -f ${MDIR}/vsprintf.0; ln ${MDIR}/printf.0 ${MDIR}/vsprintf.0
	rm -f ${MDIR}/vfprintf.0; ln ${MDIR}/printf.0 ${MDIR}/vfprintf.0
	rm -f ${MDIR}/sys_siglist.0; ln ${MDIR}/psignal.0 ${MDIR}/sys_siglist.0
	rm -f ${MDIR}/ttyslot.0; ln ${MDIR}/ttyname.0 ${MDIR}/ttyslot.0
	rm -f ${MDIR}/gamma.0; ln ${MDIR}/lgamma.0 ${MDIR}/gamma.0
	rm -f ${MDIR}/endfsent.0; ln ${MDIR}/getfsent.0 ${MDIR}/endfsent.0
	rm -f ${MDIR}/endgrent.0; ln ${MDIR}/getgrent.0 ${MDIR}/endgrent.0
	rm -f ${MDIR}/endpwent.0; ln ${MDIR}/getpwent.0 ${MDIR}/endpwent.0
	rm -f ${MDIR}/getfsfile.0; ln ${MDIR}/getfsent.0 ${MDIR}/getfsfile.0
	rm -f ${MDIR}/getfsspec.0; ln ${MDIR}/getfsent.0 ${MDIR}/getfsspec.0
	rm -f ${MDIR}/getfstype.0; ln ${MDIR}/getfsent.0 ${MDIR}/getfstype.0
	rm -f ${MDIR}/getgrgid.0; ln ${MDIR}/getgrent.0 ${MDIR}/getgrgid.0
	rm -f ${MDIR}/getgrnam.0; ln ${MDIR}/getgrent.0 ${MDIR}/getgrnam.0
	rm -f ${MDIR}/getpwnam.0; ln ${MDIR}/getpwent.0 ${MDIR}/getpwnam.0
	rm -f ${MDIR}/getpwuid.0; ln ${MDIR}/getpwent.0 ${MDIR}/getpwuid.0
	rm -f ${MDIR}/setfsent.0; ln ${MDIR}/getfsent.0 ${MDIR}/setfsent.0
	rm -f ${MDIR}/setgrent.0; ln ${MDIR}/getgrent.0 ${MDIR}/setgrent.0
	rm -f ${MDIR}/setgrfile.0; ln ${MDIR}/getgrent.0 ${MDIR}/setgrfile.0
	rm -f ${MDIR}/setpwent.0; ln ${MDIR}/getpwent.0 ${MDIR}/setpwent.0
	rm -f ${MDIR}/setpwfile.0; ln ${MDIR}/getpwent.0 ${MDIR}/setpwfile.0
	rm -f ${MDIR}/tgetent.0; ln ${MDIR}/termcap.0 ${MDIR}/tgetent.0
	rm -f ${MDIR}/tgetflag.0; ln ${MDIR}/termcap.0 ${MDIR}/tgetflag.0
	rm -f ${MDIR}/tgetnum.0; ln ${MDIR}/termcap.0 ${MDIR}/tgetnum.0
	rm -f ${MDIR}/tgetstr.0; ln ${MDIR}/termcap.0 ${MDIR}/tgetstr.0
	rm -f ${MDIR}/tgoto.0; ln ${MDIR}/termcap.0 ${MDIR}/tgoto.0
	rm -f ${MDIR}/tputs.0; ln ${MDIR}/termcap.0 ${MDIR}/tputs.0
	rm -f ${MDIR}/closedir.0; ln ${MDIR}/directory.0 ${MDIR}/closedir.0
	rm -f ${MDIR}/dirfd.0; ln ${MDIR}/directory.0 ${MDIR}/dirfd.0
	rm -f ${MDIR}/endttyent.0; ln ${MDIR}/getttyent.0 ${MDIR}/endttyent.0
	rm -f ${MDIR}/getttynam.0; ln ${MDIR}/getttyent.0 ${MDIR}/getttynam.0
	rm -f ${MDIR}/opendir.0; ln ${MDIR}/directory.0 ${MDIR}/opendir.0
	rm -f ${MDIR}/readdir.0; ln ${MDIR}/directory.0 ${MDIR}/readdir.0
	rm -f ${MDIR}/rewinddir.0; ln ${MDIR}/directory.0 ${MDIR}/rewinddir.0
	rm -f ${MDIR}/seekdir.0; ln ${MDIR}/directory.0 ${MDIR}/seekdir.0
	rm -f ${MDIR}/setttyent.0; ln ${MDIR}/getttyent.0 ${MDIR}/setttyent.0
	rm -f ${MDIR}/telldir.0; ln ${MDIR}/directory.0 ${MDIR}/telldir.0
	rm -f ${MDIR}/endnetent.0; ln ${MDIR}/getnetent.0 ${MDIR}/endnetent.0
	rm -f ${MDIR}/getnetbyaddr.0; ln ${MDIR}/getnetent.0 ${MDIR}/getnetbyaddr.0
	rm -f ${MDIR}/getnetbyname.0; ln ${MDIR}/getnetent.0 ${MDIR}/getnetbyname.0
	rm -f ${MDIR}/htonl.0; ln ${MDIR}/byteorder.0 ${MDIR}/htonl.0
	rm -f ${MDIR}/htons.0; ln ${MDIR}/byteorder.0 ${MDIR}/htons.0
	rm -f ${MDIR}/ntohl.0; ln ${MDIR}/byteorder.0 ${MDIR}/ntohl.0
	rm -f ${MDIR}/ntohs.0; ln ${MDIR}/byteorder.0 ${MDIR}/ntohs.0
	rm -f ${MDIR}/setnetent.0; ln ${MDIR}/getnetent.0 ${MDIR}/setnetent.0
	rm -f ${MDIR}/endservent.0; ln ${MDIR}/getservent.0 ${MDIR}/endservent.0
	rm -f ${MDIR}/getservbyname.0; ln ${MDIR}/getservent.0 ${MDIR}/getservbyname.0
	rm -f ${MDIR}/getservbyport.0; ln ${MDIR}/getservent.0 ${MDIR}/getservbyport.0
	rm -f ${MDIR}/setservent.0; ln ${MDIR}/getservent.0 ${MDIR}/setservent.0
	rm -f ${MDIR}/endprotoent.0; ln ${MDIR}/getprotoent.0 ${MDIR}/endprotoent.0
	rm -f ${MDIR}/endusershell.0; ln ${MDIR}/getusershell.0 ${MDIR}/endusershell.0
	rm -f ${MDIR}/getprotobyname.0; ln ${MDIR}/getprotoent.0 ${MDIR}/getprotobyname.0
	rm -f ${MDIR}/getprotobynumber.0; ln ${MDIR}/getprotoent.0 ${MDIR}/getprotobynumber.0
	rm -f ${MDIR}/setprotoent.0; ln ${MDIR}/getprotoent.0 ${MDIR}/setprotoent.0
	rm -f ${MDIR}/setusershell.0; ln ${MDIR}/getusershell.0 ${MDIR}/setusershell.0
	rm -f ${MDIR}/endhostent.0; ln ${MDIR}/gethostbyname.0 ${MDIR}/endhostent.0
	rm -f ${MDIR}/gethostbyaddr.0; ln ${MDIR}/gethostbyname.0 ${MDIR}/gethostbyaddr.0
	rm -f ${MDIR}/sethostent.0; ln ${MDIR}/gethostbyname.0 ${MDIR}/sethostent.0
	rm -f ${MDIR}/sethostfile.0; ln ${MDIR}/gethostbyname.0 ${MDIR}/sethostfile.0
	rm -f ${MDIR}/gethostent.0; ln ${MDIR}/gethostbyname.0 ${MDIR}/gethostent.0
	rm -f ${MDIR}/herror.0; ln ${MDIR}/gethostbyname.0 ${MDIR}/herror.0
	rm -f ${MDIR}/ftw.0; ln ${MDIR}/treewalk.0 ${MDIR}/ftw.0
	rm -f ${MDIR}/memccpy.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/memccpy.0
	rm -f ${MDIR}/memchr.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/memchr.0
	rm -f ${MDIR}/memcmp.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/memcmp.0
	rm -f ${MDIR}/memcpy.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/memcpy.0
	rm -f ${MDIR}/memset.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/memset.0
	rm -f ${MDIR}/strchr.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/strchr.0
	rm -f ${MDIR}/strcspn.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/strcspn.0
	rm -f ${MDIR}/strpbrk.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/strpbrk.0
	rm -f ${MDIR}/strrchr.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/strrchr.0
	rm -f ${MDIR}/strspn.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/strspn.0
	rm -f ${MDIR}/strdup.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/strdup.0
	rm -f ${MDIR}/strsep.0; ln ${MDIR}/strtok.0 ${MDIR}/strsep.0
	rm -f ${MDIR}/strtol.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/strtol.0
	rm -f ${MDIR}/tmpnam.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/tmpnam.0
	rm -f ${MDIR}/tmpfile.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/tmpfile.0
	rm -f ${MDIR}/tempnam.0; ln ${MDIR}/compat-sys5.0 ${MDIR}/tempnam.0
