#	@(#)Makefile.inc	5.11 (Berkeley) %G%

# gen sources
.PATH: ${.CURDIR}/${MACHINE}/gen ${.CURDIR}/gen ${.CURDIR}/gen/regexp

SRCS+=	alarm.c clock.c closedir.c crypt.c ctime.c ctype_.c difftime.c \
	disklabel.c errlst.c execvp.c fakcu.c fnmatch.c fstab.c fts.c \
	getgrent.c getlogin.c getmntinfo.c getopt.c getpass.c \
	getpwent.c getttyent.c getusershell.c getwd.c glob.c initgroups.c \
	isatty.c isctype.c mktemp.c ndbm.c nice.c nlist.c opendir.c \
	pause.c perror.c popen.c psignal.c random.c readdir.c \
	regex.c remove.c rewinddir.c scandir.c seekdir.c setjmperr.c \
	setmode.c setrgid.c setruid.c siginterrupt.c sigsetjmp.c \
	siglist.c signal.c sigsetops.c sleep.c syslog.c telldir.c \
	termios.c time.c times.c timezone.c tmpnam.c ttyname.c ttyslot.c \
	ualarm.c unvis.c usleep.c utime.c valloc.c vis.c wait.c wait3.c \
	waitpid.c

SRCS+=	regerror.c regexp.c regsub.c

.if   (${MACHINE} == "hp300")
SRCS+=	_setjmp.s alloca.s fabs.s frexp.s insque.s ldexp.s modf.s remque.s \
	setjmp.s
SRCS+=	adddf3.s addsf3.s ashlsi3.s ashrsi3.s cmpdf2.s cmpsf2.s divdf3.s \
	divsf3.s divsi3.s eprintf.c extendsfdf2.s fixdfsi.s fixunsdfsi.s \
	floatsidf.s lshlsi3.s lshrsi3.s modsi3.s muldf3.s mulsf3.s mulsi3.s \
	negdf2.s negsf2.s saveregs.c subdf3.s subsf3.s truncdfsf2.s udivsi3.s \
	umodsi3.s umulsi3.s
.elif (${MACHINE} == "i386")
SRCS+=	_setjmp.s alloca.s fabs.s frexp.c insque.c ldexp.c modf.s remque.c \
	setjmp.s
SRCS+=	divsi3.s fixdfsi.s fixunsdfsi.s udivsi3.s
.elif (${MACHINE} == "tahoe")
SRCS+=	_setjmp.s alloca.s fabs.s frexp.c insque.s ldexp.s modf.s remque.s \
	setjmp.s
SRCS+=	udiv.s urem.s
.elif (${MACHINE} == "vax")
SRCS+=	_setjmp.s alloca.s fabs.s frexp.s insque.s ldexp.s modf.s remque.s \
	setjmp.s
SRCS+=	udiv.s urem.s
.endif

MAN3+=	alarm.0 clock.0 crypt.0 ctime.0 ctype.0 directory.0 execvp.0 \
	fnmatch.0 frexp.0 fts.0 getdiskbyname.0 getfsent.0 \
	getgrent.0 getmntinfo.0 getopt.0 getpass.0 getpwent.0 \
	getttyent.0 getusershell.0 getwd.0 glob.0 initgroups.0 insque.0 \
	mktemp.0 ndbm.0 nice.0 nlist.0 pause.0 perror.0 popen.0 psignal.0 \
	random.0 regex.0 regexp.0 scandir.0 setjmp.0 setmode.0 \
	setuid.0 siginterrupt.0 signal.0 sigsetops.0 sleep.0 syslog.0 time.0 \
	times.0 tmpnam.0 ttyname.0 tzset.0 ualarm.0 unvis.0 usleep.0 utime.0 \
	valloc.0 vis.0

MLINKS=	crypt.3 encrypt.3 crypt.3 setkey.3
MLINKS+=ctime.3 asctime.3 ctime.3 difftime.3 ctime.3 gmtime.3 \
	ctime.3 localtime.3 ctime.3 mktime.3 ctime.3 timezone.3 
MLINKS+=ctype.3 toascii.3 ctype.3 tolower.3 ctype.3 toupper.3 \
	ctype.3 isalnum.3 ctype.3 isalpha.3 ctype.3 isascii.3 \
	ctype.3 iscntrl.3 ctype.3 isdigit.3 ctype.3 isgraph.3 \
	ctype.3 islower.3 ctype.3 isprint.3 ctype.3 ispunct.3 \
	ctype.3 isspace.3 ctype.3 isupper.3 ctype.3 isxdigit.3
MLINKS+=directory.3 closedir.3 directory.3 dirfd.3 directory.3 opendir.3 \
	directory.3 readdir.3 directory.3 rewinddir.3 directory.3 seekdir.3 \
	directory.3 telldir.3
MLINKS+=execvp.3 execle.3 execvp.3 execlp.3 execvp.3 exect.3 \
	execvp.3 execv.3 execvp.3 execve.3 execvp.3 execl.3 \
	execvp.3 exec.3
MLINKS+=frexp.3 ldexp.3 frexp.3 modf.3
MLINKS+=getfsent.3 endfsent.3 getfsent.3 getfsfile.3 getfsent.3 getfsspec.3 \
	getfsent.3 getfstype.3 getfsent.3 setfsent.3
MLINKS+=getgrent.3 endgrent.3 getgrent.3 setgroupent.3 getgrent.3 getgrgid.3 \
	getgrent.3 getgrnam.3 getgrent.3 setgrent.3 getgrent.3 setgrfile.3
MLINKS+=getpwent.3 endpwent.3 getpwent.3 setpassent.3 getpwent.3 getpwnam.3 \
	getpwent.3 getpwuid.3 getpwent.3 setpwent.3 getpwent.3 setpwfile.3
MLINKS+=getttyent.3 endttyent.3 getttyent.3 getttynam.3 getttyent.3 setttyent.3
MLINKS+=getusershell.3 endusershell.3 getusershell.3 setusershell.3
MLINKS+=glob.3 globfree.3
MLINKS+=insque.3 remque.3
MLINKS+=mktemp.3 mkstemp.3
MLINKS+=ndbm.3 dbm_clearerr.3 ndbm.3 dbm_close.3 ndbm.3 dbm_delete.3 \
	ndbm.3 dbm_error.3 ndbm.3 dbm_fetch.3 ndbm.3 dbm_firstkey.3 \
	ndbm.3 dbm_nextkey.3 ndbm.3 dbm_open.3 ndbm.3 dbm_store.3
MLINKS+=perror.3 strerror.3 perror.3 sys_errlist.3 perror.3 sys_nerr.3
MLINKS+=popen.3 pclose.3
MLINKS+=psignal.3 sys_siglist.3
MLINKS+=random.3 initstate.3 random.3 setstate.3 random.3 srandom.3
MLINKS+=regex.3 re_comp.3 regex.3 re_exec.3 regexp.3 regcomp.3 \
	regexp.3 regexec.3 regexp.3 regsub.3 regexp.3 regerror.3
MLINKS+=scandir.3 alphasort.3
MLINKS+=setjmp.3 _longjmp.3 setjmp.3 _setjmp.3 setjmp.3 longjmp.3 \
	setjmp.3 longjmperr.3
MLINKS+=setmode.3 getmode.3
MLINKS+=setuid.3 setegid.3 setuid.3 seteuid.3 setuid.3 setgid.3 \
	setuid.3 setrgid.3 setuid.3 setruid.3
MLINKS+=sigsetops.3 sigemptyset.3 sigsetops.3 sigfillset.3 \
	sigsetops.3 sigaddset.3 sigsetops.3 sigdelset.3 \
	sigsetops.3 sigismember.3
MLINKS+=syslog.3 closelog.3 syslog.3 openlog.3 syslog.3 setlogmask.3 \
	syslog.3 vsyslog.3
MLINKS+=tmpnam.3 tmpfile.3 tmpnam.3 tempnam.3
MLINKS+=ttyname.3 isatty.3 ttyname.3 ttyslot.3
MLINKS+=tzset.3 tzsetwall.3

errlst.o:
	${CC} -S ${CFLAGS} ${.IMPSRC}
	ed - < ${.CURDIR}/${MACHINE}/:errfix errlst.s
	${AS} -o errlst.o errlst.s
	cp errlst.o errlst.po
	rm -f errlst.s
