#	@(#)Makefile	4.37 (Berkeley) %G%
#
# All library objects contain sccsid strings by default; they may be
# excluded as a space-saving measure.  To produce a library that does
# not contain these strings, delete -DLIBC_SCCS and -DSYSLIBC_SCCS
# from CFLAGS below.  To remove these strings from just the system call
# stubs, remove just -DSYSLIBC_SCCS from CFLAGS.
LIB=c
CFLAGS+=-DLIBC_SCCS -DSYSLIBC_SCCS
AINC=	-I${.CURDIR}/${MACHINE} -I${.CURDIR}/sys/${MACHINE}

.PATH:	${.CURDIR}/gen/${MACHINE} ${.CURDIR}/gen ${.CURDIR}/gen/regexp \
	${.CURDIR}/sys/${MACHINE} ${.CURDIR}/sys \
	${.CURDIR}/stdio/${MACHINE} ${.CURDIR}/stdio \
	${.CURDIR}/net/${MACHINE} ${.CURDIR}/net \
	${.CURDIR}/string/${MACHINE} ${.CURDIR}/string \
	${.CURDIR}/sys5/${MACHINE} ${.CURDIR}/sys5 \
	${.CURDIR}/compat-43/${MACHINE} ${.CURDIR}/compat-43

# compat-43 sources
SRCS=	creat.c killpg.c sigcompat.c

MAN2+=	creat.0 killpg.0 sigblock.0 sigpause.0 sigsetmask.0 sigvec.0

# gen sources
SRCS+=	_setjmp.s abort.c abs.s alarm.c alloca.s calloc.c cencode.c \
	clock.c closedir.c crypt.c ctime.c ctype_.c difftime.c \
	disklabel.c errlst.c execvp.c fabs.s fakcu.c fstab.c \
	genbuildname.c getenv.c getgrent.c getlogin.c getmntinfo.c \
	getpass.c getpwent.c getttyent.c getusershell.c getwd.c \
	initgroups.c insque.s isatty.c isctype.c ldexp.s \
	malloc.c modf.s nargs.s ndbm.c nice.c nlist.c opendir.c pause.c \
	perror.c popen.c psignal.c qsort.c random.c readdir.c regerror.c \
	regex.c regexp.c remove.c remque.s scandir.c seekdir.c setegid.c \
	setenv.c seteuid.c setgid.c setjmp.s setjmperr.c setmode.c \
	setrgid.c setruid.c setuid.c siglist.c signal.c sleep.c syslog.c \
	system.c telldir.c termios.c time.c timezone.c ttyname.c \
	ttyslot.c ualarm.c udiv.s urem.s usleep.c valloc.c wait.c \
	wait3.c waitpid.c

.if (${MACHINE} == "tahoe")
SRCS+=	frexp.c
.elif (${MACHINE} == "vax")
SRCS+=	frexp.s
.endif

MAN3+=	abort.0 alarm.0 cencode.0 clock.0 crypt.0 ctime.0 ctype.0 \
	directory.0 execvp.0 frexp.0 genbuildname.0 getdiskbyname.0 \
	getenv.0 getfsent.0 getgrent.0 getlogin.0 getmntinfo.0 \
	getpass.0 getpwent.0 getttyent.0 getusershell.0 getwd.0 \
	initgroups.0 insque.0 malloc.0 ndbm.0 nice.0 nlist.0 \
	pause.0 perror.0 popen.0 psignal.0 qsort.0 random.0 regex.0 \
	regexp.0 scandir.0 setjmp.0 setmode.0 setuid.0 siginterrupt.0 \
	signal.0 sleep.0 syslog.0 system.0 time.0 ttyname.0 ualarm.0 \
	usleep.0 valloc.0

MLINKS=	crypt.3 encrypt.3 crypt.3 setkey.3
MLINKS+=ctime.3 asctime.3 ctime.3 difftime.3 ctime.3 gmtime.3 \
	ctime.3 localtime.3 ctime.3 timezone.3 ctime.3 tzset.3
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
MLINKS+=getenv.3 setenv.3 getenv.3 unsetenv.3 getenv.3 putenv.3
MLINKS+=getfsent.3 endfsent.3 getfsent.3 getfsfile.3 getfsent.3 getfsspec.3 \
	getfsent.3 getfstype.3 getfsent.3 setfsent.3
MLINKS+=getgrent.3 endgrent.3 getgrent.3 setgroupent.3 getgrent.3 getgrgid.3 \
	getgrent.3 getgrnam.3 getgrent.3 setgrent.3 getgrent.3 setgrfile.3
MLINKS+=getpwent.3 endpwent.3 getpwent.3 setpassent.3 getpwent.3 getpwnam.3 \
	getpwent.3 getpwuid.3 getpwent.3 setpwent.3 getpwent.3 setpwfile.3
MLINKS+=getttyent.3 endttyent.3 getttyent.3 getttynam.3 getttyent.3 setttyent.3
MLINKS+=getusershell.3 endusershell.3 getusershell.3 setusershell.3
MLINKS+=insque.3 remque.3
MLINKS+=malloc.3 alloca.3 malloc.3 calloc.3 malloc.3 free.3 malloc.3 realloc.3
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
MLINKS+=setjmp.3 _longjmp.3 setjmp.3 _setjmp.3 setjmp.3 longjmp.3
MLINKS+=setuid.3 setegid.3 setuid.3 seteuid.3 setuid.3 setgid.3 \
	setuid.3 setrgid.3 setuid.3 setruid.3
MLINKS+=syslog.3 closelog.3 syslog.3 openlog.3 syslog.3 setlogmask.3 \
	syslog.3 vsyslog.3
MLINKS+=ttyname.3 isatty.3 ttyname.3 ttyslot.3
MLINKS+=setmode.3 getmode.3

# net sources
SRCS+=	gethostnamadr.c getnetbyaddr.c getnetbyname.c getnetent.c \
	getproto.c getprotoent.c getprotoname.c getservbyname.c \
	getservbyport.c getservent.c herror.c htonl.s htons.s \
	inet_addr.c inet_lnaof.c inet_makeaddr.c inet_netof.c \
	inet_network.c inet_ntoa.c iso_addr.c ns_addr.c ns_ntoa.c \
	ntohl.s ntohs.s rcmd.c recv.c res_comp.c res_debug.c res_init.c \
	res_mkquery.c res_query.c res_send.c send.c sethostent.c

MAN3+=	byteorder.0 gethostbyname.0 getnetent.0 getprotoent.0 getservent.0 \
	inet.0 ns.0 rcmd.0 resolver.0 \

MLINKS+=byteorder.3 htonl.3 byteorder.3 htons.3 byteorder.3 ntohl.3 \
	byteorder.3 ntohs.3
MLINKS+=gethostbyname.3 endhostent.3 gethostbyname.3 gethostbyaddr.3 \
	gethostbyname.3 sethostent.3 gethostbyname.3 sethostfile.3 \
	gethostbyname.3 gethostent.3 gethostbyname.3 herror.3
MLINKS+=getnetent.3 endnetent.3 getnetent.3 getnetbyaddr.3 \
	getnetent.3 getnetbyname.3 getnetent.3 setnetent.3
MLINKS+=getprotoent.3 endprotoent.3 getprotoent.3 getprotobyname.3 \
	getprotoent.3 getprotobynumber.3 getprotoent.3 setprotoent.3
MLINKS+=getservent.3 endservent.3 getservent.3 getservbyname.3 \
	getservent.3 getservbyport.3 getservent.3 setservent.3
MLINKS+=inet.3 addr.3 inet.3 inet_addr.3 inet.3 inet_lnaof.3 \
	inet.3 inet_makeaddr.3 inet.3 inet_netof.3 inet.3 inet_network.3 \
	inet.3 inet_ntoa.3 inet.3 network.3 inet.3 ntoa.3
MLINKS+=ns.3 ns_addr.3 ns.3 ns_ntoa.3
MLINKS+=rcmd.3 rresvport.3 rcmd.3 ruserok.3

# stdio sources
SRCS+=	clrerr.c doprnt.c doscan.c exit.c fdopen.c fgetc.c filbuf.c \
	findiop.c flsbuf.c fopen.c fprintf.c fputc.c fread.c freopen.c \
	fseek.c ftell.c fwrite.c getchar.c gets.c getw.c printf.c \
	putchar.c putw.c rew.c scanf.c setbuf.c setbuffer.c sprintf.c \
	ungetc.c vfprintf.c vprintf.c vsprintf.c

.if (${MACHINE} == "vax")
SRCS+=	fgets.s fputs.s puts.s
.else
SRCS+=	fgets.c fputs.c puts.c
.endif

MAN3+=	exit.0 fclose.0 ferror.0 fgets.0 fopen.0 fread.0 fseek.0 getc.0 \
	gets.0 printf.0 putc.0 puts.0 scanf.0 setbuf.0 stdio.0 ungetc.0 \

MLINKS+=fclose.3 fflush.3
MLINKS+=ferror.3 clearerr.3 ferror.3 feof.3 ferror.3 fileno.3
MLINKS+=fopen.3 fdopen.3 fopen.3 freopen.3
MLINKS+=fread.3 fwrite.3
MLINKS+=fseek.3 ftell.3 fseek.3 rewind.3
MLINKS+=getc.3 fgetc.3 getc.3 getchar.3 getc.3 getw.3
MLINKS+=printf.3 fprintf.3 printf.3 sprintf.3 printf.3 vfprintf.3 \
	printf.3 vprintf.3 printf.3 vsprintf.3
MLINKS+=putc.3 fputc.3 putc.3 putchar.3 putc.3 putw.3
MLINKS+=puts.3 fputs.3
MLINKS+=scanf.3 fscanf.3 scanf.3 sscanf.3
MLINKS+=setbuf.3 setbuffer.3 setbuf.3 setlinebuf.3

# string sources
SRCS+=	atof.s atoi.c atol.c bcmp.s bcopy.s bzero.s ffs.s fnmatch.c \
	mktemp.c strcasecmp.c strcat.s strcmp.s strcpy.s strerror.c \
	strftime.c strlen.s strmode.c strncat.s strncmp.s strncpy.s swab.c

.if (${MACHINE} == "vax")
SRCS+=	index.s rindex.s
.else
SRCS+=	index.c rindex.c
.endif

MAN3+=	atof.0 bstring.0 fnmatch.0 glob.0 mktemp.0 strftime.0 string.0 \
	strmode.0 strtok.0 swab.0 \

MLINKS+=atof.3 atoi.3 atof.3 atol.3
MLINKS+=bstring.3 bcmp.3 bstring.3 bcopy.3 bstring.3 bzero.3 \
	bstring.3 ffs.3
MLINKS+=string.3 index.3 string.3 rindex.3 string.3 strcat.3 \
	string.3 strcmp.3 string.3 strcpy.3 string.3 strlen.3 \
	string.3 strncat.3 string.3 strncmp.3 string.3 strcasecmp.3 \
	string.3 strncasecmp.3 string.3 strncpy.3
MLINKS+=strtok.3 strsep.3
MLINKS+=glob.3 globfree.3
MLINKS+=mktemp.3 mkstemp.3

# sys sources
SRCS+=	Ovadvise.s Ovfork.s _exit.s _getlogin.s accept.s access.s \
	acct.s adjtime.s async_daemon.s bind.s brk.s cerror.s chdir.s \
	chflags.s chmod.s chown.s chroot.s close.s connect.s dup.s \
	dup2.s execl.s execle.s exect.s execv.s execve.s fchdir.s \
	fchflags.s fchmod.s fchown.s fcntl.s flock.s fork.s fstat.s \
	fstatfs.s fsync.s ftruncate.s getdirentries.s getdtablesize.s \
	getegid.s geteuid.s getfh.s getfsstat.s getgid.s getgroups.s \
	gethostid.s gethostname.s getitimer.s getkerninfo.s \
	getpagesize.s getpeername.s getpgrp.s getpid.s getppid.s \
	getpriority.s getrlimit.s getrusage.s getsockname.s getsockopt.s \
	gettimeofday.s getuid.s ioctl.s kill.s ktrace.s link.s listen.s \
	lseek.s lstat.s mkdir.s mkfifo.s mknod.s mmap.s mount.s nfssvc.s \
	open.s pipe.s profil.s ptrace.s quotactl.s read.s readlink.s \
	readv.s reboot.s recvfrom.s recvmsg.s rename.s rmdir.s sbrk.s \
	select.s sendmsg.s sendto.s setgroups.s sethostid.s \
	sethostname.s setitimer.s setlogin.s setpgrp.s setpriority.s \
	setregid.s setreuid.s setrlimit.s setsid.s \
	setsockopt.s settimeofday.s shutdown.s sigaction.s sigpending.s \
	sigprocmask.s sigreturn.s sigstack.s sigsuspend.s socket.s \
	socketpair.s stat.s statfs.s swapon.s symlink.s sync.s syscall.s \
	truncate.s umask.s unlink.s unmount.s utimes.s vhangup.s \
	wait4.s write.s writev.s

MAN2+=	accept.0 access.0 acct.0 adjtime.0 async_daemon.0 bind.0 brk.0 \
	chdir.0 chflags.0 chmod.0 chown.0 chroot.0 close.0 connect.0 \
	dup.0 execve.0 exit.0 fcntl.0 flock.0 fork.0 fsync.0 \
	getdirentries.0 getdtablesize.0 getfh.0 getfsstat.0 getgid.0 \
	getgroups.0 gethostid.0 gethostname.0 getitimer.0 getlogin.0 \
	getpagesize.0 getpeername.0 getpgrp.0 getpid.0 getpriority.0 \
	getrlimit.0 getrusage.0 getsockname.0 getsockopt.0 \
	gettimeofday.0 getuid.0 intro.0 ioctl.0 kill.0 link.0 listen.0 \
	lseek.0 mkdir.0 mkfifo.0 mknod.0 mount.0 nfssvc.0 open.0 pipe.0 \
	profil.0 ptrace.0 quotactl.0 read.0 readlink.0 reboot.0 recv.0 \
	rename.0 rmdir.0 select.0 send.0 setgroups.0 setpgrp.0 \
	setregid.0 setreuid.0 shutdown.0 sigreturn.0 \
	sigstack.0 socket.0 socketpair.0 stat.0 statfs.0 swapon.0 \
	symlink.0 sync.0 syscall.0 truncate.0 umask.0 unlink.0 utimes.0 \
	vfork.0 vhangup.0 wait.0 write.0

MLINKS+=brk.2 sbrk.2
MLINKS+=dup.2 dup2.2
MLINKS+=chdir.2 fchdir.2
MLINKS+=chflags.2 fchflags.2
MLINKS+=chmod.2 fchmod.2
MLINKS+=chown.2 fchown.2
MLINKS+=getgid.2 getegid.2
MLINKS+=gethostid.2 sethostid.2
MLINKS+=gethostname.2 sethostname.2
MLINKS+=getitimer.2 setitimer.2
MLINKS+=getlogin.2 setlogin.2
MLINKS+=getpid.2 getppid.2
MLINKS+=getpriority.2 setpriority.2
MLINKS+=getrlimit.2 setrlimit.2
MLINKS+=getsockopt.2 setsockopt.2
MLINKS+=gettimeofday.2 settimeofday.2
MLINKS+=getuid.2 geteuid.2
MLINKS+=intro.2 errno.2
MLINKS+=lseek.2 seek.2
MLINKS+=mount.2 unmount.2
MLINKS+=read.2 readv.2
MLINKS+=recv.2 recvfrom.2 recv.2 recvmsg.2
MLINKS+=send.2 sendmsg.2 send.2 sendto.2
MLINKS+=stat.2 fstat.2 stat.2 lstat.2
MLINKS+=statfs.2 fstatfs.2
MLINKS+=truncate.2 ftruncate.2
MLINKS+=wait.2 wait3.2 wait.2 wait4.2 wait.2 waitpid.2
MLINKS+=write.2 writev.2

# sys5 sources
SRCS+=	bsearch.c fts.c getopt.c memccpy.c memchr.c memcmp.c memcpy.c \
	memset.c putenv.c strcspn.c strdup.c strpbrk.c strsep.c strspn.c \
	strtok.c strtol.c strtoul.c tmpnam.c

.if (${MACHINE} == "vax")
SRCS+=	strchr.s strrchr.s
.else
SRCS+=	strchr.c strrchr.c
.endif

MAN3+=	bsearch.0 compat-sys5.0 fts.0 getopt.0

MLINKS+=compat-sys5.3 memccpy.3 compat-sys5.3 memchr.3 \
	compat-sys5.3 memcmp.3 compat-sys5.3 memcpy.3 \
	compat-sys5.3 memset.3 compat-sys5.3 strchr.3 \
	compat-sys5.3 strcspn.3 compat-sys5.3 strpbrk.3 \
	compat-sys5.3 strrchr.3 compat-sys5.3 strspn.3 \
	compat-sys5.3 strdup.3 compat-sys5.3 strtol.3 \
	compat-sys5.3 tmpnam.3 compat-sys5.3 tmpfile.3 \
	compat-sys5.3 tempnam.3

errlst.o:
	${CC} -S ${CFLAGS} ${.IMPSRC}
	ed - < ${.CURDIR}/gen/:errfix errlst.s
	${AS} -o errlst.o errlst.s
	cp errlst.o errlst.po
	rm -f errlst.s

beforeinstall:
	ar ma findiop.o libc.a exit.o
	ar ma exit.o libc.a fakcu.o
	ar ma findiop.po libc_p.a exit.po
	ar ma exit.po libc_p.a fakcu.po

.include <bsd.lib.mk>
