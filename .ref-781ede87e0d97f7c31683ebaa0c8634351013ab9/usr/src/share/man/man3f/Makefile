#	@(#)Makefile	5.5 (Berkeley) %G%

MAN3F=	abort.0 access.0 alarm.0 bessel.0 bit.0 chdir.0 chmod.0 etime.0 \
	exit.0 fdate.0 flmin.0 flush.0 fork.0 fseek.0 getarg.0 getc.0 \
	getcwd.0 getenv.0 getlog.0 getpid.0 getuid.0 getuid.0 hostnm.0 \
	idate.0 index.0 intro.0 ioinit.0 kill.0 link.0 loc.0 long.0 \
	malloc.0 perror.0 plot.0 putc.0 qsort.0 rand.0 random.0 rename.0 \
	signal.0 sleep.0 stat.0 system.0 time.0 topen.0 traper.0 trapov.0 \
	trpfpe.0 ttynam.0 unlink.0 wait.0
MLINKS=	bessel.3f besj0.3f bessel.3f besj1.3f bessel.3f besjn.3f bessel.3f \
	besy0.3f bessel.3f besy1.3f bessel.3f besyn.3f bessel.3f dbesj0.3f \
	bessel.3f dbesj1.3f bessel.3f dbesjn.3f bessel.3f dbesy0.3f bessel.3f \
	dbesy1.3f bessel.3f dbesyn.3f
MLINKS+=etime.3f dtime.3f
MLINKS+=flmin.3f dffrac.3f flmin.3f dflmax.3f flmin.3f dflmin.3f flmin.3f \
	ffrac.3f flmin.3f flmax.3f flmin.3f inmax.3f
MLINKS+=fseek.3f ftell.3f
MLINKS+=getarg.3f iargc.3f
MLINKS+=getc.3f fgetc.3f
MLINKS+=getuid.3f getgid.3f
MLINKS+=idate.3f itime.3f
MLINKS+=index.3f len.3f index.3f lnblnk.3f index.3f rindex.3f
MLINKS+=intro.3f fortran.3f
MLINKS+=link.3f symlnk.3f
MLINKS+=long.3f short.3f
MLINKS+=malloc.3f falloc.3f
MLINKS+=perror.3f gerror.3f perror.3f ierrno.3f
MLINKS+=plot.3f box.3f
MLINKS+=putc.3f fputc.3f
MLINKS+=rand.3f drand.3f rand.3f irand.3f
MLINKS+=random.3f drandm.3f random.3f irandm.3f
MLINKS+=stat.3f fstat.3f stat.3f lstat.3f
MLINKS+=time.3f ctime.3f time.3f gmtime.3f time.3f ltime.3f
MLINKS+=topen.3f tclose.3f topen.3f tread.3f topen.3f trewin.3f topen.3f \
	tskipf.3f topen.3f tstate.3f topen.3f twrite.3f
MLINKS+=trpfpe.3f fpecnt.3f
MLINKS+=ttynam.3f isatty.3f
MLINKS+=bit.3f and.3f bit.3f lshift.3f bit.3f not.3f bit.3f or.3f bit.3f \
	rshift.3f bit.3f xor.3f

all: ${MAN3F}

clean depend lint tags:

cleandir:
	rm -f ${MAN3F}

install: maninstall

.include <bsd.prog.mk>
