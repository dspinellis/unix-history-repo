/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* Contains the main loop initializations, and some system dependent
   type things, e.g. putting terminal in CBREAK mode, etc. */

#include "jove.h"
#include "io.h"
#include "termcap.h"

#ifdef MAC
#	include "mac.h"
#else
#	include <varargs.h>
#	include <sys/stat.h>
#endif

#include <signal.h>
#include <errno.h>

#ifdef UNIX
#	ifndef SYSV
#		include <sgtty.h>
#	else
#		include <termio.h>
#	endif /* SYSV */
#endif /* UNIX */

#ifdef MSDOS
#	include <process.h>
#endif /* MSDOS */

#ifndef MAC
#	include <fcntl.h>
#endif

#ifdef MAC
#	undef private
#	define private
#endif

#ifdef	LINT_ARGS
extern	long	time(long *);
private	void	break_off(void);
private	void	break_rst(void);
#else
extern	long	time();
private	void	break_off();
private	void	break_rst();
#endif

#ifdef MAC
#	undef private
#	define private static
#endif

#ifdef UNIX
#   ifdef TIOCSLTC
struct ltchars	ls1, ls2;
#   endif /* TIOCSLTC */

#   ifdef TIOCGETC
struct tchars	tc1, tc2;
#   endif

#   ifdef PASS8			/* use pass8 instead of raw for meta-key */
int	lmword1, lmword2;	/* local mode word */
#   endif

#   ifdef BRLUNIX
struct sg_brl	sg1, sg2;
#   else
#	ifdef SYSV
struct termio	sg1, sg2;
#	else /* SYSV */
struct sgttyb	sg1, sg2;
#	endif /* SYSV */
#   endif /* BRLUNIX */

#   ifdef BIFF
private struct stat	tt_stat;	/* for biff */
#	ifndef BSD4_2
private char	*tt_name = 0;		/* name of the control tty */
extern char	*ttyname();		/* for systems w/o fchmod ... */
#	endif
private int	dw_biff = NO;		/* whether or not to fotz at all */
#   endif /* BIFF */
#endif /* UNIX */

int	errormsg;
extern char	*tfname;
char	NullStr[] = "";

#ifdef MSDOS
#    define SIGHUP	99
#endif /* MSDOS */

void
finish(code)
{
	int	CoreDump = (code != 0 && code != SIGHUP),
		DelTmps = 1;		/* Usually we delete them. */

	if (code == SIGINT) {
		char	c;

#ifndef MENLO_JCL
		(void) signal(code, finish);
#endif
		f_mess("Abort (Type 'n' if you're not sure)? ");
#ifndef MSDOS
		(void) read(0, &c, 1);
#else /* MSDOS */
		c = getrawinchar();
#endif /* MSDOS */
		message(NullStr);
		if ((c & 0377) != 'y') {
			redisplay();
			return;
		}
	}
	ttyset(OFF);
	UnsetTerm(NullStr);
#ifndef MSDOS
	if (code != 0) {
		if (!Crashing) {
			Crashing = YES;
			lsave();
			SyncRec();
			printf("JOVE CRASH!! (code %d)\n", code);
			if (ModBufs(1)) {
				printf("Your buffers have been saved.\n");
				printf("Use \"jove -r\" to have a look at them.\n");
				DelTmps = 0;	/* Don't delete anymore. */
			} else
				printf("You didn't lose any work.\n");
		} else
			printf("\r\nYou may have lost your work!\n");
	}
#endif /* MSDOS */
	flusho();
	if (DelTmps) {
		tmpclose();
#ifndef MSDOS
		recclose();
#endif /* MSDOS */
	}
#ifdef UNIX
	if (CoreDump)
		abort();
#ifdef PROFILING
	exit(0);
#else
	_exit(0);
#endif
#else /* MSDOS or MAC*/
#ifdef MSDOS
	break_rst();	/* restore previous ctrl-c handling */
#endif
	exit(0);
#endif /* UNIX */
}

private char	smbuf[20],
		*bp = smbuf;
private int	nchars = 0;

private char	peekbuf[10],
		*peekp = peekbuf;

#if defined(SYSV) || defined(M_XENIX)
void
setblock(fd, on)	/* turn blocking on or off */
register int	fd, on;
{
    static int blockf, nonblockf;
    static int first = 1;
    int flags;

    if (first) {
	first = 0;
	if ((flags = fcntl(fd, F_GETFL, 0)) == -1)
	    finish(SIGHUP);
	blockf = flags & ~O_NDELAY;	/* make sure O_NDELAY is off */
	nonblockf = flags | O_NDELAY;	/* make sure O_NDELAY is on */
    }
    if (fcntl(fd, F_SETFL, on ? blockf : nonblockf) == -1)
	finish(SIGHUP);
}
#endif /* SYSV */

int
Peekc()
{
	int	c;

	if (peekp == peekbuf)
		c = -1;
	else
		c = *--peekp & 0377;
	return c;
}

void
Ungetc(c)
{
	if (peekp == &peekbuf[(sizeof peekbuf) - 1])
		return;		/* Sorry, can't oblige you ... */
	*peekp++ = c;
}

int	InputPending = 0;

char	*Inputp = 0;

#if (defined(IPROCS) && !defined(PIPEPROCS))	/* that is, if ptys */
getchar()
{
	extern long	global_fd;
	long		reads;
	extern int	NumProcs,
			errno;
	register int	tmp,
			nfds;
	int		c;

	if (nchars <= 0) {
		/* Get a character from the keyboard, first checking for
		   any input from a process.  Handle that first, and then
		   deal with the terminal input. */
		do {
			do {
				reads = global_fd;
				nfds = select(32, &reads, (long *) 0, (long *) 0, (struct timeval *) 0);
			} while (nfds < 0 && errno == EINTR);

			if (nfds == -1)
				complain("\rerror %d in select %ld", errno, global_fd);
			else {
				if (reads & 01) {
					nchars = read(0, smbuf, sizeof(smbuf));
					reads &= ~01;
					nfds -= 1;
				}
				while (nfds--) {
					tmp = ffs(reads) - 1;
					read_proc(tmp);
					reads &= ~(1L << tmp);
				}
			}
		} while (nchars <= 0);

		if (nchars <= 0)
			finish(SIGHUP);

		bp = smbuf;
		InputPending = (nchars > 1);
	}

	if (((c = *bp) & 0200) && MetaKey != 0) {
		*bp = (c & CHARMASK);
		return '\033';
	}
	nchars -= 1;
	return *bp++ & 0377;
}

#else

getchar()
{
	extern int	errno;
	register int	c;

	if (nchars <= 0) {
		bp = smbuf;
#ifdef MSDOS
		*bp = getrawinchar();
		nchars = 1;
#else
		do
			nchars = read(0, smbuf, sizeof smbuf);
#   ifdef SYSV
		while (nchars == 0 || (nchars < 0 && errno == EINTR));
		if (nchars < 0)
#   else
		while (nchars < 0 && errno == EINTR);
		if (nchars <= 0)
#   endif /* SYSV */
			finish(SIGHUP);
#endif MSDOS
		InputPending = nchars > 1;
	}
	if (((c = *bp) & 0200) && MetaKey != 0) {
		*bp = (c & CHARMASK);
		return '\033';
	}
	nchars -= 1;
	return (*bp++ & CHARMASK);
}

#endif /* IPROCS */

/* Returns non-zero if a character waiting */

int
charp()
{
#ifndef MSDOS
	int	some = 0;
#endif /* MSDOS */

	if (InJoverc != 0 || nchars > 0 || Inputp != 0)
		return 1;
#ifdef BRLUNIX
	{
		static struct sg_brl gttyBuf;

		gtty(0, (char *) &gttyBuf);
		if (gttyBuf.sg_xflags & INWAIT)
			some += 1;
	}
#endif
#ifdef FIONREAD
	{
		long c;

		if (ioctl(0, FIONREAD, (struct sgttyb *) &c) == -1)
			c = 0;
		some = (c > 0);
	}
#endif /* FIONREAD */
#if defined(SYSV) || defined(M_XENIX)
	setblock(0, 0);		/* turn blocking off */
	nchars = read(0, smbuf, sizeof smbuf);	/* Is anything there? */
	setblock(0, 1);		/* turn blocking on */
	if (nchars > 0)		/* something was there */
	    bp = smbuf;		/* make sure bp points to it */
	some = (nchars > 0);	/* just say we found something */
#endif /* SYSV */
#ifdef c70
	some = !empty(0);
#endif
#ifdef MSDOS
    return rawkey_ready();
#else
#ifdef MAC
	some = rawchkc();
#endif
	return some;
#endif /* MSDOS */
}

#ifdef TERMCAP
void
ResetTerm()
{
	putpad(TI, 1);
	putpad(VS, 1);
	putpad(KS, 1);
#ifndef MSDOS
	(void) chkmail(YES);	/* force it to check to we can be accurate */
#endif
#ifdef BIFF
	if (BiffChk != dw_biff)
		biff_init();
	/* just in case we changed our minds about whether to deal with
	   biff */
#endif
	do_sgtty();		/* this is so if you change baudrate or stuff
				   like that, JOVE will notice. */
	ttyset(ON);
}

void
UnsetTerm(mesg)
char	*mesg;
{
	ttyset(OFF);
	putpad(KE, 1);
	putpad(VE, 1);
	putpad(TE, 1);
#ifdef ID_CHAR
	INSmode(0);
#endif
	Placur(ILI, 0);
	printf("%s", mesg);
	putpad(CE, 1);
	flusho();
}
#endif /* TERMCAP */

#ifdef JOB_CONTROL
PauseJove()
{
	UnsetTerm(ModBufs(0) ? "[There are modified buffers]" : NullStr);
	(void) kill(0, SIGTSTP);
	ResetTerm();
	ClAndRedraw();
}
#endif


#ifndef MAC
void
Push()
{
	int
#ifndef MSDOS
		pid,
		(*old_quit)() = signal(SIGQUIT, SIG_IGN),
#endif /* MSDOS */
   		(*old_int)() = signal(SIGINT, SIG_IGN);

#ifndef MSDOS
#ifdef IPROCS
	sighold(SIGCHLD);
#endif
	switch (pid = fork()) {
	case -1:
		complain("[Fork failed]");

	case 0:
		UnsetTerm(NullStr);
#ifdef IPROCS
		sigrelse(SIGCHLD);
#endif
		(void) signal(SIGTERM, SIG_DFL);
#else /* MSDOS */
	UnsetTerm(NullStr);
#endif /* MSDOS */
		(void) signal(SIGINT, SIG_DFL);
#ifdef UNIX
		(void) signal(SIGQUIT, SIG_DFL);
		execl(Shell, basename(Shell), (char *)0);
		message("[Execl failed]");
		_exit(1);
	}
    	dowait(pid, (int *) 0);
#ifdef IPROCS
	sigrelse(SIGCHLD);
#endif
#endif /* UNIX */
#ifdef MSDOS
	break_rst();
	if (spawnl(0, Shell, basename(Shell), (char *)0) == -1)
		message("[Spawn failed]"); 
#endif /* MSDOS */
    	ResetTerm();
    	ClAndRedraw();
#ifndef MSDOS
	(void) signal(SIGQUIT, old_quit);
#else /* MSDOS */
	break_off();
#ifdef CHDIR
	getCWD();
#endif	
#endif /* MSDOS */
    	(void) signal(SIGINT, old_int);
}
#endif /* MAC */

int	OKXonXoff = 0,		/* ^S and ^Q initially DON'T work */
	IntChar = CTL(']');

void
ttsize()
{
#ifdef UNIX
#   ifdef TIOCGWINSZ
	struct winsize win;

	if (ioctl (0, TIOCGWINSZ, &win) == 0) {
		if (win.ws_col)
			CO = win.ws_col;
		if (win.ws_row)
			LI = win.ws_row;
	}
#   else /* TIOCGWINSZ */
#	ifdef BTL_BLIT
#include <sys/jioctl.h>
	struct jwinsize jwin;

	if (ioctl(0, JWINSIZE, &jwin) == 0) {
		if (jwin.bytesx)
			CO = jwin.bytesx;
		if (jwin.bytesy)
			LI = jwin.bytesy;
	}
#	endif /* BTL_BLIT */
#   endif /* TIOCGWINSZ */
#endif /* UNIX */
	ILI = LI - 1;
}

#ifdef BIFF
biff_init()
{
	dw_biff = ((BiffChk) &&
#   ifndef BSD4_2
		   ((tt_name != 0) || (tt_name = ttyname(0))) &&
		   (stat(tt_name, &tt_stat) != -1) &&
#   else
		   (fstat(0, &tt_stat) != -1) &&
#   endif
		   (tt_stat.st_mode & S_IEXEC));	/* he's using biff */

}

biff(on)
{
	if (dw_biff == NO)
		return;
#   ifndef BSD4_2
	(void) chmod(tt_name, on ? tt_stat.st_mode :
				   (tt_stat.st_mode & ~S_IEXEC));
#   else
	(void) fchmod(0, on ? tt_stat.st_mode :
			      (tt_stat.st_mode & ~S_IEXEC));
#   endif
}

#endif /* BIFF */

void
ttinit()
{
#ifdef BIFF
	biff_init();
#endif
#ifdef TIOCSLTC
	(void) ioctl(0, TIOCGLTC, (struct sgttyb *) &ls1);
	ls2 = ls1;
	ls2.t_suspc = (char) -1;
	ls2.t_dsuspc = (char) -1;
	ls2.t_flushc = (char) -1;
	ls2.t_lnextc = (char) -1;
#endif

#ifdef TIOCGETC
	/* Change interupt and quit. */
	(void) ioctl(0, TIOCGETC, (struct sgttyb *) &tc1);
	tc2 = tc1;
	tc2.t_intrc = IntChar;
	tc2.t_quitc = (char) -1;
	if (OKXonXoff) {
		tc2.t_stopc = (char) -1;
		tc2.t_startc = (char) -1;
	}
#endif /* TIOCGETC */
	do_sgtty();
}

private int	done_ttinit = 0;

void
do_sgtty()
{
#ifdef UNIX
# ifdef SYSV
	(void) ioctl(0, TCGETA, (char *) &sg1);
# else
	(void) gtty(0, &sg1);
# endif /* SYSV */
	sg2 = sg1;

# ifdef LPASS8
	(void) ioctl(0, TIOCLGET, &lmword1);
	lmword2 = lmword1;
	if (MetaKey == YES)
		lmword2 |= LPASS8;
# endif

# ifdef SYSV
	TABS = !((sg1.c_oflag & TAB3) == TAB3);
	ospeed = sg1.c_cflag & CBAUD;

	if (OKXonXoff)
		sg2.c_iflag &= ~(IXON | IXOFF);
	sg2.c_iflag &= ~(INLCR|ICRNL|IGNCR);
	sg2.c_lflag &= ~(ISIG|ICANON|ECHO);
	sg2.c_oflag &= ~(OCRNL|ONLCR);
	sg2.c_cc[VMIN] = sizeof smbuf;
	sg2.c_cc[VTIME] = 1;
# else
	TABS = !(sg1.sg_flags & XTABS);
	ospeed = sg1.sg_ospeed;
#  ifdef BRLUNIX
	sg2.sg_flags &= ~(ECHO | CRMOD);
	sg2.sg_flags |= CBREAK;

	/* VT100 Kludge: leave STALL on for flow control if DC3DC1 (Yuck.) */
	sg2.sg_xflags &= ~((sg2.sg_xflags&DC3DC1 ? 0 : STALL) | PAGE);
#  else
	sg2.sg_flags &= ~(ECHO | CRMOD);
#  endif /* BRLUNIX */

#  ifdef PURDUE_EE
#   ifdef pdp11
	sg2.sg_flags |= RAW;
#   else
	sg2.sg_flags |= (MetaKey ? RAW : CBREAK);
#   endif
#  else
#   ifdef LPASS8
	sg2.sg_flags |= CBREAK;
#   else
	sg2.sg_flags |= (MetaKey ? RAW : CBREAK);
#   endif
#  endif /* PURDUE_EE */
# endif /* SYSV */
#endif /* UNIX */

#ifdef MSDOS
# ifndef IBMPC
	setmode(1, 0x8000);
# endif /* IBMPC */
	TABS = 0;
#endif /* MSDOS */
}

void
tty_reset()
{
	if (!done_ttinit)
		return;
	ttyset(OFF);	/* go back to original modes */
	ttinit();
	ttyset(ON);
}

/* If n is OFF reset to original modes */

void
ttyset(n)
{
	if (!done_ttinit && n == 0)	/* Try to reset before we've set! */
		return;
#ifdef UNIX
# ifdef SYSV
	(void) ioctl(0, TCSETAW, n == 0 ? (struct sgttyb *) &sg1 : (struct sgttyb *) &sg2);
# else
#  ifdef BRLUNIX
	(void) stty(0, n == 0 ? (struct sgttyb *) &sg1 : (struct sgttyb *) &sg2);
#  else
	(void) ioctl(0, TIOCSETN, n == 0 ? (struct sgttyb *) &sg1 : (struct sgttyb *) &sg2);
#  endif /* BRLUNIX */
# endif /* SYSV */

# ifdef TIOCSETC
	(void) ioctl(0, TIOCSETC, n == 0 ? (struct sgttyb *) &tc1 : (struct sgttyb *) &tc2);
# endif /* TIOCSETC */
# ifdef TIOCSLTC
	(void) ioctl(0, TIOCSLTC, n == 0 ? (struct sgttyb *) &ls1 : (struct sgttyb *) &ls2);
# endif /* TIOCSLTC */
# ifdef LPASS8
	(void) ioctl(0, TIOCLSET, n == 0 ? &lmword1 : &lmword2);
# endif
#endif /* UNIX */

#ifdef MSDOS
# ifndef IBMPC
	setmode(1, n == 0 ? 0x4000 : 0x8000);
# endif
#endif /* MSDOS */
	done_ttinit = 1;
#ifdef BIFF
	biff(!n);
#endif
}

int	this_cmd,
	last_cmd;

void
dispatch(c)
register int	c;
{
	data_obj	*cp;

	this_cmd = 0;
	cp = mainmap[c & CHARMASK];

	if (cp == 0) {
		rbell();
		clr_arg_value();
		errormsg = NO;
		message(NullStr);
	} else
		ExecCmd(cp);
}

int	LastKeyStruck,
	MetaKey = 0;

int
getch()
{
	register int	c,
			peekc;
#ifdef IPROCS
	extern int	NumProcs;
#endif
	extern int	ModCount,
			Interactive;

	if (Inputp) {
		if ((c = *Inputp++) != 0)
			return LastKeyStruck = c;
		Inputp = 0;
	}

	if (InJoverc)
		return EOF;	/* somethings wrong if Inputp runs out while
				   we're reading a .joverc file. */

#ifndef MSDOS
	if (ModCount >= SyncFreq) {
		ModCount = 0;
		SyncRec();
	}
#endif /* MSDOS */

	/* If we're not interactive and we're not executing a macro,
	   AND there are no ungetc'd characters, we read from the
	   terminal (i.e., getch()).  And characters only get put
	   in macros from inside this if. */
	if (((peekc = c = Peekc()) == -1) && (Interactive || ((c = mac_getc()) == -1))) {
		/* So messages that aren't error messages don't
		   hang around forever. */
		if (!UpdMesg && !Asking) {	/* Don't erase if we are asking */
			if (mesgbuf[0] && !errormsg)
				message(NullStr);
		}
		redisplay();
#if defined(IPROCS) && defined(PIPEPROCS)
		if (NumProcs > 0) {
			sigrelse(INPUT_SIG);
			sigrelse(SIGCHLD);
		}
#endif
#ifdef UNIX
		inIOread = 1;
#endif
		if ((c = getchar()) == EOF)
			finish(SIGHUP);
#ifdef UNIX
		inIOread = 0;
#endif

#if defined(IPROCS) && defined(PIPEPROCS)
		if (NumProcs > 0) {
			sighold(INPUT_SIG);
			sighold(SIGCHLD);
		}
#endif
		if (!Interactive && InMacDefine)
			mac_putc(c);
	}
	if (peekc == -1)	/* don't add_stroke peekc's */
		add_stroke(c);
	return LastKeyStruck = c;
}

#ifdef UNIX
dorecover()
{
	execl(Recover, "recover", "-d", TmpFilePath, (char *) 0);
	printf("%s: execl failed!\n", Recover);
	flusho();
	_exit(-1);
}
#endif /* UNIX */
		
void
ShowVersion()
{
	extern char	*version;

	s_mess("Jonathan's Own Version of Emacs (%s)", version);
}

void
UNIX_cmdline(argc, argv)
char	*argv[];
{
	int	lineno = 0,
		nwinds = 1;
	Buffer	*b;

	ShowVersion();
	while (argc > 1) {
		if (argv[1][0] != '-' && argv[1][0] != '+') {
			int	force = (nwinds > 0 || lineno != 0);

#ifdef MSDOS
			strlwr(argv[1]);
#endif
			minib_add(argv[1], force ? YES : NO);
			b = do_find(nwinds > 0 ? curwind : (Window *) 0,
				    argv[1], force);
			if (force) {
				SetABuf(curbuf);
				SetBuf(b);
				if (lineno >= 0)
					SetLine(next_line(curbuf->b_first, lineno));
				else
					SetLine(curbuf->b_last);
				if (nwinds > 1)
					NextWindow();
				if (nwinds)
					nwinds -= 1;
			}
			lineno = 0;
		} else	switch (argv[1][1]) {
			case 'd':
				argv += 1;
				argc -= 1;
				break;

			case 'j':	/* Ignore .joverc in HOME */
				break;
#ifndef MAC
			case 'p':
				argv += 1;
				argc -= 1;
				if (argv[1] != 0) {
					SetBuf(do_find(curwind, argv[1], 0));
					ErrParse();
					nwinds = 0;
				}
				break;
#endif
			case 't':
 				/* check if syntax is -tTag or -t Tag */
 				if (argv[1][2] != 0) {
 					find_tag(&(argv[1][2]), YES);
 				} else {
					argv += 1;
					argc -= 1;
					if (argv[1] != 0)
						find_tag(argv[1], YES);
 				}
  				break;
  
			case 'w':
				if (argv[1][2] == '\0')
					nwinds += 1;
				else {
					int	n;

					(void) chr_to_int(&argv[1][2], 10, NO, &n);
					nwinds += -1 + n;
				}
				(void) div_wind(curwind, nwinds - 1);
				break;

			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				(void) chr_to_int(&argv[1][1], 10, NO, &lineno);
				lineno -= 1;
				break;
			case  0: 
				lineno = -1;	/* goto end of file ... */
				break;		/* just like some people's */
		}				/* favourite editor */
		argv += 1;
		argc -= 1;
	}
}

/* VARARGS1 */

void
error(fmt, va_alist)
char	*fmt;
va_dcl
{
	va_list	ap;

	if (fmt) {
		va_start(ap);
		format(mesgbuf, sizeof mesgbuf, fmt, ap);
		va_end(ap);
		UpdMesg = YES;
	}
	rbell();
	longjmp(mainjmp, ERROR);
}

/* VARARGS1 */

void
complain(fmt, va_alist)
char	*fmt;
va_dcl
{
	va_list	ap;

	if (fmt) {
		va_start(ap);
		format(mesgbuf, sizeof mesgbuf, fmt, ap);
		va_end(ap);
		UpdMesg = YES;
	}
	rbell();
	longjmp(mainjmp, COMPLAIN);
}

/* VARARGS1 */

void
confirm(fmt, va_alist)
char	*fmt;
va_dcl
{
	char	*yorn;
	va_list	ap;

	va_start(ap);
	format(mesgbuf, sizeof mesgbuf, fmt, ap);
	va_end(ap);
	yorn = ask((char *) 0, mesgbuf);
	if (*yorn != 'Y' && *yorn != 'y')
		longjmp(mainjmp, COMPLAIN);
}

int	RecDepth = 0;

void
Recur()
{
	char	bname[128];
	Mark	*m;

	sprintf(bname, "%s", curbuf->b_name);
	m = MakeMark(curline, curchar, M_FLOATER);

	RecDepth += 1;
	UpdModLine = YES;
	DoKeys(1);	/* 1 means not first time */
	UpdModLine = YES;
	RecDepth -= 1;
	SetBuf(do_select(curwind, bname));
	if (!is_an_arg())
		ToMark(m);
	DelMark(m);
}

jmp_buf	mainjmp;

#ifdef MAC
jmp_buf auxjmp;
#endif

int	iniargc;	/* main sets these for DoKeys() */
char	**iniargv;

void
DoKeys(nocmdline)
{
	int	c;
	jmp_buf	savejmp;

	push_env(savejmp);

	switch (setjmp(mainjmp)) {
	case 0:
		if (!nocmdline)
			UNIX_cmdline(iniargc, iniargv);
		break;

	case QUIT:
		if (RecDepth == 0) {
			if (ModMacs()) {
				rbell();
				if (CharUpcase(*ask("No",
"Some MACROS haven't been saved; leave anyway? ")) != 'Y')
					break;
			}
			if (ModBufs(0)) {
				rbell();
				if (CharUpcase(*ask("No",
"Some buffers haven't been saved; leave anyway? ")) != 'Y')
					break;
			}
#ifdef IPROCS
			KillProcs();
#endif
		}
		pop_env(savejmp);
		return;

	case ERROR:
		getDOT();	/* God knows what state linebuf was in */

	case COMPLAIN:
	    {
		extern int	DisabledRedisplay;

		gc_openfiles();	/* close any files we left open */
		errormsg = YES;
		unwind_macro_stack();
		Asking = 0;
		curwind->w_bufp = curbuf;
		DisabledRedisplay = NO;
		redisplay();
		break;
	    }
	}

	this_cmd = last_cmd = 0;

	for (;;) {
#ifdef MAC
		setjmp(auxjmp);
#endif
		if (this_cmd != ARG_CMD) {
			clr_arg_value();
			last_cmd = this_cmd;
			init_strokes();
		}
#ifdef MAC
		HiliteMenu(0);
		EventCmd = 0;
		menus_on();
#endif
		c = getch();
		if (c == -1)
			continue;
	 	dispatch(c);
	}
}

int	Crashing = 0;

char **
scanvec(args, str)
register char	**args,
		*str;
{
	while (*args) {
		if (strcmp(*args, str) == 0)
			return args;
		args += 1;
	}
	return 0;
}

#ifdef UNIX
int	UpdFreq = 30,
	inIOread = 0;

updmode()
{
	UpdModLine = YES;
	if (inIOread)
		redisplay();
#ifndef JOB_CONTROL
	(void) signal(SIGALRM, updmode);
#endif
	(void) alarm((unsigned) UpdFreq);
}
#endif /* UNIX */

#ifndef MSDOS
# if defined(TIOCGWINSZ) && defined(SIGWINCH)
extern win_reshape();
# endif
#else /* MSDOS */
# ifndef IBMPC
char	ttbuf[BUFSIZ];
# endif	/* IBMPC */
#endif /* MSDOS */

#if defined(TIOCGWINSZ) && defined(SIGWINCH)
win_reshape()
{
	register int oldsize;
	register int newsize, total;
	register Window *wp;

	(void) signal(SIGWINCH, SIG_IGN);

	/*
	 * Save old number of lines.
	 */
	oldsize = LI;

	/*
	 * Get new line/col info.
	 */
	ttsize();

	/*
	 * LI has changed, and now holds the
	 * new value.
	 */
	/* 
	 *  Go through the window list, changing each window size in
	 *  proportion to the resize. If a window becomes too small,
	 *  delete it. We keep track of all the excess lines (caused by
	 *  roundoff!), and give them to the current window, as a sop -
	 *  can't be more than one or two lines anyway. This seems fairer
	 *  than just resizing the current window.
	 */
	wp = fwind;
	total = 0;
	do {
		newsize = LI * wp->w_height / oldsize;

		if (newsize < 2) {
			total += wp->w_height;
			wp = wp->w_next;
			del_wind(wp->w_prev);
		} else {
			wp->w_height = newsize;
			total += newsize;
			wp = wp->w_next;
		}
	} while (wp != fwind);

	curwind->w_height += LI - total - 1;

	/* Make a new screen structure */
	make_scr();
	/* Do a 'hard' update on the screen - clear and redraw */
	cl_scr(1);
	flusho();
	redisplay();

	(void) signal(SIGWINCH, win_reshape);
}
#endif

void

#ifdef MAC	/* will get args from user, if option key held during launch */
main()
{
	int argc;
	char **argv;
#else
main(argc, argv)
char	*argv[];
{
#endif /* MAC */
	char	*cp;
#ifndef MSDOS
	char	ttbuf[MAXTTYBUF],
# ifndef VMUNIX
		s_iobuff[LBSIZE],
		s_genbuf[LBSIZE],
		s_linebuf[LBSIZE];
	/* The way I look at it, there ain't no way I is gonna run
	   out of stack space UNLESS I have some kind of infinite
	   recursive bug.  So why use up some valuable memory, when
	   there is plenty of space on the stack?  (This only matters
	   on wimpy pdp11's, of course.) */

	iobuff = s_iobuff;
	genbuf = s_genbuf;
	linebuf = s_linebuf;
# endif

#else /* MSDOS */
	char	*getenv();
#endif /* MSDOS */

	errormsg = 0;

#ifdef MAC
	MacInit();		/* initializes all */
	if(make_cache() == 0) exit(-1);
	argc = getArgs(&argv);
#endif /* MAC */

	iniargc = argc;
	iniargv = argv;

	if (setjmp(mainjmp)) {
		printf("\rAck! I can't deal with error \"%s\" now.\n\r", mesgbuf);
		finish(0);
	}

#ifdef MSDOS
	/* import the temporary file path from the environment and
	   fix the string, so that we can append a slash safely	*/

	if (((cp = getenv("TMP")) || (cp = getenv("TMPDIR"))) &&
	    (*cp != '\0')) {
		strcpy(TmpFilePath, cp);
		cp = &TmpFilePath[strlen(TmpFilePath)-1];
		if ((*cp == '/') || (*cp == '\\'))
			*cp = 0;
	}
	ShFlags[0] = switchar();
#endif /* MSDOS */

	getTERM();	/* Get terminal. */
	if (getenv("METAKEY"))
		MetaKey = 1;
	ttsize();
	InitCM();
#ifdef MAC
	InitEvents();
#endif

	d_cache_init();		/* initialize the disk buffer cache */
#ifndef MAC
	if ((cp = getenv("COMSPEC")) && (*cp != '\0')) {
		strcpy(Shell, cp);
	}
#endif
#ifdef MSDOS
	if ((cp = getenv("DESCRIBE")) && (*cp != '\0'))
	   strcpy(CmdDb, cp);
#endif /* MSDOS */

	make_scr();
	mac_init();	/* Initialize Macros */
	winit();	/* Initialize Window */
#ifdef IPROCS
	pinit();	/* Pipes/process initialization */
#endif
	SetBuf(do_select(curwind, Mainbuf));

#ifdef CHDIR
	{
		char	**argp;

		if ((argp = scanvec(argv, "-d")) && (argp[1][0] == '/'))
			setCWD(argp[1]);
		else
			getCWD();	/* After we setup curbuf in case we have to getwd() */
	}
#endif
	HomeDir = getenv("HOME");
	if (HomeDir == 0)
		HomeDir = "/";
	HomeLen = strlen(HomeDir);

#ifdef UNIX
# ifdef SYSV
	sprintf(Mailbox, "/usr/mail/%s", getenv("LOGNAME"));
# else
	sprintf(Mailbox, "/usr/spool/mail/%s", getenv("USER"));
# endif
#endif

#ifdef MSDOS
	if ((cp = getenv("JOVERC")) && (*cp != '\0'))
	   (void) joverc(cp);
#endif /* MSDOS */
	(void) joverc(Joverc);
	if (!scanvec(argv, "-j")) {
		char	tmpbuf[100];
		sprintf(tmpbuf, "%s/.joverc", HomeDir);
		(void) joverc(tmpbuf);
	}
#ifndef MSDOS
	if (scanvec(argv, "-r"))
		dorecover();
	if (scanvec(argv, "-rc"))
		FullRecover();
#endif 	/* MSDOS */
	ttinit();	/* initialize terminal (after ~/.joverc) */
#ifndef IBMPC
	settout(ttbuf);	/* not until we know baudrate */
#endif

#ifdef MSDOS
	(void) signal(SIGINT, SIG_IGN);
	break_off();	/* disable ctrl-c checking */
#endif /* MSDOS */
#ifdef UNIX
	(void) signal(SIGHUP, finish);
	(void) signal(SIGINT, finish);
	(void) signal(SIGBUS, finish);
	(void) signal(SIGSEGV, finish);
	(void) signal(SIGPIPE, finish);
	(void) signal(SIGTERM, SIG_IGN);
# if defined(TIOCGWINSZ) && defined(SIGWINCH)
	(void) signal(SIGWINCH, win_reshape);
# endif 
	/* set things up to update the modeline every UpdFreq seconds */
	(void) signal(SIGALRM, updmode);
	(void) alarm((unsigned) (60 - (time((time_t *) 0) % 60)));
#endif /* UNIX */

	ResetTerm();
	cl_scr(1);
	flusho();
	RedrawDisplay();	/* start the redisplay process. */
	DoKeys(0);
	finish(0);
}

#ifdef MSDOS

#include <dos.h>

static	char break_state;

/* set the break state to off */
private void
break_off()
{
	union REGS regs;

	regs.h.ah = 0x33;		/* break status */
	regs.h.al = 0x00;		/* request current state */
	intdos(&regs, &regs);
	break_state = regs.h.dl;
	bdos(0x33, 0, 1);	/* turn off break */
}

/* reset the break state */
private void
break_rst()
{
	bdos(0x33, break_state, 1);
}
#endif
