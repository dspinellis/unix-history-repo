/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* Contains the main loop initializations, and some system dependent
   type things, e.g. putting terminal in CBREAK mode, etc. */

#include "jove.h"
#include "fp.h"
#include "termcap.h"
#include "ctype.h"
#include "chars.h"
#include "disp.h"
#include "re.h"	/* for find_tag() */
#include "rec.h"
#ifdef	IPROCS
# include "iproc.h"
#endif

#ifdef UNIX
#include "ttystate.h"
#endif

#ifdef SCO
#undef TIOCGWINSZ

#include <sys/stream.h>
#include <sys/ptem.h>
#endif

#ifdef	MAC
# include "mac.h"
#else
# ifdef	STDARGS
#  include <stdarg.h>
# else
#  include <varargs.h>
# endif
# include <sys/stat.h>
#endif

#include <signal.h>
#include <errno.h>

#ifdef	MSDOS
# include <process.h>
#endif	/* MSDOS */

#ifndef	MAC
# include <fcntl.h>
#endif

#ifdef	MSDOS
private	void	break_off proto((void)),
		break_rst proto((void));
#endif

#ifdef	MAC
# define	WINRESIZE	1
#else
# ifdef	TIOCGWINSZ
#   ifdef	SIGWINCH
#     define	WINRESIZE	1
#   endif
# endif
#endif

private void
	DoKeys proto((bool firsttime));

#ifdef	MSDOS
extern
#else
private
#endif
    void
	UnsetTerm proto((char *)),
	do_sgtty proto((void));

/* Various tty state structures.
 * Each is an array, subscripted by one of "OFF" or "ON".
 */

#ifdef	UNIX

# ifdef	TIOCSLTC
struct ltchars	ls[2];
# endif	/* TIOCSLTC */

# ifdef	TIOCGETC
struct tchars	tc[2];
# endif

# ifdef	PASS8			/* use pass8 instead of raw for meta-key */
private int	lmword[2];		/* local mode word */
# endif

# ifdef	BRLUNIX
struct sg_brl	sg[2];
#endif
#ifdef TERMIO
struct termio	sg[2];
#endif
#ifdef TERMIOS
struct termios	sg[2];
#endif
#ifdef SGTTY
struct sgttyb	sg[2];
#endif

# ifdef	BIFF
private struct stat	tt_stat;	/* for biff */
#  ifndef	BSD4_2
private char	*tt_name = NULL;		/* name of the control tty */
extern char	*ttyname();		/* for systems w/o fchmod ... */
#  endif
private bool	dw_biff = NO;		/* whether or not to fotz at all */
# endif	/* BIFF */
#endif	/* UNIX */

bool	errormsg;
char	NullStr[] = "";
jmp_buf	mainjmp;


#ifdef	MSDOS
# define SIGHUP	99
# define SIGIOT 99
#endif	/* MSDOS */

/* finish() does not return, so it is funny that it returns a non-void
 * result.  This is because most systems claim that signal(2) deals
 * with functions of type int ().  ANSI changes this: the function
 * type must be void (int).  This bridge must soon be crossed.
 */
SIGRESULT
finish(code)
int	code;
{
	int save_errno = errno;	/* Subtle, but necessary! */
	static int	Crashing = 0;	/* we are in the middle of crashing */
	bool	CoreDump = (code != 0 && code != SIGHUP),
		DelTmps = YES;		/* Usually we delete them. */

	if (code == SIGINT) {
		char	c;
#ifdef	PIPEPROCS
		int	started;
#endif
#ifndef	MENLO_JCL
		(void) signal(code, finish);
#endif
		f_mess("Abort (Type 'n' if you're not sure)? ");
#ifndef	MSDOS
# ifdef	PIPEPROCS
		started = kbd_stop();
# endif
#ifdef	SYSV
		if (read(0, (UnivPtr) &c, (size_t) 1) != 1)
#endif
			(void) read(0, (UnivPtr) &c, (size_t) 1);
# ifdef	PIPEPROCS
		if (started)
			(void) kbd_strt();
# endif
#else	/* MSDOS */
		c = getrawinchar();
#endif	/* MSDOS */
		message(NullStr);
		if ((c & 0377) != 'y') {
			redisplay();
			errno = save_errno;
			SIGRETURN;
		}
	}
	DisabledRedisplay = YES;
#ifndef	MAC
	UnsetTerm(NullStr);
#endif
#ifdef	PIPEPROCS
	kbd_kill();		/* kill the keyboard process */
#endif
#ifndef	MSDOS
	if (code != 0) {
		if (!Crashing) {
			Crashing = YES;
			lsave();
			SyncRec();
			writef("JOVE CRASH!! (code %d): %s\n", code,
			       strerror(errno));
			if (ModBufs(YES)) {
				writef("Your buffers have been saved.\n");
				writef("Use \"jove -r\" to have a look at them.\n");
				DelTmps = NO;	/* Don't delete anymore. */
			} else
				writef("You didn't lose any work.\n");
		} else
			writef("\r\nYou may have lost your work!\n");
	}
#endif	/* MSDOS */
	flushscreen();
	if (DelTmps) {
#ifdef	PTYPROCS
		(void) signal(SIGCHLD, SIG_IGN);
#endif
		tmpremove();
#ifndef	MSDOS
		recremove();
#endif	/* MSDOS */
	}
#ifdef	UNIX
	if (CoreDump)
		abort();
#ifdef	PROFILING
	exit(0);
#else
	_exit(0);
#endif
#else	/* !UNIX */
#ifdef	MSDOS
	break_rst();	/* restore previous ctrl-c handling */
#endif
	exit(0);
#endif	/* !UNIX */
	/*NOTREACHED*/
}

private char	smbuf[20],
		*bp = smbuf;
private int	nchars = 0;

private char	peekbuf[10],
		*peekp = peekbuf;

#if	defined(SYSV) || defined(M_XENIX)

#define	NONBLOCKINGREAD	1

private void
setblock(fd, on)	/* turn blocking on or off */
register int	fd;
bool	on;
{
    static int blockf, nonblockf;
    static bool	first = TRUE;

    if (first) {
	int flags;

	first = FALSE;
	if ((flags = fcntl(fd, F_GETFL, 0)) == -1)
	    finish(SIGHUP);
	blockf = flags & ~O_NDELAY;	/* make sure O_NDELAY is off */
	nonblockf = flags | O_NDELAY;	/* make sure O_NDELAY is on */
    }
    if (fcntl(fd, F_SETFL, on ? blockf : nonblockf) == -1)
	finish(SIGHUP);
}

#endif	/* defined(SYSV) || defined(M_XENIX) */

private int
Peekc()
{
	return peekp == peekbuf? EOF : *--peekp & 0377;
}

void
Ungetc(c)
int	c;
{
	if (peekp != &peekbuf[(sizeof peekbuf) - 1])
		*peekp++ = c;
}

bool	InputPending = NO;

char	*Inputp = NULL;

#ifdef	PTYPROCS
int
jgetchar()
{

	fd_set		reads;
	register int	max = getdtablesize();
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
				nfds = select(max, &reads, (fd_set *)0, (fd_set *)0, (struct timeval *)NULL);
			} while (nfds < 0 && errno == EINTR);

			if (nfds == -1)
				complain("\rerror in select %ld: %s", global_fd, strerror(errno));
			else {
				if (FD_ISSET(0, &reads)) {
					nchars = read(0, (UnivPtr) smbuf, sizeof(smbuf));
					FD_CLR(0, &reads);
					nfds--;
				}
				for (tmp = 1; tmp < max; tmp++) {
					if (FD_ISSET(tmp, &reads)) {
						read_proc(tmp);
						FD_CLR(tmp, &reads);
						if (--nfds == 0)
							break;
					}
				}
			}
		} while (nchars <= 0);

		if (nchars <= 0)
			finish(SIGHUP);

		bp = smbuf;
		InputPending = (nchars > 1);
	}

	if (((c = *bp) & 0200) && MetaKey) {
		*bp = (c & CHARMASK);
		return '\033';
	}
	nchars -= 1;
	return *bp++ & 0377;
}

#else	/* !PTYPROCS */

int
jgetchar()
{
	register int	c;
	struct header {
		int	pid;
		int	nbytes;
	} header;

normal:
	if (nchars <= 0) {
		bp = smbuf;
#ifdef	MSDOS
		*bp = getrawinchar();
		nchars = 1;
#else	/* !MSDOS */
# ifdef	IPROCS
		if (NumProcs > 0) {
			for (;;) {
				size_t	n = f_readn(ProcInput, (char *) &header,
					    sizeof(header));

				if (n != sizeof(header)) {
					raw_complain("\r\nError reading kbd process, expected %d, got %d bytes\r\n", sizeof header, n);
					finish(SIGHUP);
				}
				/* data is from the keyboard process */
				if (header.pid == kbd_pid) {
					nchars = f_readn(ProcInput, smbuf, header.nbytes);
					if (nchars != header.nbytes) {
						raw_complain("\r\nError reading kbd process, expected %d, got %d bytes.\r\n", header.nbytes, nchars);
						finish(SIGHUP);
					}
					break;
				}
				read_proc(header.pid, header.nbytes);
				if (NumProcs == 0) {
					(void) kbd_stop();
					goto normal;
				}
			}
		} else /*...*/
# endif
		/*...*/ {
			for (;;) {
				nchars = read(0, (UnivPtr) smbuf, sizeof smbuf);
				if (nchars > 0)
					break;
# ifdef	SYSV
				/* System V seems to allow zero-length results */
				if (nchars == 0)
					continue;
# endif	/* SYSV */
				/* retry on interrupt */
				if (!(nchars < 0 && errno == EINTR))
					finish(SIGHUP);
			}
		}
#endif	/* !MSDOS */
		InputPending = nchars > 0;
	}
	if (((c = *bp) & 0200) && MetaKey) {
		*bp = (c & CHARMASK);
		return '\033';
	}
	nchars -= 1;
	return (*bp++ & CHARMASK);
}

#endif	/* !PTYPROCS */

/* Returns non-zero if a character waiting */

bool
charp()
{
	bool	some = NO;

	if (InJoverc != 0 || nchars > 0 || Inputp != NULL)
		return YES;
#ifdef	BRLUNIX
	{
		static struct sg_brl gttyBuf;

		gtty(0, (char *) &gttyBuf);
		if (gttyBuf.sg_xflags & INWAIT)
			some = YES;
	}
#else
#ifdef	FIONREAD
	{
		long c;

		if (ioctl(0, FIONREAD, (UnivPtr) &c) == -1)
			c = 0;
		some = (c > 0);
	}
#else
#ifdef	NONBLOCKINGREAD
	setblock(0, OFF);		/* turn blocking off */
	nchars = read(0, (UnivPtr) smbuf, sizeof smbuf);	/* Is anything there? */
	setblock(0, ON);		/* turn blocking on */
	if (nchars > 0)		/* something was there */
	    bp = smbuf;		/* make sure bp points to it */
	some = (nchars > 0);	/* just say we found something */
#else
#ifdef	c70
	some = !empty(0);
#else
#ifdef	MSDOS
	some = rawkey_ready();
#else
#ifdef	MAC
	some = rawchkc();
#endif
#endif
#endif
#endif
#endif
#endif
	return some;
}

#ifdef	BIFF
private void	biff_init proto((void));
#endif

#ifdef	TERMCAP

private void
ResetTerm()
{
	do_sgtty();		/* this is so if you change baudrate or stuff
				   like that, JOVE will notice. */
	ttyset(ON);
	putpad(TI, 1);
	putpad(VS, 1);
	putpad(KS, 1);
#ifdef	UNIX
	(void) chkmail(YES);	/* force it to check to we can be accurate */
#endif
#ifdef	BIFF
	if (BiffChk != dw_biff)
		biff_init();
	/* just in case we changed our minds about whether to deal with
	   biff */
#endif
}

private void
UnsetTerm(mesg)
char	*mesg;
{
	ttyset(OFF);
#ifdef	ID_CHAR
	INSmode(NO);
#endif
	putpad(KE, 1);
	putpad(VE, 1);
	Placur(ILI, 0);
	putpad(CE, 1);
	if (TE)
		putpad(TE, 1);
	if (mesg[0] != '\0')
		writef("%s\n", mesg);
	flushscreen();
}
#endif	/* TERMCAP */

#ifdef	JOB_CONTROL
void
PauseJove()
{
	UnsetTerm(ModBufs(NO) ? "[There are modified buffers]" : NullStr);
	(void) kill(0, SIGTSTP);
	ResetTerm();
	ClAndRedraw();
}
#endif


#ifndef	MAC
void
jcloseall()
{
	tmpclose();
#ifdef UNIX
	recclose();
#endif
#ifdef	LOAD_AV
	closekmem();
#endif	/* LOAD_AV */
}

void
Push()
{
#ifndef	MSDOS
	int	pid;
	SIGRESULT	(*old_quit) ptrproto((int)) = signal(SIGQUIT, SIG_IGN);
#endif	/* !MSDOS */
	SIGRESULT	(*old_int) ptrproto((int)) = signal(SIGINT, SIG_IGN);
# ifdef	PIPEPROCS
	int	started;
# endif

#ifndef	MSDOS
#ifdef	IPROCS
	SigHold(SIGCHLD);
#endif
#ifdef	WINRESIZE
	SigHold(SIGWINCH);
#endif
	alarm((unsigned)0);
# ifdef	PIPEPROCS
	started = kbd_stop();
# endif
	switch (pid = fork()) {
	case -1:
# ifdef	PIPEPROCS
		if (started)
			(void) kbd_strt();
# endif
		complain("[Fork failed: %s]", strerror(errno));
		/*NOTREACHED*/

	case 0:
		UnsetTerm(ModBufs(NO) ? "[There are modified buffers]" : NullStr);
#ifdef	WINRESIZE
		SigRelse(SIGWINCH);
#endif
#ifdef	IPROCS
		SigRelse(SIGCHLD);
#endif
		(void) signal(SIGTERM, SIG_DFL);
#else	/* MSDOS */
		UnsetTerm(ModBufs(NO) ? "[There are modified buffers]" : NullStr);
#endif	/* MSDOS */
		(void) signal(SIGINT, SIG_DFL);
#ifdef	UNIX
		(void) signal(SIGQUIT, SIG_DFL);
		jcloseall();
		/* note that curbuf->bfname may be NULL */
		execl(Shell, basename(Shell), "-is", pr_name(curbuf->b_fname, NO),
			(char *)NULL);
		raw_complain("[Execl failed: %s]", strerror(errno));
		_exit(1);
	}
#ifdef	IPROCS
	SigRelse(SIGCHLD);
#endif
	dowait(pid, (int *) NULL);
#endif	/* UNIX */
#ifdef	MSDOS
	break_rst();
	if (spawnl(0, Shell, basename(Shell), (char *)NULL) == -1)
		message("[Spawn failed]");
#endif	/* MSDOS */
#ifndef	MAC
	ResetTerm();
#endif
#ifdef	WINRESIZE
	SigRelse(SIGWINCH);
#endif
	ClAndRedraw();
#ifndef	MSDOS
	(void) signal(SIGQUIT, old_quit);
#else	/* MSDOS */
	break_off();
	getCWD();
#endif	/* MSDOS */
	(void) signal(SIGINT, old_int);
#ifdef UNIX
	if (UpdFreq != 0)
		(void) alarm((unsigned) (UpdFreq - (time((time_t *)NULL) % UpdFreq)));
#endif
# ifdef	PIPEPROCS
	if (started)
		(void) kbd_strt();
# endif
}
#endif	/* MAC */

bool	OKXonXoff = OFF;	/* ^S and ^Q initially DON'T work */
int	IntChar = CTL(']');

private void
ttsize()
{
#ifdef	UNIX
#   ifdef	TIOCGWINSZ
	struct winsize win;

	if (ioctl(0, TIOCGWINSZ, (UnivPtr) &win) == 0) {
		if (win.ws_col)
			CO = win.ws_col;
		if (win.ws_row)
			LI = win.ws_row;
	}
#   else	/* !TIOCGWINSZ */
#	ifdef	BTL_BLIT
#include <sys/jioctl.h>
	struct jwinsize jwin;

	if (ioctl(0, JWINSIZE, &jwin) == 0) {
		if (jwin.bytesx)
			CO = jwin.bytesx;
		if (jwin.bytesy)
			LI = jwin.bytesy;
	}
#	endif	/* BTL_BLIT */
#   endif	/* !TIOCGWINSZ */
#endif	/* UNIX */
#ifdef	MAC
	CO = getCO();	/* see mac.c */
	LI = getLI();
	Windchange = YES;
	clr_page();
#endif
	ILI = LI - 1;
}

#ifdef	BIFF
private void
biff_init()
{
	dw_biff = ((BiffChk) &&
#   ifndef	BSD4_2
		   ((tt_name != NULL) || (tt_name = ttyname(0))) &&
		   (stat(tt_name, &tt_stat) != -1) &&
#   else
		   (fstat(0, &tt_stat) != -1) &&
#   endif
		   (tt_stat.st_mode & S_IEXEC));	/* he's using biff */

}

private void
biff(on)
int	on;
{
	if (dw_biff == NO)
		return;
#   ifndef	BSD4_2
	(void) chmod(tt_name, on ? tt_stat.st_mode :
				   (tt_stat.st_mode & ~S_IEXEC));
#   else
	(void) fchmod(0, on ? tt_stat.st_mode :
			      (tt_stat.st_mode & ~S_IEXEC));
#   endif
}

#endif	/* BIFF */

private void
ttinit()
{
#ifdef	BIFF
	biff_init();
#endif
#ifdef	TIOCSLTC
	(void) ioctl(0, TIOCGLTC, (UnivPtr) &ls[OFF]);
	ls[ON] = ls[OFF];
	ls[ON].t_suspc = (char) -1;
	ls[ON].t_dsuspc = (char) -1;
	ls[ON].t_flushc = (char) -1;
	ls[ON].t_lnextc = (char) -1;
#endif

#ifdef	TIOCGETC
	/* Change interupt and quit. */
	(void) ioctl(0, TIOCGETC, (UnivPtr) &tc[OFF]);
	tc[ON] = tc[OFF];
	tc[ON].t_intrc = IntChar;
	tc[ON].t_quitc = (char) -1;
	if (OKXonXoff) {
		tc[ON].t_stopc = (char) -1;
		tc[ON].t_startc = (char) -1;
	}
#endif	/* TIOCGETC */
	do_sgtty();
}

private int	done_ttinit = NO;

#ifndef	MSDOS
private
#endif
void
do_sgtty()
{
#ifdef	UNIX
# ifdef	TERMIO
	(void) ioctl(0, TCGETA, (char *) &sg[OFF]);
# endif
# ifdef TERMIOS
	(void) tcgetattr(0, &sg[OFF]);
# endif

# ifdef SGTTY
	(void) gtty(0, &sg[OFF]);
# endif	/* SYSV */
	sg[ON] = sg[OFF];

# ifdef	LPASS8
	(void) ioctl(0, TIOCLGET, (UnivPtr) &lmword[OFF]);
	lmword[ON] = lmword[OFF];
	if (MetaKey)
		lmword[ON] |= LPASS8;
# endif

# ifdef LTILDE
	if (Hazeltine)
		lmword[ON] &= ~LTILDE;
# endif

# if defined(TERMIO) || defined(TERMIOS)
#ifdef TAB3
	TABS = !((sg[OFF].c_oflag & TAB3) == TAB3);
#endif
#ifdef CBAUD
	ospeed = sg[OFF].c_cflag & CBAUD;
#endif
	if (OKXonXoff)
		sg[ON].c_iflag &= ~(IXON | IXOFF);
	sg[ON].c_iflag &= ~(INLCR|ICRNL|IGNCR|ISTRIP);
	/* sg[ON].c_lflag &= ~(ICANON|ECHO); */
	sg[ON].c_cflag &= ~(CSIZE|PARENB);
	sg[ON].c_cflag |= CS8;
	sg[ON].c_lflag &= ~(ISIG|ICANON|ECHO|IEXTEN);
#ifndef OCRNL
#define OCRNL 0
#endif
	sg[ON].c_oflag &= ~(OCRNL|ONLCR);
#  ifdef _POSIX_VDISABLE
	/* The following characters cause signals in System Vr4.
	 * We should perhaps handle them; for now, we suppress them.
	 */
	sg[ON].c_cc[VQUIT] = _POSIX_VDISABLE;
#ifdef VSWTCH
	sg[ON].c_cc[VSWTCH] = _POSIX_VDISABLE;
#endif
	sg[ON].c_cc[VSUSP] = _POSIX_VDISABLE;
	sg[ON].c_cc[VDSUSP] = _POSIX_VDISABLE;
#  else
	sg[ON].c_cc[VINTR] = IntChar;
	sg[ON].c_cc[VQUIT] = (char) -1;
#  endif /* _POSIX_VDISABLE */
	sg[ON].c_cc[VMIN] = 1;
	sg[ON].c_cc[VTIME] = 1;
# endif /* TERMIO || TERMIOS */

# if defined(SGTTY) || defined(BRLUNIX)
	TABS = !(sg[OFF].sg_flags & XTABS);
	sg[ON].sg_flags &= ~XTABS;
	ospeed = sg[OFF].sg_ospeed;
#  ifdef	BRLUNIX
	sg[ON].sg_flags &= ~(ECHO | CRMOD);
	sg[ON].sg_flags |= CBREAK;

	/* VT100 Kludge: leave STALL on for flow control if DC3DC1 (Yuck.) */
	sg[ON].sg_xflags &= ~((sg[ON].sg_xflags&DC3DC1 ? 0 : STALL) | PAGE);
#  else
	sg[ON].sg_flags &= ~(ECHO | CRMOD);
#  endif	/* BRLUNIX */

#  ifdef	LPASS8
	sg[ON].sg_flags |= CBREAK;
#  else
	sg[ON].sg_flags |= (MetaKey ? RAW : CBREAK);
#  endif
# endif	/* SGTTY */
#endif	/* UNIX */

#ifdef	MSDOS
# ifndef	IBMPC
	setmode(1, 0x8000);
# endif	/* IBMPC */
	TABS = NO;
#endif	/* MSDOS */
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
bool	n;
{
	if (!done_ttinit && !n)	/* Try to reset before we've set! */
		return;
#ifdef	UNIX
# ifdef	TERMIO
	(void) ioctl(0, TCSETAW, (UnivPtr) &sg[n]);
# endif	/* TERMIO */

# ifdef	TERMIOS
	(void) tcsetattr(0, TCSADRAIN, &sg[n]);
# endif /* TERMIOS */

# ifdef	SGTTY
#  ifdef TIOCSETN
	(void) ioctl(0, TIOCSETN, (UnivPtr) &sg[n]);
#  else
	(void) stty(0, &sg[n]);
#  endif
# endif

# ifdef	TIOCGETC
	(void) ioctl(0, TIOCSETC, (UnivPtr) &tc[n]);
# endif	/* TIOCSETC */
# ifdef	TIOCSLTC
	(void) ioctl(0, TIOCSLTC, (UnivPtr) &ls[n]);
# endif	/* TIOCSLTC */
# ifdef	LPASS8
	(void) ioctl(0, TIOCLSET, (UnivPtr) &lmword[n]);
# endif
#endif	/* UNIX */

#ifdef	MSDOS
# ifndef	IBMPC
	setmode(1, n? 0x8000 : 0x4000);
# endif
#endif	/* MSDOS */
	done_ttinit = YES;
#ifdef	BIFF
	biff(!n);
#endif
}

int	this_cmd,
	last_cmd,
	LastKeyStruck,
	MetaKey = OFF;

int
getch()
{
	register int	c,
			peekc;

	if (Inputp) {
		if ((c = *Inputp++) != '\0')
			return LastKeyStruck = c;
		Inputp = NULL;
	}

	if (InJoverc)
		return EOF;	/* somethings wrong if Inputp runs out while
				   we're reading a .joverc file. */

#ifndef	MSDOS
	if (ModCount >= SyncFreq) {
		ModCount = 0;
		SyncRec();
	}
#endif	/* MSDOS */

	/* If there are no ungetc'd characters,
	   AND we're interactive or we're not executing a macro,
	   we read from the terminal (i.e., jgetchar()).
	   Note: characters only get put in macros from inside this if. */
	if (((peekc = c = Peekc()) == EOF) &&
	    (Interactive || ((c = mac_getc()) == EOF))) {
		/* So messages that aren't error messages don't
		 * hang around forever.
		 * Note: this code is duplicated in SitFor()!
		 */
		if (!UpdMesg && !Asking && mesgbuf[0] != '\0' && !errormsg)
			message(NullStr);
		redisplay();
#ifdef	UNIX
		inIOread = YES;
#endif
		if ((c = jgetchar()) == EOF)
			finish(SIGHUP);
#ifdef	UNIX
		inIOread = NO;
#endif

		if (!Interactive && InMacDefine)
			mac_putc(c);
	}
	if (peekc == EOF)	/* don't add_stroke peekc's */
		add_stroke(c);
	return LastKeyStruck = c;
}

#ifdef	UNIX
private void
dorecover()
{
	/* Since recover is a normal cooked mode program, reset the terminal */
	UnsetTerm(NullStr);
#ifdef	PIPEPROCS
	kbd_kill();		/* kill the keyboard process */
#endif
	execl(Recover, "recover", "-d", TmpFilePath, (char *) NULL);
	writef("%s: execl failed! %s\n", Recover, strerror(errno));
	flushscreen();
	_exit(-1);
	/* NOTREACHED */
}
#endif	/* UNIX */

void
ShowVersion()
{
	s_mess("Jonathan's Own Version of Emacs (%s)", version);
}

private void
UNIX_cmdline(argc, argv)
int	argc;
char	*argv[];
{
	int	lineno = 0,
		nwinds = 1;
	Buffer	*b;

	ShowVersion();
	while (argc > 1) {
		if (argv[1][0] != '-' && argv[1][0] != '+') {
			bool	force = (nwinds > 0 || lineno != 0);

#ifdef	MSDOS
			strlwr(argv[1]);
#endif
			minib_add(argv[1], force);
			b = do_find(nwinds > 0 ? curwind : (Window *) NULL,
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
#ifndef	MAC
			case 'p':
				argv += 1;
				argc -= 1;
				if (argv[1] != NULL) {
					SetBuf(do_find(curwind, argv[1], NO));
					ErrParse();
					nwinds = 0;
				}
				break;
#endif
			case 't':
				/* check if syntax is -tTag or -t Tag */
				if (argv[1][2] != '\0') {
					find_tag(&(argv[1][2]), YES);
				} else {
					argv += 1;
					argc -= 1;
					if (argv[1] != NULL)
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
			case '\0':
				lineno = -1;	/* goto end of file ... */
				break;		/* just like some people's */
		}				/* favourite editor */
		argv += 1;
		argc -= 1;
	}
}

void
raw_scream(m)
const char	*m;
{
	write(2, (UnivConstPtr)m, strlen(m));
}

#ifdef	STDARGS
void
error(const char *fmt, ...)
#else
/*VARARGS1*/ void
error(fmt, va_alist)
	const char	*fmt;
	va_dcl
#endif
{
	va_list	ap;

	if (fmt) {
		va_init(ap, fmt);
		format(mesgbuf, sizeof mesgbuf, fmt, ap);
		va_end(ap);
		UpdMesg = YES;
	}
	rbell();
	longjmp(mainjmp, ERROR);
}

#ifdef	STDARGS
void
complain(const char *fmt, ...)
#else
/*VARARGS1*/ void
complain(fmt, va_alist)
	const char	*fmt;
	va_dcl
#endif
{
	va_list	ap;

	if (fmt) {
		va_init(ap, fmt);
		format(mesgbuf, sizeof mesgbuf, fmt, ap);
		va_end(ap);
		UpdMesg = YES;
	}
	rbell();
	longjmp(mainjmp, COMPLAIN);
}

#ifdef	STDARGS
void
raw_complain(const char *fmt, ...)
#else
/*VARARGS1*/ void
raw_complain(fmt, va_alist)
	const char	*fmt;
	va_dcl
#endif
{
	va_list	ap;

	if (fmt) {
		va_init(ap, fmt);
		format(mesgbuf, sizeof mesgbuf, fmt, ap);
		va_end(ap);
		raw_scream(mesgbuf);
	}
}

#ifdef	STDARGS
void
confirm(const char *fmt, ...)
#else
/*VARARGS1*/ void
confirm(fmt, va_alist)
	const char	*fmt;
	va_dcl
#endif
{
	char	*yorn;
	va_list	ap;

	va_init(ap, fmt);
	format(mesgbuf, sizeof mesgbuf, fmt, ap);
	va_end(ap);
	yorn = ask((char *)NULL, mesgbuf);
	if (*yorn != 'Y' && *yorn != 'y')
		longjmp(mainjmp, COMPLAIN);
}

int	RecDepth = 0;

void
Recur()
{
	char	bname[128];
	Mark	*m;

	swritef(bname, sizeof(bname), "%s", curbuf->b_name);
	m = MakeMark(curline, curchar, M_FLOATER);

	RecDepth += 1;
	UpdModLine = YES;
	DoKeys(NO);	/* NO means not first time */
	UpdModLine = YES;
	RecDepth -= 1;
	SetBuf(do_select(curwind, bname));
	if (!is_an_arg())
		ToMark(m);
	DelMark(m);
}

#ifdef	MAC
jmp_buf auxjmp;
#endif

private int	iniargc;	/* main sets these for DoKeys() */
private char	**iniargv;

private void
DoKeys(firsttime)
bool	firsttime;
{
	int	c;
	jmp_buf	savejmp;

	push_env(savejmp);

	switch (setjmp(mainjmp)) {
	case 0:
		if (firsttime)
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
			if (ModBufs(NO)) {
				rbell();
				if (CharUpcase(*ask("No",
"Some buffers haven't been saved; leave anyway? ")) != 'Y')
					break;
			}
#ifdef	IPROCS
			KillProcs();
#endif
		}
		pop_env(savejmp);
		return;

	case ERROR:
		getDOT();	/* God knows what state linebuf was in */
		/*FALLTHROUGH*/
	case COMPLAIN:
	    {
		gc_openfiles();		/* close any files we left open */
		errormsg = YES;
		unwind_macro_stack();
		Asking = NO;
		curwind->w_bufp = curbuf;
		DisabledRedisplay = NO;
		redisplay();
		break;
	    }
	}

	this_cmd = last_cmd = 0;

	for (;;) {
#ifdef	MAC
		setjmp(auxjmp);
#endif
		if (this_cmd != ARG_CMD) {
			clr_arg_value();
			last_cmd = this_cmd;
			init_strokes();
		}
#ifdef	MAC
		HiliteMenu(0);
		EventCmd = NO;
		menus_on();
#endif
		c = getch();
		if (c == EOF)
			continue;
		dispatch(c);
	}
}

private char **
scanvec(args, str)
register char	**args,
		*str;
{
	while (*args) {
		if (strcmp(*args, str) == 0)
			return args;
		args += 1;
	}
	return NULL;
}

#ifdef	UNIX
int	UpdFreq = 30,
	inIOread = NO;

private SIGRESULT
updmode(junk)
int	junk;	/* passed in on signal; of no interest */
{
	int save_errno = errno;	/* Subtle, but necessary! */

	UpdModLine = YES;
	if (inIOread)
		redisplay();
#ifndef	BSD_SIGS
	(void) signal(SIGALRM, updmode);
#endif
	if (UpdFreq != 0)
		(void) alarm((unsigned) (UpdFreq - (time((time_t *)NULL) % UpdFreq)));
	errno = save_errno;
	SIGRETURN;
}
#endif	/* UNIX */

#ifdef	MSDOS
# ifndef	IBMPC
char	ttbuf[JBUFSIZ];
# endif	/* IBMPC */
#endif	/* MSDOS */

#ifdef	WINRESIZE
#ifndef	MAC
private
#endif
SIGRESULT
win_reshape(junk)
int	junk;	/* passed in when invoked by a signal; of no interest */
{
	int save_errno = errno;	/* Subtle, but necessary! */
	register int	oldLI;
	register int newsize, total;
	register Window *wp;

#ifdef	UNIX
	(void) SigHold(SIGWINCH);
#endif
	/*
	 * Save old number of lines.
	 */
	oldLI = LI;

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
		newsize = LI * wp->w_height / oldLI;

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
	cl_scr(YES);
	flushscreen();
	redisplay();

#ifdef	UNIX
	(void) SigRelse(SIGWINCH);
	(void) signal(SIGWINCH, win_reshape);
#endif
	errno = save_errno;
	SIGRETURN;
}
#endif

void

#ifdef	MAC	/* will get args from user, if option key held during launch */
main()
{
	int argc;
	char **argv;
#else
main(argc, argv)
int	argc;
char	*argv[];
{
#endif	/* MAC */
	char	*cp;
	char	ttbuf[MAXTTYBUF];
#ifdef	pdp11
	/* On the PDP-11, UNIX allocates at least 8K.
	 * In order not to waste this space, we allocate
	 * a bunch of buffers as autos.
	 */

	char	s_iobuff[LBSIZE],
		s_genbuf[LBSIZE],
		s_linebuf[LBSIZE];

	iobuff = s_iobuff;
	genbuf = s_genbuf;
	linebuf = s_linebuf;
#endif

#ifdef	MAC
	MacInit();		/* initializes all */
	{
		extern bool	make_cache proto((void));

		if (!make_cache())
			exit(-1);
	}
	argc = getArgs(&argv);
#endif	/* MAC */

	iniargc = argc;
	iniargv = argv;

	if (setjmp(mainjmp)) {
		writef("\rAck! I can't deal with error \"%s\" now.\n\r", mesgbuf);
		finish(SIGIOT);	/* some bad signal (not SIGHUP) */
	}

#ifdef	MSDOS
	/* import the temporary file path from the environment and
	   fix the string, so that we can append a slash safely	*/

	if (((cp = getenv("TMP")) || (cp = getenv("TMPDIR"))) &&
	    (*cp != '\0')) {
		strcpy(TmpFilePath, cp);
		cp = &TmpFilePath[strlen(TmpFilePath)-1];
		if ((*cp == '/') || (*cp == '\\'))
			*cp = '\0';
	}
	ShFlags[0] = switchar();
#endif	/* MSDOS */

	getTERM();	/* Get terminal. */
	if (getenv("METAKEY"))
		MetaKey = ON;
	ttsize();
#ifdef	MAC
	InitEvents();
#else
	InitCM();
#endif

	d_cache_init();		/* initialize the disk buffer cache */
#ifdef	MSDOS
	if ((cp = getenv("COMSPEC")) && (*cp != '\0')) {
		strcpy(Shell, cp);
	}
	if ((cp = getenv("DESCRIBE")) && (*cp != '\0'))
	   strcpy(CmdDb, cp);
#else	/* !MSDOS */
#ifndef	MAC
	if ((cp = getenv("SHELL"))!=NULL && (*cp != '\0')) {
		strcpy(Shell, cp);
	}
#endif
#endif	/* !MSDOS */

	make_scr();
	mac_init();	/* Initialize Macros */
	winit();	/* Initialize Window */
#ifdef	IPROCS
	pinit();	/* Pipes/process initialization */
#endif
	buf_init();

	{
		char	**argp;

		if ((argp = scanvec(argv, "-d"))!=NULL
#ifdef	UNIX
		    && chkCWD(argp[1])
#endif
		    )
			setCWD(argp[1]);
		else
			getCWD();	/* After we setup curbuf in case we have to getwd() */
	}

	HomeDir = getenv("HOME");
	if (HomeDir == NULL)
		HomeDir = "/";
	HomeLen = strlen(HomeDir);

#ifdef	UNIX
	if ((cp = getenv("MAIL")) != NULL) {
		strcpy(Mailbox, cp);
	} else {
		swritef(Mailbox, sizeof(Mailbox), "%s/%s",
			MAILSPOOL, getenv("LOGNAME"));
	}
#endif

	InitKeymaps();

	ttinit();	/* initialize terminal (before ~/.joverc) */
	settout(ttbuf);	/* not until we know baudrate */
#ifndef	MAC
	ResetTerm();
#endif

	(void) joverc(Joverc);			/* system wide .joverc */
	cp = NULL;
#ifndef	MAC
	/* If a JOVERC environment variable is set, then use that instead */
	if ((cp = getenv("JOVERC"))!=NULL && (*cp != '\0'))
	   (void) joverc(cp);
#endif	/* !MAC */
	if (!scanvec(argv, "-j") && (!cp || *cp == '\0')) {
		char	tmpbuf[100];

		swritef(tmpbuf, sizeof(tmpbuf), "%s/.joverc", HomeDir);
		(void) joverc(tmpbuf);		/* .joverc in home directory */
	}

#ifndef	MSDOS
	if (scanvec(argv, "-r"))
		dorecover();
	if (scanvec(argv, "-rc"))
		FullRecover();
#endif	/* MSDOS */

#ifdef	MSDOS
	(void) signal(SIGINT, SIG_IGN);
	break_off();	/* disable ctrl-c checking */
#endif	/* MSDOS */
#ifdef	UNIX
	(void) signal(SIGHUP, finish);
	(void) signal(SIGINT, finish);
	(void) signal(SIGBUS, finish);
	(void) signal(SIGSEGV, finish);
	(void) signal(SIGPIPE, finish);
	(void) signal(SIGTERM, SIG_IGN);
# ifdef	WINRESIZE
	(void) signal(SIGWINCH, win_reshape);
# endif
	/* set things up to update the modeline every UpdFreq seconds */
	(void) signal(SIGALRM, updmode);
	if (UpdFreq != 0)
		(void) alarm((unsigned) (UpdFreq - (time((time_t *)NULL) % UpdFreq)));
#endif	/* UNIX */
	cl_scr(YES);
	flushscreen();
	RedrawDisplay();	/* start the redisplay process. */
	DoKeys(YES);
	finish(0);
}

#ifdef	MSDOS

#include <dos.h>

private	char break_state;

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
