/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

/* Contains the main loop initializations, and some system dependent
   type things, e.g. putting terminal in CBREAK mode, etc. */

#include "jove.h"
#include "io.h"
#include "termcap.h"

#include <varargs.h>
#include <sys/stat.h>
#include <signal.h>
#include <errno.h>
#ifndef SYSV
#include <sgtty.h>
#include <fcntl.h>
#else
#include <termio.h>
#endif SYSV

#ifdef TIOCSLTC
struct ltchars	ls1,
		ls2;
#endif TIOCSLTC

#ifdef TIOCGETC
struct tchars	tc1,
		tc2;
#endif

#ifdef BRLUNIX
struct sg_brl	sg1, sg2;
#else
#ifdef SYSV
struct termio	sg1, sg2;
#else SYSV
struct sgttyb	sg1, sg2;
#endif SYSV
#endif BRLUNIX

#ifdef BIFF
private struct stat	tt_stat;	/* for biff */
#ifndef BSD4_2
private char	*tt_name = 0;		/* name of the control tty */
extern char	*ttyname();		/* for systems w/o fchmod ... */
#endif
private int	dw_biff = NO;		/* whether or not to fotz at all */
#endif

time_t	time0;			/* when jove started up */
int	errormsg;
extern char	*tfname;
char	NullStr[] = "";

finish(code)
{
	int	CoreDump = (code != 0 && code != SIGHUP),
		DelTmps = 1;		/* Usually we delete them. */

#ifdef LSRHS
	if (CoreDump)
		setdump(1);
#endif
	if (code == SIGINT) {
		char	c;

#ifndef MENLO_JCL
		(void) signal(code, finish);
#endif
		f_mess("Abort (Type 'n' if you're not sure)? ");
		(void) read(0, &c, 1);
		message(NullStr);
		if ((c & 0377) != 'y') {
			redisplay();
			return;
		}
	}
	ttyset(OFF);
	UnsetTerm(NullStr);
	if (code != 0) {
		if (!Crashing) {
			Crashing++;
			lsave();
			SyncRec();
			printf("JOVE CRASH!! (code %d)\n", code);
			if (ModBufs(1)) {
				printf("Your buffers have been saved.\n");
				printf("Use \"jove_recover\" or \"jove -r\"\n");
				printf("to have a look at them.\n");
				DelTmps = 0;	/* Don't delete anymore. */
			} else
				printf("You didn't lose any work.\n");
		} else
			printf("\r\nYou may have lost your work!\n");
	}
	flusho();
	if (DelTmps) {
		tmpclose();
		recclose();
	}
	if (CoreDump)
		abort();
#ifdef PROFILING
	exit(exp_p);
#else
	_exit(exp_p);
#endif
}

private char	smbuf[20],
		*bp = smbuf;
private int	nchars = 0;

private char	peekbuf[10],
		*peekp = peekbuf;

#ifdef SYSV
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
    return;
}
#endif SYSV

Peekc()
{
	int	c;

	if (peekp == peekbuf)
		c = -1;
	else
		c = *--peekp & 0377;
	return c;
}

Ungetc(c)
{
	if (peekp == &peekbuf[(sizeof peekbuf) - 1])
		return;		/* Sorry, can't oblige you ... */
	*peekp++ = c;
}

char	*Inputp = 0;

#ifdef IPROCS
#ifdef PIPEPROCS
getchar()
{
	extern int	errno;
	register int	c;

	if (nchars <= 0) {
		do
			nchars = read(0, smbuf, sizeof smbuf);
#ifdef SYSV
		while (nchars == 0 || (nchars < 0 && errno == EINTR));
		if (nchars < 0)
#else
		while (nchars < 0 && errno == EINTR);
		if (nchars <= 0)
#endif SYSV
			finish(SIGHUP);
		bp = smbuf;
		InputPending = nchars > 1;
	}
	if (((c = *bp) & 0200) && MetaKey != 0) {
		*bp = (c & 0177);
		return '\033';
	}
	nchars--;
	return (*bp++ & 0177);
}
#else PIPEPROCS
getchar()
{
	extern int	global_fd,
			NumProcs,
			errno;
	register int	tmp,
			nfds;
	int	reads,
		c;

	if (nchars <= 0) {
		/* Get a character from the keyboard, first checking for
		   any input from a process.  Handle that first, and then
		   deal with the terminal input. */
		if (NumProcs > 0) {
			do {
				do {
					reads = global_fd;
					nfds = select(32, &reads, (int *) 0, (int *) 0, (struct timeval *) 0);
				} while (nfds < 0 && errno == EINTR);

				switch (nfds) {
				case -1:
					printf("\rerror %d in select %d", errno, global_fd);
					global_fd = 1;
					break;
				default:
					if (reads & 01) {
						nchars = read(0, smbuf, sizeof(smbuf));
						reads &= ~01;
						--nfds;
					}

					while (nfds--) {
						tmp = ffs(reads) - 1;
						read_proc(tmp);
						reads &= ~tmp;
					}

					break;
				}
			} while (nchars <= 0);
		} else {
			do
				nchars = read(0, smbuf, sizeof(smbuf));
			while (nchars < 0 && errno == EINTR);
		}

		if (nchars <= 0)
			finish(SIGHUP);

		bp = smbuf;
		InputPending = (nchars > 1);
	}

	if (((c = *bp) & 0200) && MetaKey != 0) {
		*bp = (c & 0177);
		return '\033';
	}
	nchars--;
	return *bp++ & 0377;
}
#endif PIPEPROCS
#else IPROCS
getchar()
{
	extern int	errno;
	register int	c;

	if (nchars <= 0) {
		do
			nchars = read(0, smbuf, sizeof smbuf);
		while (nchars < 0 && errno == EINTR);

		if (nchars <= 0)
			finish(SIGHUP);
		bp = smbuf;
		InputPending = nchars > 1;
	}
	if (((c = *bp) & 0200) && MetaKey != 0) {
		*bp = (c & 0177);
		return '\033';
	}
	nchars--;
	return *bp++ & 0377;
}
#endif IPROCS

int	InputPending = 0;

/* Returns non-zero if a character waiting */

charp()
{
	int	some = 0;

	if (InJoverc != 0 || nchars > 0 || Inputp != 0)
		return 1;
#ifdef BRLUNIX
	{
		static struct sg_brl gttyBuf;

		gtty(0, (char *) &gttyBuf);
		if (gttyBuf.sg_xflags & INWAIT)
			some++;
	}
#endif
#ifdef FIONREAD
	{
		long c;

		if (ioctl(0, FIONREAD, (struct sgttyb *) &c) == -1)
			c = 0;
		some = (c > 0);
	}
#endif FIONREAD
#ifdef SYSV
	setblock(0, 0);		/* turn blocking off */
	nchars = read(0, smbuf, sizeof smbuf);	/* Is anything there? */
	setblock(0, 1);		/* turn blocking on */
	if (nchars > 0)		/* something was there */
	    bp = smbuf;		/* make sure bp points to it */
	some = (nchars > 0);	/* just say we found something */
#endif SYSV
#ifdef c70
	some = !empty(0);
#endif
	return some;
}

ResetTerm()
{
	putpad(TI, 1);
	putpad(VS, 1);
	putpad(KS, 1);
#ifdef BIFF
	if (BiffChk != dw_biff)
		biff_init();
	/* just in case we changed our minds about whether to deal with
	   biff */
#endif
	chkmail(YES);	/* force it to check to we can be accurate */
	do_sgtty();	/* this is so if you change baudrate or stuff
			   like that, JOVE will notice. */
	ttyset(ON);
}

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

#ifdef JOB_CONTROL
PauseJove()
{
	UnsetTerm(ModBufs(0) ? "[There are modified buffers]" : NullStr);
	(void) kill(0, SIGTSTP);
	ResetTerm();
	ClAndRedraw();
}
#endif

Push()
{
	int	pid;

	switch (pid = fork()) {
	case -1:
		complain("[Fork failed]");

	case 0:
		UnsetTerm(NullStr);
		(void) signal(SIGTERM, SIG_DFL);
		(void) signal(SIGINT, SIG_DFL);
		execl(Shell, basename(Shell), 0);
		message("[Execl failed]");
		_exit(1);

	default:
	    {
	    	int	(*old_int)() = signal(SIGINT, SIG_IGN);
		int	(*old_quit)() = signal(SIGQUIT, SIG_IGN);

#ifdef IPROCS
		sighold(SIGCHLD);
#endif
	    	dowait(pid, (int *) 0);
#ifdef IPROCS
		sigrelse(SIGCHLD);
#endif
	    	ResetTerm();
	    	ClAndRedraw();
	    	(void) signal(SIGINT, old_int);
		(void) signal(SIGQUIT, old_quit);
	    }
	}
}

int	OKXonXoff = 0;		/* ^S and ^Q initially DON'T work */

ttsize()
{
#ifdef TIOCGWINSZ
	struct winsize win;

	if (ioctl (0, TIOCGWINSZ, &win) == 0) {
		if (win.ws_col)
			CO = win.ws_col;
		if (win.ws_row)
			LI = win.ws_row;
	}
#else TIOCGWINSZ
#ifdef BTL_BLIT
#include <sys/jioctl.h>
	struct jwinsize jwin;

	if (ioctl(0, JWINSIZE, &jwin) == 0) {
		if (jwin.bytesx)
			CO = jwin.bytesx;
		if (jwin.bytesy)
			LI = jwin.bytesy;
	}
#endif BTL_BLIT
#endif TIOCGWINSZ
	ILI = LI - 1;
}

#ifdef BIFF
biff_init()
{
	dw_biff = ((BiffChk) &&
#ifndef BSD4_2
		   ((tt_name != 0) || (tt_name = ttyname(0))) &&
		   (stat(tt_name, &tt_stat) != -1) &&
#else
		   (fstat(0, &tt_stat) != -1) &&
#endif
		   (tt_stat.st_mode & S_IEXEC));	/* he's using biff */

}

biff(on)
{
	if (dw_biff == NO)
		return;
#ifndef BSD4_2
	(void) chmod(tt_name, on ? tt_stat.st_mode :
				   (tt_stat.st_mode & ~S_IEXEC));
#else
	(void) fchmod(0, on ? tt_stat.st_mode :
			      (tt_stat.st_mode & ~S_IEXEC));
#endif
}

#endif

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
	tc2.t_intrc = CTL(]);
	tc2.t_quitc = (char) -1;
	if (OKXonXoff) {
		tc2.t_stopc = (char) -1;
		tc2.t_startc = (char) -1;
	}
#endif TIOCGETC
	do_sgtty();
}

private int	done_ttinit = 0;

do_sgtty()
{
#ifdef SYSV
	(void) ioctl(0, TCGETA, (char *) &sg1);
#else
	(void) gtty(0, &sg1);
#endif SYSV
	sg2 = sg1;

#ifdef SYSV
	TABS = !((sg1.c_oflag & TAB3) == TAB3);
	ospeed = sg1.c_cflag & CBAUD;

	sg2.c_iflag &= ~(INLCR|ICRNL|IGNCR);
	sg2.c_lflag &= ~(ISIG|ICANON|ECHO);
	sg2.c_oflag &= ~(OCRNL|ONLCR);
	sg2.c_cc[VMIN] = sizeof smbuf;
	sg2.c_cc[VTIME] = 1;
#else
	TABS = !(sg1.sg_flags & XTABS);
	ospeed = sg1.sg_ospeed;
#ifdef BRLUNIX
	sg2.sg_flags &= ~(ECHO | CRMOD);
	sg2.sg_flags |= CBREAK;

	/* VT100 Kludge: leave STALL on for flow control if DC3DC1 (Yuck.) */
	sg2.sg_xflags &= ~((sg2.sg_xflags&DC3DC1 ? 0 : STALL) | PAGE);
#else
	sg2.sg_flags &= ~(ECHO | CRMOD);
#endif BRLUNIX

#ifdef EUNICE
	sg2.sg_flags |= RAW;	/* Eunice needs RAW mode last I heard. */
#else
#ifdef PURDUE_EE
#   ifdef pdp11
	sg2.sg_flags |= RAW;
#   else
	sg2.sg_flags |= (MetaKey ? RAW : CBREAK);
#   endif
#else
	sg2.sg_flags |= (MetaKey ? RAW : CBREAK);
#endif PURDUE_EE
#endif EUNICE
#endif SYSV
}

tty_reset()
{
	if (!done_ttinit)
		return;
	ttyset(OFF);	/* go back to original modes */
	ttinit();
	ttyset(ON);
}

/* If n is OFF reset to original modes */

ttyset(n)
{
	if (!done_ttinit && n == 0)	/* Try to reset before we've set! */
		return;
#ifdef SYSV
	(void) ioctl(0, TCSETAW, n == 0 ? (struct sgttyb *) &sg1 : (struct sgttyb *) &sg2);
#else
#ifdef BRLUNIX
	(void) stty(0, n == 0 ? (struct sgttyb *) &sg1 : (struct sgttyb *) &sg2);
#else
	(void) ioctl(0, TIOCSETN, n == 0 ? (struct sgttyb *) &sg1 : (struct sgttyb *) &sg2);
#endif BRLUNIX
#endif SYSV

#ifdef TIOCSETC
	(void) ioctl(0, TIOCSETC, n == 0 ? (struct sgttyb *) &tc1 : (struct sgttyb *) &tc2);
#endif TIOCSETC
#ifdef TIOCSLTC
	(void) ioctl(0, TIOCSLTC, n == 0 ? (struct sgttyb *) &ls1 : (struct sgttyb *) &ls2);
#endif TIOCSLTC
	done_ttinit = 1;
#ifdef BIFF
	biff(!n);
#endif
}

int	this_cmd,
	last_cmd;

dispatch(c)
register int	c;
{
	data_obj	*cp;

	this_cmd = 0;
	cp = mainmap[c & 0177];

	if (cp == 0) {
		rbell();
		exp = 1;
		exp_p = errormsg = 0;
		message(NullStr);
		return;
	}
	ExecCmd(cp);
}

int	LastKeyStruck,
	MetaKey = 0;

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

	if (ModCount >= SyncFreq) {
		ModCount = 0;
		SyncRec();
	}

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
#ifdef IPROCS
#  ifdef PIPEPROCS
		if (NumProcs > 0) {
			sigrelse(INPUT_SIG);
			sigrelse(SIGCHLD);
		}
#  endif
#endif
		inIOread = 1;
		if ((c = getchar()) == EOF)
			finish(SIGHUP);
		inIOread = 0;

#ifdef IPROCS
#  ifdef PIPEPROCS
		if (NumProcs > 0) {
			sighold(INPUT_SIG);
			sighold(SIGCHLD);
		}
#  endif
#endif
		if (!Interactive && (KeyMacro.m_flags & DEFINE))
			mac_putc(c);
	}
	if (peekc == -1)	/* Don't add_stroke peekc's */
		add_stroke(c);
	return LastKeyStruck = c;
}

dorecover()
{
	execl(RECOVER, "jove_recover", 0);
	printf("%s: execl failed!\n", RECOVER);
	flusho();
	_exit(-1);
}
		

ShowVersion()
{
	extern char	*version;

	s_mess("Jonathan's Own Version of Emacs (%s)", version);
}

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

			minib_add(argv[1], force ? YES : NO);
			b = do_find(nwinds > 0 ? curwind : (Window *) 0,
				    argv[1], force);
			if (force) {
				SetABuf(curbuf);
				SetBuf(b);
				SetLine(next_line(curbuf->b_first, lineno));
				if (nwinds > 1)
					NextWindow();
				if (nwinds)
					nwinds--;
			}
			lineno = 0;
		} else	switch (argv[1][1]) {
			case 'd':
				++argv;
				--argc;
				break;

			case 'j':	/* Ignore .joverc in HOME */
				break;

			case 'p':
				++argv;
				--argc;
				SetBuf(do_find(curwind, argv[1], 0));
				ParseAll();
				nwinds = 0;
				break;

			case 't':
				++argv;
				--argc;
				exp_p = 1;
				find_tag(argv[1], YES);
				break;

			case 'w':
				if (argv[1][2] == '\0')
					nwinds++;
				else
					nwinds += -1 + chr_to_int(&argv[1][2], 10, NIL);
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
				lineno = chr_to_int(&argv[1][1], 10, 0) - 1;
				break;
		}
		++argv;
		--argc;
	}
}

#ifdef lint
Ignore(a)
	char *a;
{

	a = a;
}

Ignorf(a)
	int (*a)();
{

	a = a;
}

Ignorl(a)
long	a;
{
	a = a;
}
#endif

/* VARARGS1 */

error(fmt, va_alist)
char	*fmt;
va_dcl
{
	va_list	ap;

	if (fmt) {
		va_start(ap);
		format(mesgbuf, sizeof mesgbuf, fmt, ap);
		va_end(ap);
		UpdMesg++;
	}
	rbell();
	(void) longjmp(mainjmp, ERROR);
}

/* VARARGS1 */

complain(fmt, va_alist)
char	*fmt;
va_dcl
{
	va_list	ap;

	if (fmt) {
		va_start(ap);
		format(mesgbuf, sizeof mesgbuf, fmt, ap);
		va_end(ap);
		UpdMesg++;
	}
	rbell();
	(void) longjmp(mainjmp, COMPLAIN);
}

/* VARARGS1 */

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
		(void) longjmp(mainjmp, COMPLAIN);
}

int	RecDepth = 0;

Recur()
{
	char	bname[128];
	Mark	*m;

	sprintf(bname, "%s", curbuf->b_name);
	m = MakeMark(curline, curchar, FLOATER);

	RecDepth++;
	UpdModLine++;
	DoKeys(1);	/* 1 means not first time */
	UpdModLine++;
	RecDepth--;
	SetBuf(do_select(curwind, bname));
	if (!exp_p)
		ToMark(m);
	DelMark(m);
}

jmp_buf	mainjmp;
int	iniargc;	/* main sets these for DoKeys() */
char	**iniargv;

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
				if (Upper(*ask("No",
"Some MACROS haven't been saved; leave anyway? ")) != 'Y')
					break;
			}
			if (ModBufs(0)) {
				rbell();
				if (Upper(*ask("No",
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
		gc_openfiles();	/* close any files we left open */
		errormsg++;
		fix_macros();
		Asking = 0;
		curwind->w_bufp = curbuf;
		redisplay();
		break;
	}

	this_cmd = last_cmd = 0;

	for (;;) {
		if (this_cmd != ARG_CMD) {
			exp = 1;
			exp_p = 0;
			last_cmd = this_cmd;
			init_strokes();
		}
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
		args++;
	}
	return 0;
}

int	UpdFreq = 30,
	inIOread = 0;

updmode()
{
	UpdModLine++;
	if (inIOread)
		redisplay();
#ifndef JOB_CONTROL
	(void) signal(SIGALRM, updmode);
#endif
	(void) alarm((unsigned) UpdFreq);
}

#ifdef TIOCGWINSZ
#ifdef SIGWINCH
extern win_reshape();
#endif
#endif

#ifdef TIOCGWINSZ
#ifdef SIGWINCH
win_reshape()
{
	register int diff;

	(void) signal(SIGWINCH, SIG_IGN);

	/*
	 * Save old number of lines.
	 */
	diff = LI;

	/*
	 * Get new line/col info.
	 */
	ttsize();

	/*
	 * LI has changed, and now holds the
	 * new value.  See how much the size
	 * changed.
	 */
	diff = LI - diff;

	/*
	 * Change the size of the current window
	 * only.  If they shrank by more than
	 * the window size, tough.
	 */
	if ((curwind->w_height + diff) < 2)
		curwind->w_height = 2;
	else
		curwind->w_height += diff;

	make_scr();
	redisplay();

	(void) signal(SIGWINCH, win_reshape);
}
#endif
#endif

main(argc, argv)
char	*argv[];
{
	char	ttbuf[512],
#ifndef VMUNIX
		s_iobuff[LBSIZE],
		s_genbuf[LBSIZE],
		s_linebuf[LBSIZE],
#endif
		*cp;


#ifndef VMUNIX
	/* The way I look at it, there ain't no way I is gonna run
	   out of stack space UNLESS I have some kind of infinite
	   recursive bug.  So why use up some valuable memory, when
	   there is plenty of space on the stack?  (This only matters
	   on wimpy pdp11's, of course.) */

	iobuff = s_iobuff;
	genbuf = s_genbuf;
	linebuf = s_linebuf;
#endif

	errormsg = 0;

	iniargc = argc;
	iniargv = argv;

	if (setjmp(mainjmp)) {
		printf("\rAck! I can't deal with error \"%s\" now.\n\r", mesgbuf);
		finish(0);
	}

	if (scanvec(argv, "-r"))
		dorecover();

	getTERM();	/* Get terminal. */
	if (getenv("METAKEY"))
		MetaKey = 1;
	ttsize();
	InitCM();

	tmpinit();	/* Init temp file. */

	if (cp = getenv("SHELL"))
		strcpy(Shell, cp);

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
	(void) joverc(JOVERC);
	if (!scanvec(argv, "-j")) {
		char	tmpbuf[100];

		sprintf(tmpbuf, "%s/.joverc", HomeDir);
		(void) joverc(tmpbuf);
	}
#ifdef SYSV
	sprintf(MailBox, "/usr/mail/%s", getenv("LOGNAME"));
#else
	sprintf(Mailbox, "/usr/spool/mail/%s", getenv("USER"));
#endif SYSV
	(void) time(&time0);
	ttinit();	/* initialize terminal (after ~/.joverc) */
	settout(ttbuf);	/* not until we know baudrate */
	ResetTerm();

	(void) signal(SIGHUP, finish);
	(void) signal(SIGINT, finish);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGBUS, finish);
	(void) signal(SIGSEGV, finish);
	(void) signal(SIGPIPE, finish);
	(void) signal(SIGTERM, SIG_IGN);
#ifdef TIOCGWINSZ
#ifdef SIGWINCH
	(void) signal(SIGWINCH, win_reshape);
#endif
#endif

	/* set things up to update the modeline every UpdFreq seconds */
	(void) signal(SIGALRM, updmode);
	(void) alarm((unsigned) UpdFreq);

	cl_scr(1);
	flusho();
	RedrawDisplay();	/* start the redisplay process. */
	DoKeys(0);
	finish(0);
}
