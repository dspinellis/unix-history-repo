/* curses.c */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This file contains the functions & variables needed for a tiny subset of
 * curses.  The principle advantage of this version of curses is its
 * extreme speed.  Disadvantages are potentially larger code, few supported
 * functions, limited compatibility with full curses, and only stdscr.
 */

#include "config.h"
#include "vi.h"

#if ANY_UNIX
# if UNIXV
#  include	<termio.h>
#  undef	TIOCWINSZ	/* we can't handle it correctly yet */
# else
#  include	<sgtty.h>
# endif
#endif

#if TOS
# include	<osbind.h>
#endif

#if OSK
# include	<sgstat.h>
#endif

#include <signal.h>

extern char	*getenv();
static void	 starttcap();

/* variables, publicly available & used in the macros */
short	ospeed;		/* speed of the tty, eg B2400 */
#if OSK
char	PC_;	/* Pad char */
char	*BC;	/* backspace character string */
#else
char	PC;		/* Pad char */
#endif
WINDOW	*stdscr;	/* pointer into kbuf[] */
WINDOW	kbuf[KBSIZ];	/* a very large output buffer */
int	LINES;		/* :li#: number of rows */
int	COLS;		/* :co#: number of columns */
int	AM;		/* :am:  boolean: auto margins? */
int	PT;		/* :pt:  boolean: physical tabs? */
char	*VB;		/* :vb=: visible bell */
char	*UP;		/* :up=: move cursor up */
char	*SO;		/* :so=: standout start */
char	*SE;		/* :se=: standout end */
char	*US = "";	/* :us=: underline start */
char	*UE = "";	/* :ue=: underline end */
char	*MD = "";	/* :md=: bold start */
char	*ME = "";	/* :me=: bold end */
char	*AS;		/* :as=: alternate (italic) start */
char	*AE;		/* :ae=: alternate (italic) end */
char	*CM;		/* :cm=: cursor movement */
char	*CE;		/* :ce=: clear to end of line */
char	*CD;		/* :cd=: clear to end of screen */
char	*AL;		/* :al=: add a line */
char	*DL;		/* :dl=: delete a line */
#if OSK
char	*SR_;		/* :sr=: scroll reverse */
#else
char	*SR;		/* :sr=: scroll reverse */
#endif
char	*KS;		/* :ks=: init string for cursor */
char	*KE;		/* :ke=: restore string for cursor */
char	*KU;		/* :ku=: key sequence sent by up arrow */
char	*KD;		/* :kd=: key sequence sent by down arrow */
char	*KL;		/* :kl=: key sequence sent by left arrow */
char	*KR;		/* :kr=: key sequence sent by right arrow */
char	*HM;		/* :HM=: key sequence sent by the <Home> key */
char	*EN;		/* :EN=: key sequence sent by the <End> key */
char	*PU;		/* :PU=: key sequence sent by the <PgUp> key */
char	*PD;		/* :PD=: key sequence sent by the <PgDn> key */
char	*IM;		/* :im=: insert mode start */
char	*IC = "";	/* :ic=: insert the following character */
char	*EI;		/* :ei=: insert mode end */
char	*DC;		/* :dc=: delete a character */
char	*TI;		/* :ti=: terminal init */	/* GB */
char	*TE;		/* :te=: terminal exit */	/* GB */
#ifndef NO_CURSORSHAPE
char	*CQ = (char *)0;/* :cQ=: normal cursor */
char	*CX = (char *)1;/* :cX=: cursor used for EX command/entry */
char	*CV = (char *)2;/* :cV=: cursor used for VI command mode */
char	*CI = (char *)3;/* :cI=: cursor used for VI input mode */
char	*CR = (char *)4;/* :cR=: cursor used for VI replace mode */
#endif
char	*aend = "";	/* end an attribute -- either UE or ME */
char	ERASEKEY;	/* backspace key taken from ioctl structure */

#if ANY_UNIX
# if UNIXV
static struct termio	oldtermio;	/* original tty mode */
static struct termio	newtermio;	/* cbreak/noecho tty mode */
# else
static struct sgttyb	oldsgttyb;	/* original tty mode */
static struct sgttyb	newsgttyb;	/* cbreak/nl/noecho tty mode */
static int		oldint;		/* ^C or DEL, the "intr" character */
#  ifdef TIOCSLTC
static int		oldswitch;	/* ^Z, the "suspend" character */
static int		oldquote;	/* ^V, the "quote next char" char */
#  endif
# endif
#endif

#if OSK
static struct sgbuf	oldsgttyb;	/* orginal tty mode */
static struct sgbuf	newsgttyb;	/* noecho tty mode */
#endif

static char	*capbuf;	/* capability string buffer */


void initscr()
{
	/* make sure TERM variable is set */
#if MSDOS
	char *val;
	if (! (val = getenv("TERM"))
	|| !strcmp(val, "pcbios"))
#else
	if (!getenv("TERM"))
#endif
	{
#if ANY_UNIX
		write(2, "Environment variable TERM must be set\n", (unsigned)38);
		exit(1);
#endif
#if OSK
		writeln(2, "Environment variable TERM must be set\n", (unsigned)38);
		exit(1);
#endif
#if MSDOS || TOS
		getsize(0);
#endif
	}
	else
	{
#if MSDOS
		*o_pcbios=0;
#endif
		/* start termcap stuff */
		starttcap();
	}

	/* create stdscr and curscr */
	stdscr = kbuf;

	/* change the terminal mode to cbreak/noecho */
#if ANY_UNIX
# if UNIXV
	ioctl(2, TCGETA, &oldtermio);
# else
	ioctl(2, TIOCGETP, &oldsgttyb);
# endif
#endif

#if OSK
	_gs_opt(0, &oldsgttyb);
#endif
	resume_curses(TRUE);
}


void endwin()
{
	/* change the terminal mode back the way it was */
	suspend_curses();
}


static int curses_active = FALSE;

void suspend_curses()
{
#if ANY_UNIX && !UNIXV
	struct tchars	tbuf;
# ifdef TIOCSLTC
	struct ltchars	ltbuf;
# endif
#endif
#ifndef NO_CURSORSHAPE
	if (has_CQ)
	{
		do_CQ();
	}
#endif
	if (has_TE)					/* GB */
	{
		do_TE();
	}
	if (has_KE)
	{
		do_KE();
	}
	refresh();

	/* change the terminal mode back the way it was */
#if ANY_UNIX
# if UNIXV
	ioctl(2, TCSETAW, &oldtermio);
# else
	ioctl(2, TIOCSETP, &oldsgttyb);

	ioctl(2, TIOCGETC, &tbuf);
	tbuf.t_intrc = oldint;
	ioctl(2, TIOCSETC, &tbuf);

#  ifdef TIOCSLTC
	ioctl(2, TIOCGLTC, &ltbuf);
	ltbuf.t_suspc = oldswitch;
	ltbuf.t_lnextc = oldquote;
	ioctl(2, TIOCSLTC, &ltbuf);
#  endif
# endif
#endif
#if OSK
	_ss_opt(0, &oldsgttyb);
#endif
	curses_active = FALSE;
}

void resume_curses(quietly)
	int	quietly;
{
	if (!curses_active)
	{
		/* change the terminal mode to cbreak/noecho */
#if ANY_UNIX
# if UNIXV
		ospeed = (oldtermio.c_cflag & CBAUD);
		ERASEKEY = oldtermio.c_cc[VERASE];
		newtermio = oldtermio;
		newtermio.c_iflag &= (IXON|IXOFF|IXANY|ISTRIP|IGNBRK);
		newtermio.c_oflag &= ~OPOST;
		newtermio.c_lflag &= ISIG;
		newtermio.c_cc[VINTR] = ctrl('C'); /* always use ^C for interrupts */
		newtermio.c_cc[VMIN] = 1;
		newtermio.c_cc[VTIME] = 0;
#  ifdef VSWTCH
		newtermio.c_cc[VSWTCH] = 0;
#  endif
		ioctl(2, TCSETAW, &newtermio);
# else /* BSD or V7 or Coherent or Minix */
		struct tchars	tbuf;
#  ifdef TIOCSLTC
		struct ltchars	ltbuf;
#  endif

		ospeed = oldsgttyb.sg_ospeed;
		ERASEKEY = oldsgttyb.sg_erase;
		newsgttyb = oldsgttyb;
		newsgttyb.sg_flags |= CBREAK;
		newsgttyb.sg_flags &= ~(CRMOD|ECHO|XTABS);
		ioctl(2, TIOCSETP, &newsgttyb);

		ioctl(2, TIOCGETC, &tbuf);
		oldint = tbuf.t_intrc;
		tbuf.t_intrc = ctrl('C');	/* always use ^C for interrupts */
		ioctl(2, TIOCSETC, &tbuf);

#  ifdef TIOCSLTC
		ioctl(2, TIOCGLTC, &ltbuf);
		oldswitch = ltbuf.t_suspc;
		ltbuf.t_suspc = 0;		/* disable ^Z for elvis */
		oldquote = ltbuf.t_lnextc;
		ltbuf.t_lnextc = 0;		/* disable ^V for elvis */
		ioctl(2, TIOCSLTC, &ltbuf);
#  endif

# endif
#endif
#if OSK
		newsgttyb = oldsgttyb;
		newsgttyb.sg_echo = 0;
		newsgttyb.sg_eofch = 0;
		newsgttyb.sg_kbach = 0;
		newsgttyb.sg_kbich = ctrl('C');
		_ss_opt(0, &newsgttyb);
		ospeed = oldsgttyb.sg_baud;
		ERASEKEY = oldsgttyb.sg_bspch;
#endif

		if (has_TI)					/* GB */
		{
			do_TI();
		}
		if (has_KS)
		{
			do_KS();
		}

		curses_active = TRUE;
	}

	/* If we're supposed to quit quietly, then we're done */
	if (quietly)
	{
		return;
	}

	signal(SIGINT, SIG_IGN);

	move(LINES - 1, 0);
	do_SO();
	qaddstr("[Press <RETURN> to continue]");
	do_SE();
	refresh();
	ttyread(kbuf, 20); /* in RAW mode, so <20 is very likely */
	if (kbuf[0] == ':')
	{
		mode = MODE_COLON;
		addch('\n');
		refresh();
	}
	else
	{
		mode = MODE_VI;
		redraw(MARK_UNSET, FALSE);
	}	
	exwrote = FALSE;

#if TURBOC
	signal(SIGINT, (void(*)()) trapint);
#else
	signal(SIGINT, trapint);
#endif
}

static void lacking(s)
	char	*s;
{
	write(2, "This termcap entry lacks the :", (unsigned)30);
	write(2, s, (unsigned)2);
	write(2, "=: capability\n", (unsigned)14);
#if OSK
	write(2, "\l", 1);
#endif
	exit(1);
}

static void starttcap()
{
	char	*str;
	static char	cbmem[800];
#define MUSTHAVE(T,s)	if (!(T = tgetstr(s, &capbuf))) lacking(s)
#define MAYHAVE(T,s)	if (str = tgetstr(s, &capbuf)) T = str
#define PAIR(T,U,sT,sU)	T=tgetstr(sT,&capbuf);U=tgetstr(sU,&capbuf);if (!T||!U)T=U=""

	/* allocate memory for capbuf */
	capbuf = cbmem;

	/* get the termcap entry */
	switch (tgetent(kbuf, getenv("TERM")))
	{
	  case -1:
		write(2, "Can't read /etc/termcap\n", (unsigned)24);
#if OSK
		write(2, "\l", 1);
#endif
		exit(2);

	  case 0:
		write(2, "Unrecognized TERM type\n", (unsigned)23);
#if OSK
		write(2, "\l", 1);
#endif
		exit(3);
	}

	/* get strings */
	MUSTHAVE(UP, "up");
	MAYHAVE(VB, "vb");
	MUSTHAVE(CM, "cm");
	PAIR(SO, SE, "so", "se");
	PAIR(TI, TE, "ti", "te");
	if (tgetnum("ug") <= 0)
	{
		PAIR(US, UE, "us", "ue");
		PAIR(MD, ME, "md", "me");

		/* get italics, or have it default to underline */
		PAIR(AS, AE, "as", "ae");
		if (!*AS)
		{
			AS = US;
			AE = UE;
		}
	}
	MAYHAVE(AL, "al");
	MAYHAVE(DL, "dl");
	MUSTHAVE(CE, "ce");
	MAYHAVE(CD, "cd");
#if OSK
	MAYHAVE(SR_, "sr");
#else	
	MAYHAVE(SR, "sr");
#endif
	PAIR(IM, EI, "im", "ei");
	MAYHAVE(IC, "ic");
	MAYHAVE(DC, "dc");

	/* other termcap stuff */
	AM = tgetflag("am");
	PT = tgetflag("pt");
	getsize(0);

	/* Key sequences */
	PAIR(KS, KE, "ks", "ke");
	MAYHAVE(KU, "ku");		/* up */
	MAYHAVE(KD, "kd");		/* down */
	MAYHAVE(KL, "kl");		/* left */
	MAYHAVE(KR, "kr");		/* right */
	MAYHAVE(PU, "kP");		/* PgUp */
	MAYHAVE(PD, "kN");		/* PgDn */
	MAYHAVE(HM, "kh");		/* Home */
	MAYHAVE(EN, "kH");		/* End */
#ifndef CRUNCH
	if (!PU) MAYHAVE(PU, "K2");	/* "3x3 pad" names for PgUp, etc. */
	if (!PD) MAYHAVE(PD, "K5");
	if (!HM) MAYHAVE(HM, "K1");
	if (!EN) MAYHAVE(EN, "K4");

	MAYHAVE(PU, "PU");		/* old XENIX names for PgUp, etc. */
	MAYHAVE(PD, "PD");		/* (overrides others, if used.) */
	MAYHAVE(HM, "HM");
	MAYHAVE(EN, "EN");
#endif

#ifndef NO_CURSORSHAPE
	/* cursor shapes */
	CQ = tgetstr("cQ", &capbuf);
	if (has_CQ)
	{
		CX = tgetstr("cX", &capbuf);
		if (!CX) CX = CQ;
		CV = tgetstr("cV", &capbuf);
		if (!CV) CV = CQ;
		CI = tgetstr("cI", &capbuf);
		if (!CI) CI = CQ;
		CR = tgetstr("cR", &capbuf);
		if (!CR) CR = CQ;
	}
# ifndef CRUNCH
	else
	{
		PAIR(CQ, CV, "ve", "vs");
		CX = CI = CR = CQ;
	}
# endif /* !CRUNCH */
#endif /* !NO_CURSORSHAPE */

#undef MUSTHAVE
#undef MAYHAVE
#undef PAIR
}


/* This function gets the window size.  It uses the TIOCGWINSZ ioctl call if
 * your system has it, or tgetnum("li") and tgetnum("co") if it doesn't.
 * This function is called once during initialization, and thereafter it is
 * called whenever the SIGWINCH signal is sent to this process.
 */
int getsize(signo)
	int	signo;
{
	int	lines;
	int	cols;
#ifdef TIOCGWINSZ
	struct winsize size;
#endif

#ifdef SIGWINCH
	/* reset the signal vector */
	signal(SIGWINCH, getsize);
#endif

	/* get the window size, one way or another. */
	lines = cols = 0;
#ifdef TIOCGWINSZ
	if (ioctl(2, TIOCGWINSZ, &size) >= 0)
	{
		lines = size.ws_row;
		cols = size.ws_col;
	}
#endif
	if ((lines == 0 || cols == 0) && signo == 0)
	{
		LINES = CHECKBIOS(v_rows(), tgetnum("li"));
		COLS = CHECKBIOS(v_cols(), tgetnum("co"));
	}
	if (lines >= 2 && cols >= 30)
	{
		LINES = lines;
		COLS = cols;
	}

	/* Make sure we got values that we can live with */
	if (LINES < 2 || COLS < 30)
	{
		write(2, "Screen too small\n", (unsigned)17);
#if OSK
		write(2, "\l", 1);
#endif
		endwin();
		exit(2);
	}

	/* !!! copy the new values into Elvis' options */
	{
		extern char	o_columns[], o_lines[];

		*o_columns = COLS;
		*o_lines = LINES;
	}

	return 0;
}


/* This is a function version of addch() -- it is used by tputs() */
int faddch(ch)
	int	ch;
{
	addch(ch);

	return 0;
}

/* These functions are equivelent to the macros of the same names... */

void qaddstr(str)
	char	*str;
{
	REG char *s_, *d_;

#if MSDOS
	if (o_pcbios[0])
	{
		while (*str)
			qaddch(*str++);
		return;
	}
#endif
	for (s_=(str), d_=stdscr; *d_++ = *s_++; )
	{
	}
	stdscr = d_ - 1;
}

void attrset(a)
	int	a;
{
	do_aend();
	if (a == A_BOLD)
	{
		do_MD();
		aend = ME;
	}
	else if (a == A_UNDERLINE)
	{
		do_US();
		aend = UE;
	}
	else if (a == A_ALTCHARSET)
	{
		do_AS();
		aend = AE;
	}
	else
	{
		aend = "";
	}
}


void insch(ch)
	int	ch;
{
	if (has_IM)
		do_IM();
	do_IC();
	qaddch(ch);
	if (has_EI)
		do_EI();
}

#if MSDOS

static int alarmtime;

/* raw read - #defined to read (0, ...) on non-MSDOS.
 * With MSDOS, am maximum of 1 byte is read.
 * If more bytes should be read, just change the loop.
 * The following code uses the IBM-PC-System-Timer, so probably wont't work
 * on non-compatibles.
 */
/*ARGSUSED*/
ttyread(buf, len)
	char *buf;
	int len;
{
	volatile char far *biostimer;
	char oldtime;
	int nticks = 0;
	int pos = 0;

	biostimer = (char far *)0x0040006cl;
	oldtime = *biostimer;

	while (!pos && (!alarmtime || nticks<alarmtime))
	{	if (kbhit())
			if ((buf[pos++] = getch()) == 0) /* function key */
				buf[pos-1] = '#';
		if (oldtime != *biostimer)
		{	nticks++;
			oldtime = *biostimer;
		}
	}
	return pos;
}

alarm(time)
	int time;
{
	alarmtime = 2 * time;		/* ticks are 1/18 sec. */
}

sleep(seconds)
	unsigned seconds;
{
	volatile char	far *biostimer = (char far *)0x0040006cl;
	char		stop;

	stop = *biostimer + 18 * seconds;
	while (*biostimer != stop)
	{
	}
}
#endif

#if TOS

static int alarmtime;
static long timer;

static gettime()
{
	timer = *(long *)(0x4ba);
}

/*ARGSUSED*/
ttyread(buf, len)
	char *buf;
	int len;
{
	int	pos=0;
	long	l;
	long	endtime;

	Supexec(gettime);
	endtime = timer+alarmtime;

	while (!pos && (!alarmtime || timer<endtime))
	{
		if (Bconstat(2))
		{
			l = Bconin(2);
			if ((buf[pos++]=l) == '\0')
			{
				buf[pos-1]='#';
				buf[pos++]=l>>16;
			}
		}
		Supexec(gettime);
	}
	return pos;
}

alarm(time)
	int time;
{
	alarmtime = 50 * time;		/* ticks are 1/200 sec. */
}

ttywrite(buf, len)
	char *buf;
	int len;
{
	while (len--)
		Bconout(2, *buf++);
}
#endif

#if OSK
ttyread(buf, len)
	char *buf;
	int len;
{
	REG int i;
	if ((i = _gs_rdy(0)) > 0)
		return read(0, buf, i < len ? i : len);
	else
		return read(0, buf, 1);
}

alarm(time)
	int time;
{
#define TIME(secs) ((secs << 8) | 0x80000000)
	static int alrmid;

	if (time)	
		alrmid = alm_set(SIGQUIT, TIME(time));
	else	
		alm_delete(alrmid);
}
#endif /* OSK */
