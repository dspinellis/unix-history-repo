/* vmh.c - visual front-end to mh */
#ifndef	lint
static char ident[] = "@(#)$Id: vmh.c,v 1.20 1993/08/25 17:29:44 jromine Exp $";
#endif	/* lint */
#if defined(SYS5) && !defined(TERMINFO)
/*
 * Define TERMINFO if you have it.
 * You get it automatically if you're running SYS5, and you don't get
 * it if you're not.  (If you're not SYS5, you probably have termcap.)
 * We distinguish TERMINFO from SYS5 because in this file SYS5 really
 * means "AT&T line discipline" (termio, not sgttyb), whereas terminfo
 * is quite a separate issue.
 */
#define	TERMINFO	1
#endif

/* TODO:
	Pass signals to client during execution

	Figure out a way for the user to say how big the Scan/Display
	windows should be.

	If curses ever gets fixed, then XYZ code can be removed
 */

#include <curses.h>
#ifdef	ncr
#define	_SYS_REG_H		/* NCR redefines "ERR" in <sys/reg.h> */
#endif
#undef	OK			/* tricky */
#ifdef	TERMINFO
#include <term.h>	/* variables describing terminal capabilities */
#endif	/* TERMINFO */
#include "../h/mh.h"
#include "../h/vmhsbr.h"
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
#ifndef	sigmask
#define	sigmask(s)	(1 << ((s) - 1))
#endif	/* not sigmask */
#ifdef	ridge
#undef	SIGTSTP
#endif	/* ridge */
#ifndef	BSD42
struct iovec {
    char   *iov_base;
    int     iov_len;
};
#else	/* BSD42 */
#include <sys/types.h>
#include <sys/uio.h>
#endif	/* BSD42 */
#ifdef LOCALE
#include	<locale.h>
#endif
#ifdef	hpux
#include <termio.h>
#define	TCGETATTR		/* tcgetattr() */
#endif
#ifdef	BSD44
#define	USE_OLD_TTY
#define	_maxx	maxx		/* curses.h */
#define	_maxy	maxy
#define	_curx	curx		/* curses.h */
#define	_cury	cury
void     __cputchar __P((int));
#undef	_putchar
#define	_putchar	__cputchar
#include <sys/ioctl.h>		/* sgttyb */
#endif

#define	ALARM	((unsigned int) 10)
#define	PAUSE	((unsigned int) 2)

#ifndef	abs
#define	abs(a)		((a) > 0 ? (a) : -(a))
#endif
#define	SMALLMOVE	1
#define	LARGEMOVE	10


#define	XYZ			/* XXX */

/*  */

static struct swit switches[] = {
#define	PRMPTSW	0
    "prompt string", 6,

#define	PROGSW	1
    "vmhproc program", 7,
#define	NPROGSW	2
    "novmhproc", 9,

#define	HELPSW	3
    "help", 4,

    NULL, 0
};

/*  */
					/* PEERS */
static int  PEERpid = NOTOK;

static  jmp_buf PEERctx;


					/* WINDOWS */
static char *myprompt = "(%s) ";

static  WINDOW *Scan;
static  WINDOW *Status;
static  WINDOW *Display;
static  WINDOW *Command;

#define	NWIN	3
static	int numwins;
WINDOW *windows[NWIN + 1];


					/* LINES */

struct line {
    int     l_no;
    char   *l_buf;
    struct line *l_prev;
    struct line *l_next;
};

static struct line *lhead = NULL;
static struct line *ltop = NULL;
static struct line *ltail = NULL;

static int did_less = 0;
static int smallmove = SMALLMOVE;
static int largemove = LARGEMOVE;


					/* TTYS */

static int  tty_ready = NOTOK;

static int  intrc;
#ifndef	SYS5
#define	ERASE	sg.sg_erase
#define	KILL	sg.sg_kill
static struct sgttyb    sg;

#define	EOFC	tc.t_eofc
#define	INTR	tc.t_intrc
static struct tchars    tc;
#else	/* SYS5 */
#define	ERASE	sg.c_cc[VERASE]
#define	KILL	sg.c_cc[VKILL]
#define	EOFC	sg.c_cc[VEOF]
#define	INTR	sg.c_cc[VINTR]
static struct termio    sg;
#endif	/* SYS5 */

#ifndef	TIOCGLTC
#define	WERASC	('W' & 037)
#else	/* TIOCGLTC */
#ifndef SVR4
#define	WERASC	ltc.t_werasc
static struct ltchars ltc;
#else	/* SVR4 */
#define WERASC	sg.c_cc[VWERASE]
#undef TIOCGLTC		/* the define exists, but struct ltchars doesn't */
#endif
#endif	/* TIOCGLTC */


#if !defined(SYS5) && !defined(BSD44)
int	_putchar ();
#endif	/* not SYS5 */
#ifdef	SIGTSTP
char   *tgoto ();
#endif	/* SIGTSTP */


					/* SIGNALS */
static TYPESIG     ALRMser (), PIPEser (), SIGser ();
#ifdef	SIGTSTP
static TYPESIG	TSTPser ();
#endif	/* SIGTSTP */


					/* MISCELLANY */
extern int  errno;
#ifndef	BSD44
extern int  sys_nerr;
extern char *sys_errlist[];
#endif

static void	adorn ();

static	vmh(), lreset(), linsert(), ladvance(), lretreat(), lgo();
static	TTYon(), TTYoff(), foreground();
static int	PEERinit(), pINI(), pLOOP(), pTTY(), pWIN(), WINinit();
static int	WINgetstr(), WINless(), WINputc(), TTYinit(), pWINaux();
/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
    int     vecp = 1,
	    nprog = 0;
    char   *cp,
            buffer[BUFSIZ],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *vec[MAXARGS];

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

/*  */

    while (cp = *argp++)
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    vec[vecp++] = --cp;
		    continue;
		case HELPSW: 
		    (void) sprintf (buffer, "%s [switches for vmhproc]",
			    invo_name);
		    help (buffer, switches);
		    done (1);

		case PRMPTSW:
		    if (!(myprompt = *argp++) || *myprompt == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case PROGSW: 
		    if (!(vmhproc = *argp++) || *vmhproc == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case NPROGSW:
		    nprog++;
		    continue;
	    }
	else
	    vec[vecp++] = cp;

/*  */

    if (TTYinit (nprog) == NOTOK || WINinit (nprog) == NOTOK) {
	vec[vecp] = NULL;

	vec[0] = r1bindex (vmhproc, '/');
	execvp (vmhproc, vec);
	adios (vmhproc, "unable to exec");
    }
    TTYoff ();
    (void) PEERinit (vecp, vec);
    TTYon ();

    vmh ();

    done (0);
}

/*  */

static  vmh () {
    char    buffer[BUFSIZ];

    for (;;) {
	(void) pLOOP (RC_QRY, NULLCP);

	wmove (Command, 0, 0);
	wprintw (Command, myprompt, invo_name);
	wclrtoeol (Command);
	wrefresh (Command);

	switch (WINgetstr (Command, buffer)) {
	    case NOTOK: 
		break;

	    case OK:
		done (0);	/* NOTREACHED */

	    default: 
		if (*buffer)
		    (void) pLOOP (RC_CMD, buffer);
		break;
	}
    }
}

/*    PEERS */

static int  PEERinit (vecp, vec)
int	vecp;
char   *vec[];
{
    int	    pfd0[2],
            pfd1[2];
    char    buf1[BUFSIZ],
            buf2[BUFSIZ];

    if (pipe (pfd0) == NOTOK || pipe (pfd1) == NOTOK)
	adios ("pipe", "unable to");
#ifdef	hpux
    switch (PEERpid = fork ()) {
    /*
     * Calling vfork() and then another routine [like close()] before
     * an exec() messes up the stack frame, causing crib death.
     * Use fork() instead.
     */
#else	/* not hpux */
    switch (PEERpid = vfork ()) {
#endif	/* not hpux */
	case NOTOK: 
	    adios ("vfork", "unable to");/* NOTREACHED */

	case OK: 
	    (void) close (pfd0[0]);
	    (void) close (pfd1[1]);

	    vec[vecp++] = "-vmhread";
	    (void) sprintf (buf1, "%d", pfd1[0]);
	    vec[vecp++] = buf1;
	    vec[vecp++] = "-vmhwrite";
	    (void) sprintf (buf2, "%d", pfd0[1]);
	    vec[vecp++] = buf2;
	    vec[vecp] = NULL;

	    (void) signal (SIGINT, SIG_DFL);
	    (void) signal (SIGQUIT, SIG_DFL);

	    vec[0] = r1bindex (vmhproc, '/');
	    execvp (vmhproc, vec);
	    perror (vmhproc);
	    _exit (-1);		/* NOTREACHED */

	default: 
	    (void) close (pfd0[1]);
	    (void) close (pfd1[0]);

	    (void) rcinit (pfd0[0], pfd1[1]);
	    return pINI ();
    }
}

/*  */

static int  pINI () {
    register char  *bp;
    char    buffer[BUFSIZ];
    struct record   rcs;
    register struct record *rc = &rcs;
    register    WINDOW **w;

    initrc (rc);

    bp = buffer;
    (void) sprintf (bp, "%d %d", RC_VRSN, numwins);
    bp += strlen (bp);
    for (w = windows; *w; w++) {
	(void) sprintf (bp, " %d", (*w) -> _maxy);
	bp += strlen (bp);
    }

    switch (str2rc (RC_INI, buffer, rc)) {
	case RC_ACK: 
	    return OK;

	case RC_ERR: 
	    if (rc -> rc_len)
		adios (NULLCP, "%s", rc -> rc_data);
	    else
		adios (NULLCP, "pINI peer error");

	case RC_XXX: 
	    adios (NULLCP, "%s", rc -> rc_data);

	default:
	    adios (NULLCP, "pINI protocol screw-up");
    }
/* NOTREACHED */
}

/*  */

static int  pLOOP (code, str)
char	code,
       *str;
{
    int	    i;
    struct record   rcs;
    register struct record *rc = &rcs;

    initrc (rc);

    (void) str2peer (code, str);
    for (;;)
	switch (peer2rc (rc)) {
	    case RC_TTY:
		if (pTTY (rc) == NOTOK)
		    return NOTOK;
		break;

	    case RC_WIN:
		if (sscanf (rc -> rc_data, "%d", &i) != 1
			|| i <= 0
			|| i > numwins) {
		    (void) fmt2peer (RC_ERR, "no such window \"%s\"",
				rc -> rc_data);
		    return NOTOK;
		}
		if (pWIN (windows[i - 1]) == NOTOK)
		    return NOTOK;
		break;

	    case RC_EOF:
		return OK;

	    case RC_ERR:
		if (rc -> rc_len)
		    adorn (NULLCP, "%s", rc -> rc_data);
		else
		    adorn (NULLCP, "pLOOP(%s) peer error",
			    code == RC_QRY ? "QRY" : "CMD");
		return NOTOK;

	    case RC_FIN:
		if (rc -> rc_len)
		    adorn (NULLCP, "%s", rc -> rc_data);
		(void) rcdone ();
		i = pidwait (PEERpid, OK);
		PEERpid = NOTOK;
		done (i);

	    case RC_XXX: 
		adios (NULLCP, "%s", rc -> rc_data);

	    default:
		adios (NULLCP, "pLOOP(%s) protocol screw-up",
			code == RC_QRY ? "QRY" : "CMD");
	}
}

/*  */

static int  pTTY (r)
register struct record *r;
{
    TYPESIG (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();
    struct record   rcs;
    register struct record *rc = &rcs;

    initrc (rc);

    TTYoff ();

    hstat = signal (SIGHUP, SIG_IGN);
    istat = signal (SIGINT, SIG_IGN);
    qstat = signal (SIGQUIT, SIG_IGN);
    tstat = signal (SIGTERM, SIG_IGN);

    (void) rc2rc (RC_ACK, 0, NULLCP, rc);

    (void) signal (SIGHUP, hstat);
    (void) signal (SIGINT, istat);
    (void) signal (SIGQUIT, qstat);
    (void) signal (SIGTERM, tstat);

    TTYon ();

    if (r -> rc_len && strcmp (r -> rc_data, "FAST") == 0)
	goto no_refresh;

#ifdef	SIGTSTP
    (void) signal (SIGTSTP, SIG_IGN);
#endif	/* SIGTSTP */
#ifndef	TERMINFO
    if (SO)
	tputs (SO, 0, _putchar);
#else	/* TERMINFO */
    putp(enter_standout_mode);
#endif	/* TERMINFO */
    fprintf (stdout, "Type any key to continue... ");
    (void) fflush (stdout);
#ifndef	TERMINFO
    if (SE)
	tputs (SE, 0, _putchar);
#else	/* TERMINFO */
    putp(exit_standout_mode);
#endif	/* TERMINFO */
    (void) getc (stdin);
#ifdef	SIGTSTP
    (void) signal (SIGTSTP, TSTPser);
#endif	/* SIGTSTP */

    wrefresh (curscr);

no_refresh: ;
    switch (rc -> rc_type) {
	case RC_EOF: 
	    (void) rc2peer (RC_ACK, 0, NULLCP);
	    return OK;

	case RC_ERR: 
	    if (rc -> rc_len)
		adorn (NULLCP, "%s", rc -> rc_data);
	    else
		adorn (NULLCP, "pTTY peer error");
	    return NOTOK;

	case RC_XXX: 
	    adios (NULLCP, "%s", rc -> rc_data);

	default:
	    adios (NULLCP, "pTTY protocol screw-up");
    }
/* NOTREACHED */
}

/*  */

static int  pWIN (w)
register WINDOW *w;
{
    int     i;

    did_less = 0;
    if ((i = pWINaux (w)) == OK && did_less)
	(void) WINless (w, 1);

    lreset ();

    return i;
}

/*  */

static int  pWINaux (w)
register WINDOW *w;
{
    register int    n;
    int	    eol;
    register char   c,
                   *bp;
    struct record   rcs;
    register struct record *rc = &rcs;

    initrc (rc);

    werase (w);
    wmove (w, 0, 0);
#ifdef	XYZ
    if (w == Status)
	wstandout (w);
#endif	/* XYZ */

    for (eol = 0;;)
	switch (rc2rc (RC_ACK, 0, NULLCP, rc)) {
	    case RC_DATA: 
		if (eol && WINputc (w, '\n') == ERR && WINless (w, 0))
		    goto flush;
		for (bp = rc -> rc_data, n = rc -> rc_len; n-- > 0; ) {
		    if ((c = *bp++) == '\n')
		        linsert (w);
		    if (WINputc (w, c) == ERR)
			if (n == 0 && c == '\n')
			    eol++;
			else
			    if (WINless (w, 0)) {
flush: ;
				(void) fmt2peer (RC_ERR, "flush window");
#ifdef	XYZ			/* should NEVER happen... */
				if (w == Status)
				    wstandend (w);
#endif	/* XYZ */
				wrefresh (w);
				return NOTOK;
			    }
		}
		break;

	    case RC_EOF: 
		(void) rc2peer (RC_ACK, 0, NULLCP);
#ifdef	XYZ
		if (w == Status)
		    wstandend (w);
#endif	/* XYZ */
		wrefresh (w);
		return OK;

	    case RC_ERR: 
		if (rc -> rc_len)
		    adorn (NULLCP, "%s", rc -> rc_data);
		else
		    adorn (NULLCP, "pWIN peer error");
		return NOTOK;

	    case RC_XXX: 
		adios (NULLCP, "%s", rc -> rc_data);

	    default:
		adios (NULLCP, "pWIN protocol screw-up");
	}
/* NOTREACHED */
}

/*  */

static int  pFIN () {
    int     status;

    if (PEERpid <= OK)
	return OK;

    (void) rc2peer (RC_FIN, 0, NULLCP);
    (void) rcdone ();

    switch (setjmp (PEERctx)) {
	case OK: 
	    (void) signal (SIGALRM, ALRMser);
	    (void) alarm (ALARM);

	    status = pidwait (PEERpid, OK);

	    (void) alarm (0);
	    break;

	default: 
	    (void) kill (PEERpid, SIGKILL);
	    status = NOTOK;
	    break;
    }
    PEERpid = NOTOK;

    return status;
}

/*    WINDOWS */

static int  WINinit (nprog) {
    register int    nlines,	/* not "lines" because terminfo uses that */
                    top,
                    bottom;

    foreground ();
    if (initscr () == (WINDOW *) ERR)
	if (nprog)
	    return NOTOK;
	else
	    adios (NULLCP, "could not initialize terminal");
#ifdef	SIGTSTP
    (void) signal (SIGTSTP, SIG_DFL);
#endif	/* SIGTSTP */
    sideground ();

#ifndef	TERMINFO
    if (CM == NULL)
#else	/* TERMINFO */
    if (cursor_address == NULL)	/* assume mtr wanted "cm", not "CM" */
#endif	/* TERMINFO */
	if (nprog)
	    return NOTOK;
	else
	    adios (NULLCP,
		    "sorry, your terminal isn't powerful enough to run %s",
		    invo_name);

#ifndef	TERMINFO
    if (tgetflag ("xt") || tgetnum ("sg") > 0)
	SO = SE = US = UE = NULL;
#else	/* TERMINFO */
/*
 * If termcap mapped directly to terminfo, we'd use the following:
 *  if (teleray_glitch || magic_cookie_glitch > 0)
 *	enter_standout_mode = exit_standout_mode =
 *	enter_underline_mode = exit_underline_mode = NULL;
 * But terminfo does the right thing so we don't have to resort to that.
 */
#endif	/* TERMINFO */

    if ((nlines = LINES - 1) < 11)
	adios (NULLCP, "screen too small");
    if ((top = nlines / 3 + 1) > LINES / 4 + 2)
	top--;
    bottom = nlines - top - 2;

    numwins = 0;
    Scan = windows[numwins++] = newwin (top, COLS, 0, 0);
    Status = windows[numwins++] = newwin (1, COLS, top, 0);
#ifndef	XYZ
    wstandout (Status);
#endif	/* XYZ */
    Display = windows[numwins++] = newwin (bottom, COLS, top + 1, 0);
    Command = newwin (1, COLS - 1, top + 1 + bottom, 0);
    windows[numwins] = NULL;

    largemove = Display -> _maxy / 2 + 2;
    return OK;
}

/*  */

static int WINgetstr (w, buffer)
register WINDOW *w;
char   *buffer;
{
    register int    c;
    register char  *bp;

    bp = buffer;
    *bp = 0;

    for (;;) {
	switch (c = toascii (wgetch (w))) {
	    case ERR: 
		adios (NULLCP, "wgetch lost");

	    case '\f':
		wrefresh (curscr);
		break;

	    case '\r': 
	    case '\n': 
		*bp = 0;
		if (bp > buffer) {
		    leaveok (curscr, FALSE);
		    wmove (w, 0, w -> _curx - (bp - buffer));
		    wrefresh (w);
		    leaveok (curscr, TRUE);
		}
		return DONE;

	    default: 
		if (c == intrc) {
		    wprintw (w, " ");
		    wstandout (w);
		    wprintw (w, "Interrupt");
		    wstandend (w);
		    wrefresh (w);
		    *buffer = 0;
		    return NOTOK;
		}
		if (c == EOFC) {
		    if (bp <= buffer)
			return OK;
		    break;
		}
		if (c == ERASE) {
		    if (bp <= buffer)
			continue;
		    bp--, w -> _curx--;
		    wclrtoeol (w);
		    break;
		}
		if (c == KILL) {
		    if (bp <= buffer)
			continue;
		    w -> _curx -= bp - buffer;
		    bp = buffer;
		    wclrtoeol (w);
		    break;
		}
		if (c == WERASC) {
		    if (bp <= buffer)
			continue;
		    do {
			bp--, w -> _curx--;
		    } while (isspace (*bp) && bp > buffer);

		    if (bp > buffer) {
			do {
			    bp--, w -> _curx--;
			} while (!isspace (*bp) && bp > buffer);
			if (isspace (*bp))
			    bp++, w -> _curx++;
		    }
		    wclrtoeol (w);
		    break;
		}
		
		if (c >= ' ' && c < '\177')
		    (void) waddch (w, *bp++ = c);
		break;
	}

	wrefresh (w);
    }
}

/*  */

static int  WINwritev (w, iov, n)
register WINDOW *w;
register struct iovec   *iov;
register int     n;
{
    register int    i;

    werase (w);
    wmove (w, 0, 0);
    for (i = 0; i < n; i++, iov++)
	wprintw (w, "%*.*s", iov -> iov_len, iov -> iov_len, iov -> iov_base);
    wrefresh (w);

    sleep (PAUSE);

    return OK;
}

/*  */

static struct {
    char   *h_msg;
    int    *h_val;
}               hlpmsg[] = {
                    "		forward		backwards", NULL,
                    "		-------		---------", NULL,
                    "next screen	SPACE", NULL,
                    "next %d line%s	RETURN		y", &smallmove,
                    "next %d line%s	EOT		u", &largemove,
                    "go		g		G", NULL,
                    "", NULL,
                    "refresh		CTRL-L", NULL,
                    "quit		q", NULL,

                    NULL, NULL
};

/*  */

static int  WINless (w, fin)
register WINDOW *w;
int	fin;
{
    register int    c,
                    i,
                    n;
    int     nfresh,
#ifdef	notdef
	    nlatch,
#endif	/* notdef */
            nwait;
    char   *cp;
    register struct line   *lbottom;

    did_less++;

    cp = NULL;
#ifdef	notdef
    if (fin)
	ltop = NULL;
#endif	/* notdef */
    lbottom = NULL;
    nfresh = 1;
    nwait = 0;
    wrefresh (w);

    for (;;) {
	if (nfresh || nwait) {
	    nfresh = 0;
#ifdef	notdef
	    nlatch = 1;

once_only: ;
#endif	/* notdef */
	    werase (w);
	    wmove (w, 0, 0);

	    if (ltop == NULL)
		if (fin) {
		    (void) lgo (ltail -> l_no - w -> _maxy + 1);
		    if (ltop == NULL)
			ltop = lhead;
		}
		else
		    ltop = lbottom && lbottom -> l_prev ? lbottom -> l_prev
			    : lbottom;

	    for (lbottom = ltop; lbottom; lbottom = lbottom -> l_next)
		if (waddstr (w, lbottom -> l_buf) == ERR
			|| waddch (w, '\n') == ERR)
		    break;
	    if (lbottom == NULL)
		if (fin) {
#ifdef	notdef
		    if (nlatch && (ltail -> l_no >= w -> _maxy)) {
			(void) lgo (ltail -> l_no - w -> _maxy + 1);
			nlatch = 0;
			goto once_only;
		    }
#endif	/* notdef */
		    lbottom = ltail;
		    while (waddstr (w, "~\n") != ERR)
			continue;
		}
		else {
		    wrefresh (w);
		    return 0;
		}

	    if (!nwait)
		wrefresh (w);
	}

	wmove (Command, 0, 0);
	if (cp) {
	    wstandout (Command);
	    wprintw (Command, "%s", cp);
	    wstandend (Command);
	    cp = NULL;
	}
	else
	    wprintw (Command, fin ? "top:%d bot:%d end:%d" : "top:%d bot:%d",
		    ltop -> l_no, lbottom -> l_no, ltail -> l_no);
	wprintw (Command, ">> ");
	wclrtoeol (Command);
	wrefresh (Command);

	c = toascii (wgetch (Command));

	werase (Command);
	wrefresh (Command);

	if (nwait) {
	    nwait = 0;
	    wrefresh (w);
	}

	n = 0;
again: 	;
	switch (c) {
	    case ' ': 
		ltop = lbottom -> l_next;
		nfresh++;
		break;

	    case '\r': 
	    case '\n': 
	    case 'e': 
	    case 'j': 
		if (n)
		    smallmove = n;
		if (ladvance (smallmove))
		    nfresh++;
		break;

	    case 'y': 
	    case 'k': 
		if (n)
		    smallmove = n;
		if (lretreat (smallmove))
		    nfresh++;
		break;

	    case 'd': 
	eof: 	;
		if (n)
		    largemove = n;
		if (ladvance (largemove))
		    nfresh++;
		break;

	    case 'u': 
		if (n)
		    largemove = n;
		if (lretreat (largemove))
		    nfresh++;
		break;

	    case 'g': 
		if (lgo (n ? n : 1))
		    nfresh++;
		break;

	    case 'G': 
		if (lgo (n ? n : ltail -> l_no - w -> _maxy + 1))
		    nfresh++;
		break;

	    case '\f': 
	    case 'r': 
		wrefresh (curscr);
		break;

	    case 'h': 
	    case '?': 
		werase (w);
		wmove (w, 0, 0);
		for (i = 0; hlpmsg[i].h_msg; i++) {
		    if (hlpmsg[i].h_val)
			wprintw (w, hlpmsg[i].h_msg, *hlpmsg[i].h_val,
				*hlpmsg[i].h_val != 1 ? "s" : "");
		    else
			(void) waddstr (w, hlpmsg[i].h_msg);
		    (void) waddch (w, '\n');
		}
		wrefresh (w);
		nwait++;
		break;

	    case 'q': 
		return 1;

	    default: 
		if (c == EOFC)
		    goto eof;

		if (isdigit (c)) {
		    wmove (Command, 0, 0);
		    i = 0;
		    while (isdigit (c)) {
			wprintw (Command, "%c", c);
			wrefresh (Command);
			i = i * 10 + c - '0';
			c = toascii (wgetch (Command));
		    }
		    werase (Command);
		    wrefresh (Command);

		    if (i > 0) {
			n = i;
			goto again;
		    }
		    cp = "bad number";
		}
		else
		    cp = "not understood";
		break;
	}
    }
}

/*  */

static int  WINputc (w, c)
register WINDOW *w;
register char c;
{
    register int    x,
                    y;

    switch (c) {
	default: 
	    if (!isascii (c)) {
		if (WINputc (w, 'M') == ERR || WINputc (w, '-') == ERR)
		    return ERR;
		c = toascii (c);
	    }
	    else
		if (c < ' ' || c == '\177') {
		    if (WINputc (w, '^') == ERR)
			return ERR;
		    c ^= 0100;
		}
	    break;

	case '\t': 
	case '\n': 
	    break;
    }

    if (w != Scan)
	return waddch (w, c);

    if ((x = w -> _curx) < 0 || x >= w -> _maxx
	    || (y = w -> _cury) < 0 || y >= w -> _maxy)
	return DONE;

    switch (c) {
	case '\t': 
	    for (x = 8 - (x & 0x07); x > 0; x--)
		if (WINputc (w, ' ') == ERR)
		    return ERR;
	    break;

	case '\n': 
	    if (++y < w -> _maxy) 
		(void) waddch (w, c);
	    else
		wclrtoeol (w);
	    break;

	default: 
	    if (++x < w -> _maxx) 
		(void) waddch (w, c);
	    break;
    }

    return DONE;
}

/*    LINES */

static  lreset () {
    register struct line   *lp,
                           *mp;

    for (lp = lhead; lp; lp = mp) {
	mp = lp -> l_next;
	free (lp -> l_buf);
	free ((char *) lp);
    }
    lhead = ltop = ltail = NULL;
}


static	linsert (w)
WINDOW *w;
{
    register char  *cp;
    register struct line   *lp;

    if ((lp = (struct line  *) calloc ((unsigned) 1, sizeof *lp)) == NULL)
	adios (NULLCP, "unable to allocate line storage");

    lp -> l_no = (ltail ? ltail -> l_no : 0) + 1;
#ifndef	BSD44
    lp -> l_buf = getcpy (w -> _y[w -> _cury]);
#else
    lp -> l_buf = getcpy (w -> lines[w -> _cury]->line);
#endif
    for (cp = lp -> l_buf + strlen (lp -> l_buf) - 1; cp >= lp -> l_buf; cp--)
	if (isspace (*cp))
	    *cp = 0;
	else
	    break;

    if (lhead == NULL)
	lhead = lp;
    if (ltop == NULL)
	ltop = lp;
    if (ltail)
	ltail -> l_next = lp;
    lp -> l_prev = ltail;
    ltail = lp;
}

/*  */

static int  ladvance (n)
int	n;
{
    register int    i;
    register struct line   *lp;

    for (i = 0, lp = ltop; i < n && lp; i++, lp = lp -> l_next)
	continue;

    if (ltop == lp)
	return 0;

    ltop = lp;
    return 1;
}


static int  lretreat (n)
int	n;
{
    register int    i;
    register struct line   *lp;

    for (i = 0, lp = ltop; i < n && lp; i++, lp = lp -> l_prev)
	if (!lp -> l_prev)
	    break;

    if (ltop == lp)
	return 0;

    ltop = lp;
    return 1;
}

/*  */

static int  lgo (n)
int	n;
{
    register int    i,
                    j;
    register struct line   *lp;

    if ((i = n - (lp = lhead) -> l_no)
	    > (j = abs (n - (ltop ? ltop : ltail) -> l_no)))
	i = j, lp = ltop ? ltop : ltail;
    if (i > (j = abs (ltail -> l_no - n)))
	i = j, lp = ltail;

    if (n >= lp -> l_no) {
	for (; lp; lp = lp -> l_next)
	    if (lp -> l_no == n)
		break;
    }
    else {
	for (; lp; lp = lp -> l_prev)
	    if (lp -> l_no == n)
		break;
	if (!lp)
	    lp = lhead;
    }

    if (ltop == lp)
	return 0;

    ltop = lp;
    return 1;
}

/*    TTYS */

static int  TTYinit (nprog) {
    if (!isatty (fileno (stdin)) || !isatty (fileno (stdout)))
	if (nprog)
	    return NOTOK;
	else
	    adios (NULLCP, "not a tty");

    foreground ();
#ifndef	SYS5
    if (ioctl (fileno (stdin), TIOCGETP, (char *) &sg) == NOTOK)
	adios ("failed", "ioctl TIOCGETP");
    if (ioctl (fileno (stdin), TIOCGETC, (char *) &tc) == NOTOK)
	adios ("failed", "ioctl TIOCGETC");
#else
#ifdef	TCGETATTR
    if( tcgetattr( fileno(stdin), &sg) == NOTOK)
	adios( "failed", "tcgetattr");
#else	/* SYS5 */
    if (ioctl (fileno (stdin), TCGETA, &sg) == NOTOK)
	adios ("failed", "ioctl TCGETA");
#endif
#endif
#ifdef	TIOCGLTC
    if (ioctl (fileno (stdin), TIOCGLTC, (char *) &ltc) == NOTOK)
	adios ("failed", "ioctl TIOCGLTC");
#endif	/* TIOCGLTC */
    intrc = INTR;
    sideground ();

    tty_ready = OK;

    (void) signal (SIGPIPE, PIPEser);

    return OK;
}

/*  */

static	TTYon () {
    if (tty_ready == DONE)
	return;

    INTR = NOTOK;
#ifndef	SYS5
    (void) ioctl (fileno (stdin), TIOCSETC, (char *) &tc);
#else	/* SYS5 */
    (void) ioctl (fileno (stdin), TCSETA, &sg);
#endif	/* SYS5 */

    (void) crmode ();
    (void) noecho ();
    (void) nonl ();
    scrollok (curscr, FALSE);

    discard (stdin);

    tty_ready = DONE;

    (void) signal (SIGHUP, SIGser);
    (void) signal (SIGINT, SIGser);
    (void) signal (SIGQUIT, SIGser);
#ifdef	SIGTSTP
    (void) signal (SIGTSTP, TSTPser);
#endif	/* SIGTSTP */
}

/*  */

static	TTYoff () {
    if (tty_ready == NOTOK)
	return;

    INTR = intrc;
#ifndef	SYS5
    (void) ioctl (fileno (stdin), TIOCSETC, (char *) &tc);
#else	/* SYS5 */
    (void) ioctl (fileno (stdin), TCSETA, &sg);
#endif	/* SYS5 */

    leaveok (curscr, TRUE);
    mvcur (0, COLS - 1, LINES - 1, 0);
    endwin ();
    if (tty_ready == DONE) {
#ifndef	TERMINFO
	if (CE)
	    tputs (CE, 0, _putchar);
	else
#else	/* TERMINFO */
	putp(clr_eol);
#endif	/* TERMINFO */
	    fprintf (stdout, "\r\n");
    }
    (void) fflush (stdout);

    tty_ready = NOTOK;

    (void) signal (SIGHUP, SIG_DFL);
    (void) signal (SIGINT, SIG_DFL);
    (void) signal (SIGQUIT, SIG_DFL);
#ifdef	SIGTSTP
    (void) signal (SIGTSTP, SIG_DFL);
#endif	/* SIGTSTP */
}

/*  */

static  foreground () {
#ifdef	TIOCGPGRP
    int     pgrp,
            tpgrp;
    TYPESIG     (*tstat) ();

    if ((pgrp = getpgrp (0)) == NOTOK)
	adios ("process group", "unable to determine");
    for (;;) {
	if (ioctl (fileno (stdin), TIOCGPGRP, (char *) &tpgrp) == NOTOK)
	    adios ("tty's process group", "unable to determine");
	if (pgrp == tpgrp)
	    break;

	tstat = signal (SIGTTIN, SIG_DFL);
	(void) kill (0, SIGTTIN);
	(void) signal (SIGTTIN, tstat);
    }
    
    (void) signal (SIGTTIN, SIG_IGN);
    (void) signal (SIGTTOU, SIG_IGN);
    (void) signal (SIGTSTP, SIG_IGN);
#endif	/* TIOCGPGRP */
}


sideground () {
#ifdef	TIOCGPGRP
    (void) signal (SIGTTIN, SIG_DFL);
    (void) signal (SIGTTOU, SIG_DFL);
    (void) signal (SIGTSTP, SIG_DFL);
#endif	/* TIOCGPGRP */
}

/*    SIGNALS */

/* ARGSUSED */

static TYPESIG  ALRMser (sig)
int     sig;
{
     longjmp (PEERctx, DONE);
}


#ifdef	BSD42
/* ARGSUSED */
#endif	/* BSD42 */

static TYPESIG  PIPEser (sig)
int	sig;
{
#ifndef	BSD42
    (void) signal (sig, SIG_IGN);
#endif	/* BSD42 */

    adios (NULLCP, "lost peer");
}


#ifdef	BSD42
/* ARGSUSED */
#endif	/* BSD42 */

static TYPESIG  SIGser (sig)
int     sig;
{
#ifndef	BSD42
    (void) signal (sig, SIG_IGN);
#endif	/* BSD42 */

    done (1);
}


#ifdef	SIGTSTP
static TYPESIG  TSTPser (sig)
int     sig;
{
#ifndef	TERMINFO
    tputs (tgoto (CM, 0, LINES - 1), 0, _putchar);
#else	/* TERMINFO */
    move(LINES - 1, 0);	/* to lower left corner */
    clrtoeol();		/* clear bottom line */
    wrefresh(curscr);	/* flush out everything */
#endif	/* TERMINFO */
    (void) fflush (stdout);

    TTYoff ();
#ifdef	BSD42
    (void) sigsetmask (sigblock (0) & ~sigmask (SIGTSTP));
#endif	/* BSD42 */

    (void) kill (getpid (), sig);

#ifdef	BSD42
    (void) sigblock (sigmask (SIGTSTP));
#endif	/* BSD42 */
    TTYon ();

    wrefresh (curscr);
}
#endif	/* SIGTSTP */

/*    MISCELLANY */

void	done (status)
int	status;
{
    TTYoff ();
    (void) pFIN ();

    exit (status);
}

/*  */

/* VARARGS2 */

static void  adorn (what, fmt, a, b, c, d, e, f)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
{
    char   *cp = invo_name;

    invo_name = NULL;
    advise (what, fmt, a, b, c, d, e, f);
    invo_name = cp;
}

/*  */

/* VARARGS3 */

void advertise (what, tail, fmt, a, b, c, d, e, f)
char   *what,
       *tail,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
{
    int	    eindex = errno;
    char    buffer[BUFSIZ],
            err[BUFSIZ];
    struct iovec    iob[20];
    register struct iovec  *iov = iob;

    (void) fflush (stdout);

    (void) fflush (stderr);

    if (invo_name) {
	iov -> iov_len = strlen (iov -> iov_base = invo_name);
	iov++;
	iov -> iov_len = strlen (iov -> iov_base = ": ");
	iov++;
    }
    
    (void) sprintf (buffer, fmt, a, b, c, d, e, f);
    iov -> iov_len = strlen (iov -> iov_base = buffer);
    iov++;
    if (what) {
	if (*what) {
	    iov -> iov_len = strlen (iov -> iov_base = " ");
	    iov++;
	    iov -> iov_len = strlen (iov -> iov_base = what);
	    iov++;
	    iov -> iov_len = strlen (iov -> iov_base = ": ");
	    iov++;
	}
	if (eindex > 0 && eindex < sys_nerr)
	    iov -> iov_len = strlen (iov -> iov_base = sys_errlist[eindex]);
	else {
	    (void) sprintf (err, "Error %d", eindex);
	    iov -> iov_len = strlen (iov -> iov_base = err);
	}
	iov++;
    }
    if (tail && *tail) {
	iov -> iov_len = strlen (iov -> iov_base = ", ");
	iov++;
	iov -> iov_len = strlen (iov -> iov_base = tail);
	iov++;
    }
    iov -> iov_len = strlen (iov -> iov_base = "\n");
    iov++;

    if (tty_ready == DONE)
	(void) WINwritev (Display, iob, iov - iob);
    else
	(void) writev (fileno (stderr), iob, iov - iob);
}

/*  */

#ifndef	BSD42
static int     writev (fd, iov, n)
register int     fd;
register struct iovec   *iov;
register int     n;
{
    register int    i,
                    j;

    for (i = j = 0; i < n; i++, iov++)
	if (write (fd, iov -> iov_base, iov -> iov_len) != iov -> iov_len)
	    break;
	else
	    j += iov -> iov_len;

    return j;
}
#endif	/* BSD42 */
