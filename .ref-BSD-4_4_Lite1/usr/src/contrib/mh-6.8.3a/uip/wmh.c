/* wmh.c - window front-end to mh */
#ifndef	lint
static char ident[] = "@(#)$Id: wmh.c,v 1.5 1993/08/25 17:29:59 jromine Exp $";
#endif	lint

/* TODO:
	Pass signals to client during execution

	Figure out a way for the user to say how big the Scan/Display
	windows should be, and where all the windows should be.
 */

#include <stdio.h>
#include "../h/mh.h"
#include "../h/vmhsbr.h"
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
#ifndef	sigmask
#define	sigmask(s)	(1 << ((s) - 1))
#endif	not sigmask
#include <sys/types.h>
#include <sys/uio.h>
#include <vt.h>
#include <bitmap.h>
#include <tools.h>
#ifdef LOCALE
#include	<locale.h>
#endif


#define	ALARM	((unsigned int) 10)
#define	PAUSE	((unsigned int) 2)

#define	abs(a)	((a) > 0 ? (a) : -(a))

#define	SZ(a)	(sizeof a / sizeof a[0])

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

    NULL, NULL
};

/*  */
					/* PEERS */
static int  PEERpid = NOTOK;

static  jmp_buf PEERctx;



					/* WINDOWS */
static int dfd = NOTOK;

static int twd = NOTOK;

static char *myprompt = "(%s) ";


struct line {
    int     l_no;
    char   *l_buf;
    struct line *l_prev;
    struct line *l_next;
};


typedef struct {
    int	    w_fd;

    int	    w_flags;
#define	W_NULL	0x00
#define	W_CMND	0x01
#define	W_FAKE	0x02
#define	W_EBAR	0x04

    int	    w_wd;

    struct wstate w_ws;

    char   *w_eb;
    int	    w_ebloc;
    int	    w_ebsize;

    int	    w_cbase;
    int	    w_height;
    int	    w_cheight;
    int	    w_width;
    int	    w_cwidth;

    struct line *w_head;
    struct line *w_top;
    struct line *w_bottom;
    struct line *w_tail;

    char   w_buffer[BUFSIZ];
    int	   w_bufpos;
}	WINDOW;


static  WINDOW *Scan;
static  WINDOW *Status;
static  WINDOW *Display;
static  WINDOW *Command;


#define	NWIN	4
static	int numwins;
WINDOW *windows[NWIN + 1];


WINDOW *WINnew ();


					/* SIGNALS */
#define	ERASE	sg.sg_erase
#define	KILL	sg.sg_kill
static struct sgttyb    sg;

#define	EOFC	tc.t_eofc
#define	INTR	tc.t_intrc
static struct tchars    tc;

#define	WERASC	ltc.t_werasc
static struct ltchars ltc;


int     ALRMser (), PIPEser (), SIGser ();
int	ADJser (), REFser ();


					/* MISCELLANY */
extern int  errno;
#ifndef	BSD44
extern int  sys_nerr;
extern char *sys_errlist[];
#endif

void	adorn ();

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

    (void) SIGinit ();
    if (WINinit (nprog) == NOTOK) {
	vec[vecp] = NULL;

	vec[0] = r1bindex (vmhproc, '/');
	execvp (vmhproc, vec);
	adios (vmhproc, "unable to exec");
    }
    (void) PEERinit (vecp, vec);

    vmh ();

    done (0);
}

/*  */

static  vmh () {
    char    buffer[BUFSIZ],
            prompt[BUFSIZ];

    for (;;) {
	(void) pLOOP (RC_QRY, NULLCP);

	(void) sprintf (prompt, myprompt, invo_name);

	switch (WINgetstr (Command, prompt, buffer)) {
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
    register WINDOW **w;

    (void) signal (SIGPIPE, PIPEser);

    if (pipe (pfd0) == NOTOK || pipe (pfd1) == NOTOK)
	adios ("pipe", "unable to");
    switch (PEERpid = vfork ()) {
	case NOTOK: 
	    adios ("vfork", "unable to");/* NOTREACHED */

	case OK: 
	    for (w = windows; *w; w++)
		if ((*w) -> w_fd != NOTOK)
		    (void) close ((*w) -> w_fd);
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
	    (void) signal (SIGTERM, SIG_DFL);

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
    register WINDOW **w;

    initrc (rc);

    bp = buffer;
    (void) sprintf (bp, "%d %d", RC_VRSN, numwins);
    bp += strlen (bp);
    for (w = windows; *w; w++) {
	(void) sprintf (bp, " %d", (*w) -> w_height);
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
    WINDOW *w;

    initrc (rc);

    (void) str2peer (code, str);
    for (;;)
	switch (peer2rc (rc)) {
	    case RC_TTY:
		if (pTTY () == NOTOK)
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
		if ((w = windows[i - 1]) -> w_flags & W_CMND) {
		    (void) fmt2peer (RC_ERR, "not a display window \"%s\"",
				rc -> rc_data);
		    return NOTOK;
		}
		if (pWIN (w) == NOTOK)
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

static int  pTTY () {
    TYPESIG     (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();
    struct record   rcs;
    register struct record *rc = &rcs;

    initrc (rc);

    if (ChangeWindowDepth (dfd, twd, 0) == NOTOK)
	adios ("failed", "ChangeWindowDepth");

    hstat = signal (SIGHUP, SIG_IGN);
    istat = signal (SIGINT, SIG_IGN);
    qstat = signal (SIGQUIT, SIG_IGN);
    tstat = signal (SIGTERM, SIG_IGN);

    (void) rc2rc (RC_ACK, 0, NULLCP, rc);

    (void) signal (SIGHUP, hstat);
    (void) signal (SIGINT, istat);
    (void) signal (SIGQUIT, qstat);
    (void) signal (SIGTERM, tstat);

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

    if ((i = pWINaux (w)) == OK)
	WINless (w);

    return i;
}

/*  */

static int  pWINaux (w)
register WINDOW *w;
{
    register int    n;
    register char  *bp;
    register struct line   *lp,
                           *mp;
    struct record   rcs;
    register struct record *rc = &rcs;

    initrc (rc);

    for (lp = w -> w_head; lp; lp = mp) {
	mp = lp -> l_next;
	free (lp -> l_buf);
	free ((char *) lp);
    }
    w -> w_head = w -> w_top = w -> w_bottom = w -> w_tail = NULL;
    w -> w_bufpos = 0;

    for (;;)
	switch (rc2rc (RC_ACK, 0, NULLCP, rc)) {
	    case RC_DATA: 
		for (bp = rc -> rc_data, n = rc -> rc_len; n-- > 0; )
		    (void) WINputc (w, *bp++);
		break;

	    case RC_EOF: 
		(void) rc2peer (RC_ACK, 0, NULLCP);
		if (w -> w_bufpos)
		    (void) WINputc (w, '\n');
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

/* should dynamically determine all this stuff from gconfig... */

#define	MyX	20		/* anchored hpos */
#define	MyY	40		/*   .. vpos */
#define	MyW	800		/*   .. width */
#define	MyH	500		/*   .. height */
#define	MyS	30		/*   .. height for Status, about one line */


#define	MySlop	45		/* slop */

#define	EWIDTH	25		/* Width of vertical EBAR */
#define	ESLOP	5		/*   .. slop */


static int  WINinit (nprog) {
    short   wx,
            wy,
            wh,
	    sy;
    struct gconfig   gc;

    if (GetGraphicsConfig (fileno (stderr), &gc) == NOTOK)
	if (nprog)
	    return NOTOK;
	else
	    adios (NULLCP, "not a window");

    if ((dfd = open ("/dev/ttyw0", 2)) == NOTOK)
	adios ("/dev/ttyw0", "unable to open");

    if ((twd = GetTopWindow (dfd)) == NOTOK)
	adios ("failed", "GetTopWindow");

    (void) BlockRefreshAdjust (1);

    numwins = 0;

    wx = gc.w - (MyX + MyW + EWIDTH + ESLOP);
    Scan = WINnew (wx, wy = MyY, MyW, wh = MyH * 2 / 3, "Scan", W_EBAR);

    wy += wh + MySlop;
    Status = WINnew (wx, sy = wy, MyW, wh = MyS, "Status", W_FAKE);

    wy += wh + MySlop;
    Display = WINnew (wx, wy, MyW, MyH, "Display", W_EBAR);

    Command = WINnew (wx, sy, MyW, MyS, invo_name, W_CMND);

    windows[numwins] = NULL;

    return OK;
}

/*  */

WINDOW *WINnew (wx, wy, ww, wh, name, flags)
short	wx,
	wy,
	ww,
	wh;
char   *name;
int	flags;
{
    register WINDOW *w;

    if ((w = (WINDOW *) calloc (1, sizeof *w)) == NULL)
	adios (NULLCP, "unable to allocate window");

    if ((w -> w_flags = flags) & W_FAKE) {
	w -> w_fd = NOTOK;
	w -> w_height = 1;

	goto out;
    }

    if (w -> w_flags & W_EBAR)
	ww += EWIDTH + ESLOP;
    else
	wx += EWIDTH + ESLOP;

    if ((w -> w_fd = OpenWindow (wx, wy, ww, wh, name)) == NOTOK)
	adios ("failed", "OpenWindow");
    if ((w -> w_wd = GetTopWindow (dfd)) == NOTOK)
	adios ("failed", "GetTopWindow");
    if (GetWindowState (w -> w_fd, &w -> w_ws) == NOTOK)
	adios ("failed", "GetWindowState");
    if (SetLineDisc (w -> w_fd, TWSDISC) == NOTOK)
	adios ("failed", "SetLineDisc");

    SetBuf (w -> w_fd, 1024);
    (void) SetAdjust (w -> w_fd, numwins, ADJser);
    (void) SetRefresh (w -> w_fd, numwins, REFser);

    SetAddressing (w -> w_fd, VT_ABSOLUTE);

    if (w -> w_flags & W_EBAR) {
	w -> w_eb = CreateElevatorBar (w -> w_fd, 0, 0, EWIDTH,
			w -> w_ws.height, VT_Gray50, 1, EB_VERTICAL,
			EB_ARROWS, w -> w_ebloc = 0, w -> w_ebsize = EB_MAX,
			VT_White);
	if (w -> w_eb == NULL)
	    adios (NULLCP, "CreateElevatorBar failed");
	RefreshElevatorBar (w -> w_eb);
    }

    if ((w -> w_cbase = CharacterBaseline (w -> w_ws.font)) <= 0)
	w -> w_cbase = 14;

    if ((w -> w_cheight = CharacterHeight (w -> w_ws.font)) <= 0)
	w -> w_cheight = 20;
    w -> w_height = w -> w_ws.height / w -> w_cheight;
    if (w -> w_height < 1)
	w -> w_height = 1;

						/* 1 em */
    if ((w -> w_cwidth = CharacterWidth (w -> w_ws.font, 'm')) <= 0)
	w -> w_cwidth = 10;
    w -> w_width = (w -> w_ws.width - (w -> w_eb ? (EWIDTH + ESLOP) : 0))
		    / w -> w_cwidth;
    if (w -> w_width < 1)
	w -> w_width = 1;

out: ;
    windows[numwins++] = w;

    return w;
}

/*  */

static int  WINgetstr (w, prompt, buffer)
register WINDOW *w;
char   *prompt,
       *buffer;
{
    register int    c;
    register char  *bp,
                   *ip;
    char    image[BUFSIZ];
    struct vtseq    vts;
    register struct vtseq  *vt = &vts;

    if (w -> w_eb != NULL)
	adios (NULLCP, "internal error--elevator bar found");

    if (w -> w_head == NULL
	    && (w -> w_head = (struct line *) calloc (1, sizeof *w -> w_head))
		== NULL)
	adios (NULLCP, "unable to allocate line storage");
    w -> w_head -> l_buf = image;
    w -> w_top = w -> w_bottom = w -> w_tail = w -> w_head;

    if (ChangeWindowDepth (dfd, w -> w_wd, 0) == NOTOK)
	adios ("failed", "ChangeWindowDepth");

    (void) strcpy (image, prompt);
    bp = ip = image + strlen (image);

    Redisplay (w, 0);

    for (;;)
	switch (getvtseq (w -> w_fd, vt)) {
	    case VT_HARDKEY: 
		DisplayStatus (w -> w_fd, "no hardkeys, please");
		break;

	    case VT_ASCII: 
		switch (c = toascii (vt -> u.ascii)) {
		    case '\f': 	/* refresh? */
			break;

		    case '\r': 
		    case '\n': 
			(void) strcpy (buffer, ip);
			return DONE;

		    default: 
			if (c == INTR) {
			    adorn (NULLCP, "Interrupt");
			    return NOTOK;
			}

			if (c == EOFC) {
			    if (bp <= ip)
				return OK;
			    break;
			}

			if (c == ERASE) {
			    if (bp <= ip)
				continue;
			    bp--;
			    break;
			}

			if (c == KILL) {
			    if (bp <= ip)
				continue;
			    bp = ip;
			    break;
			}

			if (c == WERASC) {
			    if (bp <= ip)
				continue;
			    do {
				bp--;
			    } while (isspace (*bp) && bp > ip);
			    if (bp > ip) {
				do {
				    bp--;
				} while (!isspace (*bp) && bp > buffer);
				if (isspace (*bp))
				    bp++;
			    }
			    break;
			}

			if (c < ' ' || c >= '\177')
			    continue;
			*bp++ = c;
			break;
		}
		*bp = NULL;
		Redisplay (w, 0);
		break;

	    case VT_MOUSE: 
		switch (vt -> u.mouse.buttons
			& (VT_MOUSE_LEFT | VT_MOUSE_MIDDLE | VT_MOUSE_RIGHT)) {
		    case VT_MOUSE_LEFT: 
			DisplayStatus (w -> w_fd, "use middle or right button");
			break;

#define	WPOP	"WMH\0Advance\0Burst\0Exit\0EOF\0"
		    case VT_MOUSE_MIDDLE: 
			SetPosition (w -> w_fd, vt -> u.mouse.x,
				vt -> u.mouse.y);
			switch (DisplayPopUp (w -> w_fd, WPOP)) {
			    case 1: /* Advance */
			do_advance: ;
				(void) strcpy (buffer, "advance");
				return DONE;

			    case 2: /* Burst */
				(void) strcpy (buffer, "burst");
				return DONE;

			    case 3: /* Exit */
				(void) strcpy (buffer, "exit");
				return DONE;

			    case 4: /* EOF */
				return OK;

			    default: /* failed or none taken */
				break;
			}
			break;
#undef	WPOP

		    case VT_MOUSE_RIGHT: 
			goto do_advance;
		}
		break;

	    case VT_EOF: 
		adios (NULLCP, "end-of-file on window");/* NOTREACHED */

	    default: 
		DisplayStatus (w -> w_fd, "unknown VT sequence");
		break;
	}
}

/*  */

static int  WINputc (w, c)
register WINDOW *w;
register char c;
{
    register int i;
    register char  *cp;
    register struct line   *lp;

    switch (c) {
	default: 
	    if (!isascii (c)) {
		if (WINputc (w, 'M') == NOTOK || WINputc (w, '-') == NOTOK)
		    return NOTOK;
		c = toascii (c);
	    }
	    else
		if (c < ' ' || c == '\177') {
		    if (WINputc (w, '^') == NOTOK)
			return NOTOK;
		    c ^= 0100;
		}
	    break;

	case '\t': 
	    for (i = 8 - (w -> w_bufpos & 0x07); i > 0; i--)
		if (WINputc (w, ' ') == NOTOK)
		    return NOTOK;
	    return OK;

	case '\b':
	    if (w -> w_bufpos > 0)
		w -> w_bufpos--;
	    return OK;

	case '\n': 
	    break;
    }

    if (c != '\n') {
	w -> w_buffer[w -> w_bufpos++] = c;
	return OK;
    }

    w -> w_buffer[w -> w_bufpos] = NULL;
    w -> w_bufpos = 0;

    if ((lp = (struct line *) calloc (1, sizeof *lp)) == NULL)
	adios (NULLCP, "unable to allocate line storage");

    lp -> l_no = (w -> w_tail ? w -> w_tail -> l_no : 0) + 1;
    lp -> l_buf = getcpy (w -> w_buffer);
    for (cp = lp -> l_buf + strlen (lp -> l_buf) - 1; cp >= lp -> l_buf; cp--)
	if (isspace (*cp))
	    *cp = NULL;
	else
	    break;

    if (w -> w_head == NULL)
	w -> w_head = lp;
    if (w -> w_top == NULL)
	w -> w_top = lp;
    if (w -> w_bottom == NULL)
	w -> w_bottom = lp;
    if (w -> w_tail)
	w -> w_tail -> l_next = lp;
    lp -> l_prev = w -> w_tail;
    w -> w_tail = lp;

    return DONE;
}

/*  */

#define	PSLOP	2


static char mylineno[5];

static bool cancel[] =  { 1 };
static struct choice mychoices[] = { LABEL, "cancel", VT_White };

static struct question myquestions[] = {
    STRING, "Line", SZ (mylineno), (struct choice *) 0,

    TOGGLE, "", SZ (mychoices),  mychoices
};

static struct menu mymenu = { "Goto", SZ (myquestions), myquestions };

static int *myanswers[] = { (int *) mylineno, (int *) cancel };


static	WINless (w)
register WINDOW *w;
{
    int     clear,
	    pos,
            forw,
	    refresh;
    struct vtseq    vts;
    register struct vtseq  *vt = &vts;

    if (w -> w_fd == NOTOK) {
	if (w -> w_head)
	    DisplayStatus (dfd, w -> w_top -> l_buf);
	else
	    RemoveStatus (dfd);

	return;
    }

    if (ChangeWindowDepth (dfd, w -> w_wd, 0) == NOTOK)
	adios ("failed", "ChangeWindowDepth");

    Redisplay (w, 0);

    if (w -> w_bottom == w -> w_tail)
	return;

    if (w -> w_eb == NULL)
	adios (NULLCP, "internal error--no elevator bar");

    for (clear = refresh = 0, forw = 1;;) {
	if (clear) {
	    RemoveStatus (w -> w_fd);
	    clear = 0;
	}
	if (refresh) {
	    Redisplay (w, 0);
	    refresh = 0;
	}

	switch (getvtseq (w -> w_fd, vt)) {
	    case VT_HARDKEY: 
	    case VT_ASCII: 
		DisplayStatus (w -> w_fd, "use the mouse");
		clear++;
		break;

	    case VT_MOUSE: 
		switch (vt -> u.mouse.buttons
			& (VT_MOUSE_LEFT | VT_MOUSE_MIDDLE | VT_MOUSE_RIGHT)) {
		    case VT_MOUSE_LEFT: 
			if ((pos = vt -> u.mouse.x) < EWIDTH) {
			    pos = w -> w_ebloc = DoElevatorBar (w -> w_eb, pos,
				    vt -> u.mouse.y);
			    refresh = WINgoto (w, ((pos * (w -> w_tail -> l_no
						- w -> w_head -> l_no))
					/ EB_MAX) + w -> w_head -> l_no);
			}
			break;

#define	WPOP "Paging\0Next\0Prev\0Left\0Right\0First\0Last\0Goto ...\0Exit\0"
		    case VT_MOUSE_MIDDLE: 
			SetPosition (w -> w_fd, vt -> u.mouse.x,
				vt -> u.mouse.y);
			switch (DisplayPopUp (w -> w_fd, WPOP)) {
			    case 1: /* Next */
			do_next_page: ;
				if (w -> w_bottom == w -> w_tail)
				    forw = 0;
				refresh = WINgoto (w, w -> w_bottom -> l_no + 1 - PSLOP);
				break;

			    case 2: /* Prev */
			do_prev_page: ;
				if (w -> w_top == w -> w_head)
				    forw = 1;
				refresh = WINgoto (w, w -> w_top -> l_no
					- w -> w_height + PSLOP);
				break;

			    case 3: /* Left */
			    case 4: /* Right */
				DisplayStatus (w -> w_fd, "not yet");
				clear++;
				break;

			    case 5: /* First */
				forw = 1;
				refresh = WINgoto (w, w -> w_head -> l_no);
				break;

			    case 6: /* Last */
				forw = 0;
				refresh = WINgoto (w, w -> w_tail -> l_no
					- w -> w_height + 1);
				break;

			    case 7: /* Goto ... */
				(void) sprintf (mylineno, "%d",
					w -> w_top -> l_no);
				cancel[0] = 0;
				if (PresentMenu (&mymenu, myanswers)
					|| cancel[0])
				    break;
				if (sscanf (mylineno, "%d", &pos) != 1) {
				    DisplayStatus (w -> w_fd, "bad format");
				    clear++;
				    break;
				}
				if (pos < w -> w_head -> l_no
					|| pos > w -> w_tail -> l_no) {
				    DisplayStatus (w -> w_fd, "no such line");
				    clear++;
				    break;
				}
				refresh = WINgoto (w, pos);
				break;

			    case 8: /* Exit */
				return;

			    default: /* failed or none taken */
				break;
			}
			break;
#undef	WPOP

		    case VT_MOUSE_RIGHT: 
			if (forw) {
			    if (w -> w_bottom == w -> w_tail)
				return;
			    else
				goto do_next_page;
			}
			else
			    goto do_prev_page;
		}
		break;

	    case VT_EOF: 
		adios (NULLCP, "end-of-file on window");/* NOTREACHED */

	    default: 
		DisplayStatus (w -> w_fd, "unknown VT sequence");
		clear++;
		break;
	}
    }
}

/*  */

static int  WINgoto (w, n)
register WINDOW *w;
register int	n;
{
    register int    i,
                    j;
    register struct line   *lp;

    if (n > (i = w -> w_tail -> l_no - w -> w_height + 1))
	n = i;
    if (n < w -> w_head -> l_no)
	n = w -> w_head -> l_no;

    if ((i = n - (lp = w -> w_head) -> l_no)
	    > (j = abs (n - w -> w_top -> l_no)))
	i = j, lp = w -> w_top;

    if (i > (j = abs (w -> w_tail -> l_no - n)))
	i = j, lp = w -> w_tail;

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
	    lp = w -> w_head;
    }

    if (w -> w_top == lp)
	return 0;

    w -> w_top = lp;

    return 1;
}

/*  */

static int  ADJser (id, ww, wh)
int     id;
short   ww,
        wh;
{
    register WINDOW *w;

    if (id < 0 || id >= numwins)
	adios (NULLCP, "ADJser on bogus window (%d)", id);
    w = windows[id];
    if (w -> w_fd == NOTOK)
	adios (NULLCP, "ADJser on closed window (%d)", id);

    w -> w_ws.width = w -> w_ws.tw = ww;
    w -> w_ws.height = w -> w_ws.th = wh;

    if (w -> w_eb) {
	DeleteElevatorBar (w -> w_eb);
	w -> w_eb = CreateElevatorBar (w -> w_fd, 0, 0, EWIDTH,
			w -> w_ws.height, VT_Gray50, 1, EB_VERTICAL,
			EB_ARROWS, w -> w_ebloc = 0, w -> w_ebsize = EB_MAX,
			VT_White);
	if (w -> w_eb == NULL)
	    adios (NULLCP, "CreateElevatorBar failed");
    }

    Redisplay (w, 1);
}


/* ARGSUSED */

static int  REFser (id, wx, wy, ww, wh)
int     id;
short   wx,
        wy,
        ww,
        wh;
{
    short   cx,
            cy,
            cw,
            ch;
    register WINDOW *w;

    if (id < 0 || id >= numwins)
	adios (NULLCP, "REFser on bogus window (%d)", id);
    w = windows[id];
    if (w -> w_fd == NOTOK)
	adios (NULLCP, "REFser on closed window (%d)", id);


    if (GetWindowState (w -> w_fd, &w -> w_ws) == NOTOK)
	adios ("failed", "GetWindowState");

    GetPermanentClipping (w -> w_fd, &cx, &cy, &cw, &ch);
    SetPermanentClipping (w -> w_fd, wx, wy, ww, wh);
    Redisplay (w, 1);
    SetPermanentClipping (w -> w_fd, cx, cy, cw, ch);
}

/*  */

static  Redisplay (w, doeb)
register WINDOW *w;
int	doeb;
{
    register int    y;
    short   sx;
    register struct line   *lp;

    if (w -> w_fd == NOTOK)
	return;

    sx = w -> w_eb ? (EWIDTH + ESLOP) : 0;
    w -> w_height = w -> w_ws.height / w -> w_cheight;
    if (w -> w_height < 1)
	w -> w_height = 1;

    w -> w_width = (w -> w_ws.width - (w -> w_eb ? (EWIDTH + ESLOP) : 0))
	/ w -> w_cwidth;
    if (w -> w_width < 1)
	w -> w_width = 1;

    SetPosition (w -> w_fd, sx, 0);
    SetColor (w -> w_fd, VT_White);
    PaintRectangleInterior (w -> w_fd, w -> w_ws.width, w -> w_ws.height);

    if (w -> w_head) {
	SetColor (w -> w_fd, VT_Black);
	for (lp = w -> w_top, y = 0;
		lp && y < w -> w_height;
		w -> w_bottom = lp, lp = lp -> l_next, y++) {
	    SetPosition (w -> w_fd, sx, y * w -> w_cheight + w -> w_cbase);
	    PaintString (w -> w_fd, VT_STREND, lp -> l_buf);
	}
    }

    if (w -> w_eb) {
	if ((y = EB_LOC (w)) != w -> w_ebloc)
	    MoveElevator (w -> w_eb, w -> w_ebloc = y);
	if ((y = EB_SIZE (w)) != w -> w_ebsize)
	    SizeElevator (w -> w_eb, w -> w_ebsize = y);
	if (doeb)
	    RefreshElevatorBar (w -> w_eb);
    }

    Flush (w -> w_fd);
}

/*  */

static int  EB_SIZE (w)
register WINDOW *w;
{
    register int    i;

    if (w -> w_head == NULL)
	return 0;

    if ((i = w -> w_tail -> l_no - w -> w_head -> l_no) <= 0)
	return EB_MAX;

    return (((w -> w_bottom -> l_no - w -> w_top -> l_no) * EB_MAX) / i);
}


static int  EB_LOC (w)
register WINDOW *w;
{
    register int    i;

    if (w -> w_head == NULL)
	return 0;

    if ((i = w -> w_tail -> l_no - w -> w_head -> l_no) <= 0)
	return EB_MAX;

    return (((w -> w_top -> l_no - w -> w_head -> l_no) * EB_MAX) / i);
}

/*    SIGNALS */

static	SIGinit () {
    foreground ();
    if (ioctl (fileno (stdin), TIOCGETP, (char *) &sg) == NOTOK)
	adios ("failed", "ioctl TIOCGETP");
    if (ioctl (fileno (stdin), TIOCGETC, (char *) &tc) == NOTOK)
	adios ("failed", "ioctl TIOCGETC");
    if (ioctl (fileno (stdin), TIOCGLTC, (char *) &ltc) == NOTOK)
	adios ("failed", "ioctl TIOCGLTC");
    sideground ();

    (void) signal (SIGHUP, SIGser);
    (void) signal (SIGINT, SIGser);
    (void) signal (SIGQUIT, SIGser);
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
#endif	TIOCGPGRP
}


static	sideground () {
#ifdef	TIOCGPGRP
    (void) signal (SIGTTIN, SIG_DFL);
    (void) signal (SIGTTOU, SIG_DFL);
    (void) signal (SIGTSTP, SIG_DFL);
#endif	TIOCGPGRP
}

/*  */

/* ARGSUSED */

static int  ALRMser (sig)
int     sig;
{
     longjmp (PEERctx, DONE);
}


#ifdef	BSD42
/* ARGSUSED */
#endif	BSD42

static int  PIPEser (sig)
int	sig;
{
#ifndef	BSD42
    (void) signal (sig, SIG_IGN);
#endif	BSD42

    adios (NULLCP, "lost peer");
}


#ifdef	BSD42
/* ARGSUSED */
#endif	BSD42

static int  SIGser (sig)
int     sig;
{
#ifndef	BSD42
    (void) signal (sig, SIG_IGN);
#endif	BSD42

    done (1);
}

/*    MISCELLANY */

void	done (status)
int	status;
{
    if (dfd != NOTOK)
	RemoveStatus (dfd);

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

    if (dfd != NOTOK)
	(void) DisplayVector (iob, iov - iob);
    else
	(void) writev (fileno (stderr), iob, iov - iob);
}

/*  */

static	DisplayVector (iov, n)
register struct iovec   *iov;
register int     n;
{
    register int    i;
    register char  *cp;
    char    buffer[BUFSIZ];

    for (i = 0, cp = NULL; i < n; i++, iov++) {
	(void) sprintf (buffer, "%*.*s", iov -> iov_len, iov -> iov_len,
		iov -> iov_base);
	cp = add (buffer, cp);
    }

    DisplayStatus (dfd, cp);

    free (cp);

    sleep (PAUSE);

    RemoveStatus (dfd);
}
