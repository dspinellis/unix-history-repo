/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: vtrm.c,v 1.3 85/08/30 10:11:04 timo Exp $";

/* History:
 *	21-aug-85 GvR	added support for AL and DL (parametrized al and dl).
 *	The Epoch tk	created and modified.
 */

/*
 * Virtual TeRMinal package.
 *
 * This package uses termcap to determine the terminal capabilities.
 *
 * The lines and columns of our virtual terminal are numbered 
 *	y = {0...lines-1} from top to bottom, and
 *	x = {0...cols-1} from left to right,
 * respectively.
 *
 * The Visible Procedures in this package are:
 *
 * trmstart(&lines, &cols, &flags)
 * 	Obligatory initialization call (sets tty modes etc.),
 * 	Returns the height and width of the screen to the integers
 * 	whose addresses are passed as parameters, and a flag that
 *	describes some capabilities.
 *	Function return value: Yes if all went well, No if the terminal
 *	is not supported.  An error message has already been displayed.
 *
 * trmundefined()
 *	Sets internal representation of screen and attributes to undefined.
 *	This is necessary for a hard redraw, which would get optimised to
 *	oblivion,
 *
 * trmsense(&y, &x)
 *	Returns the cursor position through its parameters
 *	after a possible manual change by the user.
 *
 * trmputdata(yfirst, ylast, indent, data)
 * 	Fill lines {yfirst..ylast} with data, after skipping the initial
 *	'indent' positions. It is assumed that these positions do not contain
 *	anything dangerous (like standout cookies or null characters).
 *
 * trmscrollup(yfirst, ylast, by)
 * 	Shift lines {yfirst..ylast} up by lines (down |by| if by < 0).
 *
 * trmsync(y, x)
 * 	Call to output data to the terminal and set cursor position.
 *
 * trmbell()
 *	Send a (possibly visible) bell, immediately (flushing stdout).
 *
 * trmend()
 * 	Obligatory termination call (resets tty modes etc.).
 *
 * You may call these as one or more cycles of:
 * 	+ trmstart
 * 	+    zero or more times any of the other routines
 * 	+ trmend
 * To catch interrupts and the like, you may call trmend even in the middle
 * of trmstart.
 */


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Includes and data definitions.                                           */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

#include <stdio.h>
#include <setjmp.h>
#ifndef TERMIO
#include <sgtty.h>
#else
#include <termio.h>
#endif TERMIO
#include <signal.h>
#include <ctype.h> 		/* for isprint() */

#include "vtrm.h"

#ifdef lint
#define VOID (void)
#else
#define VOID
#endif

#define Forward
#define Visible
#define Hidden static
#define Procedure

typedef short intlet;
typedef char *string;
typedef char bool;
#define Yes ((bool) 1)
#define No  ((bool) 0)

#define Min(a,b) ((a) <= (b) ? (a) : (b))

/* tty modes */
#ifndef TERMIO
/* v7/BSD tty control */
Hidden struct sgttyb oldtty, newtty;
/* to enable type ahead for abled persons on systems that provide this: */
#ifdef TIOCSETN
#define stty(fd,bp) VOID ioctl(fd, TIOCSETN, bp)
#endif
#else
/* AT&T tty control */
Hidden struct termio oldtty, newtty;
#define gtty(fd,bp) ioctl(fd, TCGETA, bp)
#define stty(fd,bp) VOID ioctl(fd, TCSETAW, bp)
#endif TERMIO
Hidden bool know_ttys = No;

/* visible data for termcap */
char PC;
char *BC;
char *UP;
short ospeed;

Forward int outchar(); 		/* procedure for termcap's tputs */
#define Putstr(str)	tputs((str), 1, outchar)
extern char *tgoto();

/* termcap terminal capabilities */

Hidden int lines;
Hidden int cols;

Hidden bool has_am; 		/* has automatic margins */
Hidden bool has_da; 		/* display may be retained above screen */
Hidden bool has_db; 		/* display may be retained below screen */
Hidden bool has_in; 		/* not save to have null chars on the screen */
Hidden bool has_mi; 		/* move safely in insert (and delete?) mode */
Hidden bool has_ms; 		/* move safely in standout mode */
Hidden bool has_xs;		/* standout not erased by overwriting */

Hidden char *al_str; 		/* add new blank line */
Hidden char *par_al_str;	/* parametrized al (AL) */
Hidden char *cd_str; 		/* clear to end of display */
Hidden char *ce_str; 		/* clear to end of line */
Hidden char *cl_str; 		/* cursor home and clear screen */
Hidden char *cm_str; 		/* cursor motion */
Hidden char *cr_str; 		/* carriage return */
Hidden char *cs_str; 		/* change scrolling region */
Hidden char *dc_str; 		/* delete character */
Hidden char *dl_str; 		/* delete line */
Hidden char *par_dl_str;	/* parametrized dl (DL) */
Hidden char *do_str; 		/* cursor down one line */
Hidden char *dm_str; 		/* enter delete mode */
Hidden char *ed_str; 		/* end delete mode */
Hidden char *ei_str; 		/* end insert mode */
Hidden char *ho_str;		/* cursor home */
Hidden char *ic_str; 		/* insert character (iff necessary, maybe pad) */
Hidden char *im_str; 		/* enter insert mode */
Hidden char *le_str; 		/* cursor left */
Hidden char *nd_str; 		/* cursor right (non-destructive space) */
Hidden char *se_str; 		/* end standout mode */
Hidden char *sf_str; 		/* scroll text up (from bottom of region) */
Hidden char *so_str; 		/* begin standout mode */
Hidden char *sr_str; 		/* scroll text down (from top of region) */
Hidden char *te_str; 		/* end termcap */
Hidden char *ti_str; 		/* start termcap */
Hidden char *up_str; 		/* cursor up */
Hidden char *vb_str; 		/* visible bell */
Hidden char *ve_str; 		/* make cursor visible again */
Hidden char *vi_str; 		/* make cursor invisible */

/* sense cursor position, addition to termcap */
Hidden char *cp_str; 		/* format of returned Cursor Position string */
Hidden char *sp_str; 		/* Sense cursor Position from terminal */

/* terminal status */

/* calling order of Visible Procs */
Hidden bool started = No;

/* to exports the capabilities mentioned in vtrm.h: */
Hidden int flags = 0;

/* cost for impossible operations */
#define Infinity 9999
	/* Allow for adding Infinity+Infinity within range */
	/* (Range is assumed at least 2**15 - 1) */

/* The following for all sorts of undefined things (except for UNKNOWN char) */
#define Undefined (-1)

/* current mode of putting char's */
#define Normal	0
#define Insert	1
#define	Delete	2
Hidden short mode = Normal;

/* current standout mode */
#define Off	0
#define On	0200
Hidden short so_mode = Off;

/* masks for char's and intlet's */
#define NULCHAR	'\000'
#define CHAR	0177
#define SOBIT	On
#define SOCHAR	0377
/* if (has_xs) record cookies placed on screen in extra bit */
/* type of cookie is determined by the SO bit */
#define XSBIT	0400
#define SOCOOK	0600
#define COOKBITS SOCOOK
#define UNKNOWN	1
#define NOCOOK	UNKNOWN

/* current cursor position */
Hidden intlet cur_y = Undefined, cur_x = Undefined;

/* "line[y][x]" holds the char on the terminal, with the SOBIT and XSBIT.
 * the SOBIT tells whether the character is standing out, the XSBIT whether
 * there is a cookie on the screen at this position.
 * In particular a standend-cookie may be recorded AFTER the line
 * (just in case some trmputdata will write after that position).
 * "lenline[y]" holds the length of the line.
 * Unknown chars will be 1, so the optimising compare in putline will fail.
 * (Partially) empty lines are distinghuished by "lenline[y] < cols".
 */
Hidden intlet **line = 0, *lenline = 0;

/* Clear the screen initially iff only memory cursor addressing available */
Hidden bool mustclear = No;

/* Make the cursor invisible when trmsync() tries to move outside the screen */
Hidden bool no_cursor = No;

/* Optimise cursor motion */
Hidden int abs_cost; 		/* cost of absolute cursor motion */
Hidden int cr_cost; 		/* cost of carriage return */
Hidden int do_cost; 		/* cost of down */
Hidden int le_cost; 		/* cost of left */
Hidden int nd_cost; 		/* cost of right */
Hidden int up_cost; 		/* cost of up */

/* Optimise trailing match in put_line, iff the terminal can insert and delete
 * characters; the cost per n characters will be:
 * 	n * MultiplyFactor + OverHead
 */
Hidden int ins_mf, ins_oh, del_mf, del_oh;
Hidden int ed_cost, ei_cost; 		/* used in move() */

/* The type of scrolling possible determines which routines get used;
 * these may be:
 * (1) with addline and deleteline (termcap: al_str & dl_str);
 * (2) with a settable scrolling region, like VT100 (cs_str, sr_str, sf_str);
 * (3) no scrolling available. (NOT YET IMPLEMENTED)
 */
Hidden Procedure (*scr_up)();
Hidden Procedure (*scr_down)();
Forward Procedure scr1up();
Forward Procedure scr1down();
Forward Procedure scr2up();
Forward Procedure scr2down();
/*Forward Procedure scr3up(); */
/*Forward Procedure scr3down(); */

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Starting, Ending and (fatal) Error.                                      */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/* 
 * Initialization call.
 * Determine terminal capabilities from termcap.
 * Set up tty modes.
 * Start up terminal and internal administration.
 * Return Yes if succeeded, No if trouble (e.g., bad terminal type).
 */
Visible int
trmstart(plines, pcols, pflags)
int *plines;
int *pcols;
int *pflags;
{
#ifdef TRACE
fprintf(stderr, "\ttrmstart(&li, &co, &fl);\n");
#endif
	if (started)
		trmerr("trmstart called twice in succession");
	if (!gettermcaps())
		return No;
	if (!setttymode())
		return No;
	start_trm();

	*plines = lines;
	*pcols = cols;
	*pflags = flags;
	
	started = Yes;
	return Yes;
}

/*
 * Termination call.
 * Reset tty modes, etc.
 * Beware that it might be called by a catched interrupt even in the middle
 * of trmstart()!
 */
Visible Procedure
trmend()
{
#ifdef TRACE
fprintf(stderr, "\ttrmend();\n");
#endif
	set_mode(Normal);
	if (so_mode != Off)
		standend();
	Putstr(te_str);
	VOID fflush(stdout);
	resetttymode();

	started = No;
}

/*
 * Set all internal statuses to undefined, especially the contents of
 * the screen, so a hard redraw will not be optimised to heaven.
 */
Visible Procedure
trmundefined()
{
	register int y, x;
#ifdef TRACE
fprintf(stderr, "\ttrmundefined();\n");
#endif

	cur_y = cur_x = Undefined;
	mode = so_mode = Undefined;
	
	for (y = 0; y < lines; y++) {
		for (x = 0; x <= cols; x++)
			line[y][x] = 1; /* impossible char, no so bits */
		lenline[y] = cols;
	}
}

/* 
 * Give an error message, and abort.
 * The abort can be catched by the calling process.
 */
Hidden Procedure
trmerr(mess)
string mess;
{
	trmreset();
	fprintf(stderr,
		"*** System error in screen output module:\n*** %s\n", mess);
	VOID fflush(stderr);
	abort();
}

/*
 * Give an error message and reset the tty modes (but don't abort).
 */
Hidden Procedure trmmess(mess)
string mess;
{
	trmreset();
	fprintf(stderr, "*** Fatal error: %s\n", mess);
	VOID fflush(stderr);
}

/*
 * Complain about a missing terminal feature.  Otherwise like trmmess.
 */
Hidden Procedure
trmsorry(mess)
string mess;
{
    trmreset();
    fprintf(stderr, (
#ifdef BED
      "*** Sorry, this terminal isn't powerful enough to run the B editor.\n"
#else
      "*** Sorry, this terminal isn't powerful emough.\n"
#endif
    ));
    fprintf(stderr, "*** The problem is: %s.\n", mess);
#ifdef BED
    fprintf(stderr,
      "*** (You might try 'b -e' to use a standard editor instead.)\n");
#endif
    VOID fflush(stderr);
}

/*
 * Prepare for giving a (more or less fatal) error message.
 */
Hidden Procedure
trmreset()
{
	if (started) {
		move(lines-1, 0);
		clear_lines(lines-1, lines-1);
	}
	VOID fflush(stdout);
	resetttymode();
}

Hidden Procedure
check_started(m)
char *m;
{
	char s[80];
	
	if (!started) {
		VOID sprintf(s, "%s called outside trmstart/trmend", m);
		trmerr(s);
	}
}

int ccc;

/*ARGSUSED*/
Hidden Procedure
countchar(ch)
char ch;
{
	ccc++;
}

Hidden int
strcost(str)
char *str;
{
	if (str == NULL)
		return Infinity;
	return str0cost(str);
}

Hidden int
str0cost(str)
char *str;
{
	ccc = 0;
	tputs(str, 1, countchar);
	return ccc;
}

Hidden int
gettermcaps() 			/* get terminal capabilities from termcap
				 * and related static properties
				 */
{
	string trmname;
	char tc_buf[1024];
	static char strbuf[1024];
	char *area = strbuf;
	char *xPC;
	char *getenv();
	int tgetent();
	int tgetnum();
	int tgetflag();
	char *tgetstr();
	int sg;
	static bool tc_initialized = No;
#ifdef TIOCGWINSZ
	struct winsize win;
#endif
	
	if (tc_initialized)
		return Yes;
	
	if ((trmname=getenv("TERM")) == NULL) {
		trmmess("terminal type not exported in $TERM variable");
		return No;
	}
	if (tgetent(tc_buf, trmname) != 1) {
		trmmess("unknown terminal type in $TERM envariable");
		return No;
	}
	
	if (tgetflag("hc")) {
		trmsorry("can't use a hardcopy terminal");
		return No;
	}

	BC = tgetstr("le", &area);
	if (BC == NULL)
		BC = tgetstr("bc", &area);
	if (BC == NULL)
		if (tgetflag("bs"))
			BC="\b";
		else {
			trmsorry("no LEFT cursor motion");
			return No;
		}
	UP = tgetstr("up", &area);
	if (UP == NULL) {
		trmsorry("no UP cursor motion");
		return No;
	}
	xPC = tgetstr("pc", &area);
	PC = (xPC != NULL? xPC[0] : NULCHAR);

	ho_str = tgetstr("ho", &area);
	do_str = tgetstr("do", &area);
	nd_str = tgetstr("nd", &area);
	cm_str = tgetstr("cm", &area);
	if (cm_str == NULL) {
		cm_str = tgetstr("CM", &area);
		if (cm_str == NULL) {
			if (ho_str == NULL || do_str == NULL || nd_str == NULL) {
				trmsorry("no absolute cursor motion");
				return No;
			}
		}
		else
			mustclear = Yes;
	}

	al_str = tgetstr("al", &area);
	dl_str = tgetstr("dl", &area);
	par_al_str = tgetstr("AL", &area);
	par_dl_str = tgetstr("DL", &area);
	if (al_str && dl_str) {
		scr_up = scr1up;
		scr_down = scr1down;
		flags |= CAN_SCROLL;
	}
	else {
		cs_str = tgetstr("cs", &area);
		sf_str = tgetstr("sf", &area);
		if (sf_str == NULL)
			sf_str = "\n";
		sr_str = tgetstr("sr", &area);
		if (cs_str && sr_str) {
			scr_up = scr2up;
			scr_down = scr2down;
			flags |= CAN_SCROLL;
		}
		else {
			trmsorry("can't scroll");
			return No;
		}
	}
		
	lines = tgetnum("li");
	cols = tgetnum("co");
#ifdef TIOCGWINSZ
	if (ioctl (0, TIOCGWINSZ, &win) == 0) {
		if (win.ws_col)
			cols = win.ws_col;
		if (win.ws_row)
			lines = win.ws_row;
	}
#endif
	if (lines == -1) lines = 24;
	if (cols == -1) cols = 80;
	
	has_am = tgetflag("am");
	has_db = tgetflag("db");
	has_in = tgetflag("in");
	has_mi = tgetflag("mi");
	has_ms = tgetflag("ms");
	has_xs = tgetflag("xs");
	if ((sg=tgetnum("sg")) == 0)
		has_xs = Yes;
	else if (sg > 0) {
		trmsorry("video attributes take up space on the screen");
		return No;
	}
	
	cd_str = tgetstr("cd", &area);
	ce_str = tgetstr("ce", &area);
	if (!ce_str) {
		trmsorry("can't clear to end of line");
		return No;
	}
	cl_str = tgetstr("cl", &area);
	cr_str = tgetstr("cr", &area);
	if (cr_str == NULL) cr_str = "\r";
	dc_str = tgetstr("dc", &area);
	dm_str = tgetstr("dm", &area);
	if (do_str == NULL) do_str = tgetstr("nl", &area);
	if (do_str == NULL) do_str = "\n";
	ed_str = tgetstr("ed", &area);
	ei_str = tgetstr("ei", &area);
	ic_str = tgetstr("ic", &area);
	im_str = tgetstr("im", &area);
	le_str = BC;
	se_str = tgetstr("se", &area);
	so_str = tgetstr("so", &area);
	te_str = tgetstr("te", &area);
	ti_str = tgetstr("ti", &area);
	up_str = UP;
	vb_str = tgetstr("vb", &area);
	if (vb_str == NULL) 	/* then we will do with the audible bell */
		vb_str = "\007";
	ve_str = tgetstr("ve", &area);
	vi_str = tgetstr("vi", &area);
	
	/* cursor sensing (non standard) */
	cp_str = tgetstr("cp", &area);
	sp_str = tgetstr("sp", &area);
	if (cp_str != NULL && sp_str != NULL)
		flags |= CAN_SENSE;

	if (so_str != NULL && se_str != NULL)
		flags |= HAS_STANDOUT;

	/* calculate costs of local and absolute cursor motions */
	if (cm_str == NULL)
		abs_cost = Infinity;
	else
		abs_cost = strcost(tgoto(cm_str, 0, 0));
	cr_cost = strcost(cr_str);
	do_cost = strcost(do_str);
	le_cost = strcost(le_str);
	nd_cost = strcost(nd_str);
	up_cost = strcost(up_str);

	/* cost of leaving insert or delete mode, used in move() */
	ei_cost = str0cost(ei_str);
	ed_cost = str0cost(ed_str);
	
	/* calculate insert and delete cost multiply_factor and overhead */
	if (((im_str && ei_str) || ic_str) && dc_str) {
		flags |= CAN_OPTIMISE;
		ins_mf = 1 + str0cost(ic_str);
		ins_oh = str0cost(im_str) + ei_cost;
		del_mf = str0cost(dc_str);
		del_oh = str0cost(dm_str) + ed_cost;
	}
		
	tc_initialized = Yes;
	return Yes;
}

Hidden int
setttymode()
{
	if (!know_ttys) {
		if (gtty(1, &oldtty) != 0 || gtty(1, &newtty) != 0) {
			trmmess("can't get tty modes (output not a terminal)");
			return No;
		}
#ifndef TERMIO
		ospeed = oldtty.sg_ospeed;
#ifdef PWB
		newtty.sg_flags = (newtty.sg_flags & ~ECHO & ~CRMOD & ~XTABS)
				  | RAW;
#else PWB
		newtty.sg_flags = (newtty.sg_flags & ~ECHO & ~CRMOD & ~XTABS)
				  | CBREAK;
#endif PWB
#else TERMIO
		ospeed= oldtty.c_lflag & CBAUD;
		newtty.c_iflag &= ~ICRNL; /* No CR->NL mapping on input */
		newtty.c_oflag &= ~ONLCR; /* NL doesn't output CR */
		newtty.c_lflag &= ~(ICANON|ECHO); /* No line editing, no echo */
		newtty.c_cc[VMIN]= 3; /* wait for 3 characters */
		newtty.c_cc[VTIME]= 1; /* or 0.1 sec. */
#endif TERMIO
		know_ttys = Yes;
	}
	stty(1, &newtty);
	return Yes;
}

Hidden Procedure
resetttymode()
{
	if (know_ttys)
		stty(1, &oldtty);
}

Hidden char*
lalloc(size)
unsigned size;
{
	char *l;
	char *malloc();
	
	l = malloc(size);
	if (l == NULL)
		trmerr("not enough memory for screen buffer");
	return l;
}

Hidden Procedure
start_trm()
{
	register int y;
	
	if (line == 0) {
		line = (intlet**) lalloc((unsigned) lines * sizeof(intlet*));
		for (y = 0; y < lines; y++)
			line[y] = (intlet*) lalloc((unsigned) ((cols+1)*sizeof(intlet)));
	}
	if (lenline == 0)
		lenline = (intlet*) lalloc((unsigned) lines * sizeof(intlet));

	
	trmundefined();
	
	Putstr(ti_str);
	if (cs_str)
		Putstr(tgoto(cs_str, lines-1, 0));
	if (mustclear)
		clear_lines(0, lines-1);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Sensing and moving the cursor.                                           */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*
 * Sense the current (y, x) cursor position, after a possible manual
 * change by the user with local cursor motions.
 * If the terminal cannot be asked for the current cursor position,
 * or if the string returned by the terminal is garbled,
 * the position is made Undefined.
 */
Visible Procedure
trmsense(py, px)
	int *py;
	int *px;
{
	bool getpos();

#ifdef TRACE
fprintf(stderr, "\ttrmsense(&yy, &xx);\n");
#endif
	check_started("trmsense");

	*py = *px = Undefined;
	set_mode(Normal);
	if (so_mode != Off)
		standend();
	
	if (flags&CAN_SENSE && getpos(py, px)) {
		if (*py < 0 || lines <= *py || *px < 0 || cols <= *px)
			*py = *px = Undefined;
	}
	cur_y = *py;
	cur_x = *px;
}

Hidden bool
getpos(py, px)
int *py, *px;
{
	char *format = cp_str;
	int fc; 		/* current format character */
	int ic; 		/* current input character */
	int num;
	int on_y = 1;
	bool incr_orig = No;
	int i, ni;

	Putstr(sp_str);
	VOID fflush(stdout);

	while (fc = *format++) {
		if (fc != '%') {
			if (getchar() != fc)
				return No;
		}
		else {
			switch (fc = *format++) {
			case '%':
				if (getchar() != '%')
					return No;
				continue;
			case 'r':
				on_y = 1 - on_y;
				continue;
			case 'i':
				incr_orig = Yes;
				continue;
			case 'd':
				ic = getchar();
				if (!isdigit(ic))
					return No;
				num = ic - '0';
				while (isdigit(ic=getchar()))
					num = 10*num + ic - '0';
				VOID ungetc(ic, stdin);
				break;
			case '2':
			case '3':
				ni = fc - '0';
		    		num = 0;
				for (i=0; i<ni; i++) {
					ic = getchar();
					if (isdigit(ic))
						num = 10*num + ic - '0';
					else
						return No;
				}
				break;
			case '+':
				num = getchar() - *format++;
				break;
			case '-':
				num = getchar() + *format++;
				break;
			default:
				return No;
			}
			/* assign num to parameter */
			if (incr_orig)
				num--;
			if (on_y)
				*py = num;
			else
				*px = num;
			on_y = 1 - on_y;
		}
	}

	return Yes;
}
		
/* 
 * To move over characters by rewriting them, we have to check:
 * (1) that the screen has been initialised on these positions;
 * (2) we do not screw up characters
 * when rewriting line[y] from x_from upto x_to
 */
Hidden bool
rewrite_ok(y, xfrom, xto)
int y, xfrom, xto;
{
	register intlet *plnyx, *plnyto;
	
	if (xto > lenline[y])
		return No;

	plnyto = &line[y][xto];
	for (plnyx = &line[y][xfrom]; plnyx <= plnyto; plnyx++)
		if (*plnyx == UNKNOWN
		    ||
		    (!has_xs && (*plnyx & SOBIT) != so_mode)
		   )
			return No;
	return Yes;
}
		
/*
 * Move to position y,x on the screen
 */
/* possible move types for y and x respectively: */
#define None	0
#define Down	1
#define Up	2
#define Right	1
#define ReWrite	2
#define Left	3
#define CrWrite	4

Hidden Procedure
move(y, x)
int y, x;
{
	int dy, dx;
	int y_cost, x_cost, y_move, x_move;
	int mode_cost;
	int xi;
	
	if (cur_y == y && cur_x == x)
		return;
	
	if (!has_mi || mode == Undefined)
		set_mode(Normal);
	if (!has_xs && ((!has_ms && so_mode != Off) || so_mode == Undefined))
		standend();
	
	if (cur_y == Undefined || cur_x == Undefined)
		goto absmove;
	
	dy = y - cur_y;
	dx = x - cur_x;

	if (dy > 0) {
		y_move = Down;
		y_cost = dy * do_cost;
	}
	else if (dy < 0) {
		y_move = Up;
		y_cost = -dy * up_cost;
	}
	else {
		y_move = None;
		y_cost = 0;
	}
	if (y_cost < abs_cost) {
		switch (mode) {
		case Normal:
			mode_cost = 0;
			break;
		case Insert:
			mode_cost = ei_cost;
			break;
		case Delete:
			mode_cost = ed_cost;
			break;
		}
		if (dx > 0) {
			x_cost = dx + mode_cost;
			if (dx*nd_cost < x_cost || !rewrite_ok(y, cur_x, x)) {
				x_cost = dx * nd_cost;
				x_move = Right;
			}
			else
				x_move = ReWrite;
		}
		else if (dx < 0) {
			x_cost = -dx * le_cost;
			x_move = Left;
		}
		else {
			x_cost = 0;
			x_move = None;
		}
		if (cr_cost + x + mode_cost < x_cost && rewrite_ok(y, 0, x)) {
			x_move = CrWrite;
			x_cost = cr_cost + x + mode_cost;
		}
	}
	else
		x_cost = abs_cost;

	if (y_cost + x_cost < abs_cost) {
		switch (y_move) {
		case Down:
			while (dy-- > 0) Putstr(do_str);
			break;
		case Up:
			while (dy++ < 0) Putstr(up_str);
			break;
		}
		switch (x_move) {
		case Right:
			while (dx-- > 0) Putstr(nd_str);
			break;
		case Left:
			while (dx++ < 0) Putstr(le_str);
			break;
		case CrWrite:
			Putstr(cr_str);
			cur_x = 0;
			/* FALL THROUGH */
		case ReWrite:
			set_mode(Normal);
			for (xi = cur_x; xi < x; xi++)
				putchar(line[y][xi]);
			break;
		}
	}
	else
	{
    absmove:
		if (cm_str == NULL) {
			Putstr(ho_str);
			for (cur_y = 0; cur_y < y; ++cur_y)
				Putstr(do_str);
			/* Should try to use tabs here: */
			for (cur_x = 0; cur_x < x; ++cur_x)
				Putstr(nd_str);
		}
		else
			Putstr(tgoto(cm_str, x, y));
	}
	
	cur_y = y;
	cur_x = x;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Putting data on the screen.                                              */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*
 * Fill screen area with given data.
 * Characters with the SO-bit (0200) set are put in standout mode.
 */
Visible Procedure
trmputdata(yfirst, ylast, indent, data)
int yfirst;
int ylast;
register int indent;
register string data;
{
	register int y;
	int x, len, lendata, space;
		
#ifdef TRACE
fprintf(stderr, "\ttrmputdata(%d, %d, %d, \"%s\");\n", yfirst, ylast, indent, data);
#endif
	check_started("trmputdata");
	
	if (yfirst < 0)
		yfirst = 0;
	if (ylast >= lines)
		ylast = lines-1;
	space = cols*(ylast-yfirst+1) - indent;
	if (space <= 0)
		return;
	yfirst += indent/cols;
	indent %= cols;
	if (data) {
		x = indent;
		lendata = strlen(data);
		if (ylast == lines-1 && lendata >= space)
			lendata = space - 1;
		len = Min(lendata, cols-x);
		for (y = yfirst; y <= ylast; ) {
			put_line(y, x, data, len);
			y++;
			lendata -= len;
			if (lendata > 0) {
				x = 0;
				data += len;
				len = Min(lendata, cols);
			}
			else
				break;
		}
	}
	if (y <= ylast)
		clear_lines(y, ylast);
}

/* 
 * We will first try to get the picture:
 *
 *                  op>>>>>>>>>>>op          oq<<<<<<<<<<<<<<<<<<<<<<<<oq
 *                  ^            ^           ^                         ^
 *           <xskip><-----m1----><----od-----><-----------m2----------->
 *   OLD:   "You're in a maze of twisty little pieces of code, all alike"
 *   NEW:          "in a maze of little twisting pieces of code, all alike"
 *                  <-----m1----><-----nd------><-----------m2----------->
 *                  ^            ^             ^                         ^
 *                  np>>>>>>>>>>>np            nq<<<<<<<<<<<<<<<<<<<<<<<<nq
 * where
 *	op, oq, np, nq are pointers to start and end of Old and New data,
 * and
 *	xskip = length of indent to be skipped,
 *	m1 = length of Matching part at start,
 *	od = length of Differing mid on screen,
 *	nd = length of Differing mid in data to be put,
 *	m2 = length of Matching trail.
 *
 * Then we will try to find a long blank-or-cleared piece in <nd+m2>:
 *
 *    <---m1---><---d1---><---nb---><---d2---><---m2--->
 *              ^         ^         ^        ^         ^
 *              np        bp        bq1      nq        nend
 * where
 *	bp, bq are pointers to start and AFTER end of blank piece,
 * and
 *	d1 = length of differing part before blank piece,
 *	nb = length of blank piece to be skipped,
 *	d2 = length of differing part after blank piece.
 * Remarks:
 *	d1 + nb + d2 == nd,
 * and
 *	d2 maybe less than 0.
 */
Hidden int
put_line(y, xskip, data, len)
int y, xskip;
string data;
int len;
{
	register intlet *op, *oq;
	register char *np, *nq, *nend;
	char *bp, *bq1, *p, *q;
	int m1, m2, od, nd, delta, dd, d1, nb, d2;
	bool skipping;
	int cost, o_cost; 	/* normal and optimising cost */
	
	/* calculate the magic parameters */
	op = &line[y][xskip];
	oq = &line[y][lenline[y]-1];
	np = data;
	nq = nend = data + len - 1;
	m1 = m2 = 0;
	while ((*op&SOCHAR) == (((intlet)*np)&SOCHAR) && op <= oq && np <= nq)
		op++, np++, m1++;
	if (flags & CAN_OPTIMISE)
		while ((*oq&SOCHAR) == (((intlet)*nq)&SOCHAR) && op <= oq && np <= nq)
			oq--, nq--, m2++;
	od = oq - op + 1;
	nd = nq - np + 1;
	/* now we have the first picture above */

	if (od==0 && nd==0)
		return;
	delta = nd - od;

	/* find the blank piece */
	p = q = bp = bq1 = np;
	oq += m2; 		/* back to current eol */
	if (!has_in) {
		while (p <= nend) {
			while (q<=nend && *q==' ' && (op>oq || *op==' '))
				q++, op++;
			if (q - p > bq1 - bp)
				bp = p, bq1 = q;
			p = ++q;
			op++;
		}
	}
	d1 = bp - np;
	nb = bq1 - bp;
	d2 = nq - bq1 + 1;
	
	/* what is cheapest:
	 *	normal: put nd+m2;                         (dd = nd+m2)
	 *	skipping: put d1, skip nb, put d2+m2;      (dd = d2+m2)
	 *	optimise: put dd, insert or delete delta.  (dd = min(od,nd))
	 */
	cost = nd + m2; 	/* normal cost */
	if (nb > abs_cost || (d1 == 0 && nb > 0)) {
		skipping = Yes;
		cost -= nb - (d1>0 ? abs_cost : 0); /* skipping cost */
		dd = d2;
	}
	else {
		skipping = No;
		dd = nd;
	}
	
	if (m2 != 0) {
		/* try optimising */
		o_cost = Min(od, nd);
		if (delta > 0)
			o_cost += delta * ins_mf + ins_oh;
		else if (delta < 0)
			o_cost += -delta * del_mf + del_oh;
		if (o_cost >= cost) {
			/* discard m2, no optimise */
			dd += m2;
			m2 = 0;
		}
		else {
			dd = Min(od, nd);
			skipping = No;
		}
	}

	/* and now for the real work */
	if (!skipping || d1 > 0)
		move(y, xskip + m1);

	if (has_xs)
		get_so_mode();
	
	if (skipping) {
		if (d1 > 0) {
			set_mode(Normal);
			put_str(np, d1, No);
		}
		if (has_xs && so_mode != Off)
			standend();
		set_blanks(y, xskip+m1+d1, xskip+m1+d1+nb);
		if (dd != 0 || delta < 0) {
			move(y, xskip+m1+d1+nb);
			np = bq1;
		}
	}
	
	if (dd > 0) {
		set_mode(Normal);
		put_str(np, dd, No);
	}
	
	if (m2 > 0) {
		if (delta > 0) {
			set_mode(Insert);
			ins_str(np+dd, delta);
		}
		else if (delta < 0) {
			set_mode(Delete);
			del_str(-delta);
		}
	}
	else {
		if (delta < 0) {
			clr_to_eol();
			return;
		}
	}
	
	lenline[y] = xskip + len;
	if (cur_x == cols) {
		if (!has_mi)
			set_mode(Normal);
		if (!has_ms)
			so_mode = Undefined;
		if (has_am)
			cur_y++;
		else
			Putstr(cr_str);
		cur_x = 0;
	}
	else if (has_xs) {
		if (m2 == 0) {
			if (so_mode == On)
				standend();
		}
		else {
			if (!(line[cur_y][cur_x] & XSBIT)) {
				if (so_mode != (line[cur_y][cur_x] & SOBIT))
					(so_mode ? standend() : standout());
			}
		}
	}
}

Hidden Procedure
set_mode(m)
int m;
{
	if (m == mode)
		return;
	switch (mode) {
	case Insert:
		Putstr(ei_str);
		break;
	case Delete:
		Putstr(ed_str);
		break;
	case Undefined:
		Putstr(ei_str);
		Putstr(ed_str);
		break;
	}
	switch (m) {
	case Insert:
		Putstr(im_str);
		break;
	case Delete:
		Putstr(dm_str);
		break;
	}
	mode = m;
}

Hidden Procedure
get_so_mode()
{
	if (cur_x >= lenline[cur_y] || line[cur_y][cur_x] == UNKNOWN)
		so_mode = Off;
	else
		so_mode = line[cur_y][cur_x] & SOBIT;
}

Hidden Procedure
standout()
{
	Putstr(so_str);
	so_mode = On;
	if (has_xs)
		line[cur_y][cur_x] |= SOCOOK;
}

Hidden Procedure
standend()
{
	Putstr(se_str);
	so_mode = Off;
	if (has_xs)
		line[cur_y][cur_x] = (line[cur_y][cur_x] & ~SOBIT) | XSBIT;
}

Hidden Procedure
put_str(data, n, inserting)
char *data;
int n;
bool inserting;
{
	register intlet c, so;
	intlet *ln_y_x, *ln_y_end;
	
	so = so_mode;
	if (has_xs) {
		ln_y_x = &line[cur_y][cur_x];
		ln_y_end = &line[cur_y][lenline[cur_y]];
	}
	while (n-- > 0) {
		if (has_xs && ln_y_x <= ln_y_end && ((*ln_y_x)&XSBIT))
			so = so_mode = (*ln_y_x)&SOBIT;
			/* this also checks for the standend cookie AFTER */
			/* the line because off the equals sign in <= */
		c = ((intlet)(*data++))&SOCHAR;
		if ((c&SOBIT) != so) {
			so = c&SOBIT;
			so ? standout() : standend();
 		}
		if (inserting)
			Putstr(ic_str);
		put_c(c);
		if (has_xs)
			ln_y_x++;
	}
}

Hidden Procedure
ins_str(data, n)
char *data;
int n;
{
	int x;
	
	/* x will start AFTER the line, because there might be a cookie */
	for (x = lenline[cur_y]; x >= cur_x; x--)
		line[cur_y][x+n] = line[cur_y][x];
	put_str(data, n, Yes);
}

Hidden Procedure
del_str(n)
int n;
{
	int x, xto;
	
	xto = lenline[cur_y] - n; /* again one too far because of cookie */
	if (has_xs) {
		for (x = cur_x + n; x >= cur_x; x--) {
			if (line[cur_y][x] & XSBIT)
				break;
		}
		if (x >= cur_x)
			line[cur_y][cur_x+n] =
				(line[cur_y][cur_x+n] & CHAR)
				|
				(line[cur_y][x] & COOKBITS);
	}
	for (x = cur_x; x <= xto; x++)
		line[cur_y][x] = line[cur_y][x+n];
	while (n-- > 0)
		Putstr(dc_str);
}

Hidden Procedure
put_c(c)
intlet c;
{
	char ch;
	intlet xs_flag;
	
	ch = c&CHAR;
	if (!isprint(ch) && ch != ' ') { /* V7 isprint doesn't include blank */
		ch = '?';
		c = (c&SOBIT)|'?';
	}
	putchar(ch);
	if (has_xs)
		xs_flag = line[cur_y][cur_x]&XSBIT;
	else
		xs_flag = 0;
	line[cur_y][cur_x] = (c&SOCHAR)|xs_flag;
	cur_x++;
}

Hidden Procedure
clear_lines(yfirst, ylast)
int yfirst, ylast ;
{
	register int y;
	
	if (!has_xs && so_mode != Off)
		standend();
	if (cl_str && yfirst == 0 && ylast == lines-1) {
		Putstr(cl_str);
		cur_y = cur_x = 0;
		return;
	}
	for (y = yfirst; y <= ylast; y++) {
		if (lenline[y] > 0) {
			move(y, 0);
			if (ylast == lines-1 && cd_str) {
				Putstr(cd_str);
				while (y <= ylast) {
					if (has_xs) line[y][0] = NOCOOK;
					lenline[y++] = 0;
				}
				break;
			}
			else {
				clr_to_eol();
			}
		}
	}
}

Hidden Procedure
clr_to_eol()
{
	lenline[cur_y] = cur_x;
	if (!has_xs && so_mode != Off)
		standend();
	Putstr(ce_str);
	if (has_xs) {
		if (cur_x == 0)
			line[cur_y][0] = NOCOOK;
		else if (line[cur_y][cur_x-1]&SOBIT)
			standend();
	}
}

Hidden Procedure
set_blanks
(y, xfrom, xto)
int y, xfrom, xto;
{
	register int x;
	
	for (x = xfrom; x < xto; x++) {
		line[y][x] = (line[y][x]&XSBIT) | ' ';
	}
}

/* 
 * outchar() is used by termcap's tputs;
 * we can't use putchar because that's probably a macro
 */
Hidden int
outchar(ch)
char ch;
{
	putchar(ch);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Scrolling (part of) the screen up (or down, dy<0).                       */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

Visible Procedure
trmscrollup(yfirst, ylast, by)
register int yfirst;
register int ylast;
register int by;
{
#ifdef TRACE
fprintf(stderr, "\ttrmscrollup(%d, %d, %d);\n", yfirst, ylast, by);
#endif
	check_started("trmscrollup");
	
	if (yfirst < 0)
		yfirst = 0;
	if (ylast >= lines)
		ylast = lines-1;

	if (yfirst > ylast)
		return;

	if (!has_xs && so_mode != Off)
		standend();
	
	if (by > 0 && yfirst + by > ylast
	    ||
	    by < 0 && yfirst - by > ylast)
	{
		clear_lines(yfirst, ylast);
		return;
	}
	
	if (by > 0) {
		(*scr_up)(yfirst, ylast, by);
		scr_lines(yfirst, ylast, by, 1);
	}
	else if (by < 0) {
		(*scr_down)(yfirst, ylast, -by);
		scr_lines(ylast, yfirst, -by, -1);
	}
}

Hidden Procedure
scr_lines(yfrom, yto, n, dy)
int yfrom, yto, n, dy;
{
	register int y;
	intlet *saveln;
	
	while (n-- > 0) {
		saveln = line[yfrom];
		for (y = yfrom; y != yto; y += dy) {
			line[y] = line[y+dy];
			lenline[y] = lenline[y+dy];
		}
		line[yto] = saveln;
		lenline[yto] = 0;
		if (has_xs) line[yto][0] = NOCOOK;
	}
}

Hidden Procedure
scr1up(yfirst, ylast, n)
	int yfirst;
	int ylast;
	int n;
{
	move(yfirst, 0);
	dellines(n);
	if (ylast < lines-1) {
		move(ylast-n+1, 0);
		addlines(n);
	}
}

Hidden Procedure
scr1down(yfirst, ylast, n)
	int yfirst;
	int ylast;
	int n;
{
	if (ylast == lines-1) {
		clear_lines(ylast-n+1, ylast);
	}
	else {
		move(ylast-n+1, 0);
		dellines(n);
	}
	move(yfirst, 0);
	addlines(n);
}

Hidden Procedure
addlines(n)
register int n;
{
	if (par_al_str && n > 1)
			Putstr(tgoto(par_al_str, n, n));
	else {
		while (n-- > 0)
			Putstr(al_str);
	}
}

Hidden Procedure
dellines(n)
register int n;
{
	if (par_dl_str && n > 1)
		Putstr(tgoto(par_dl_str, n, n));
	else {
		while (n-- > 0)
			Putstr(dl_str);
	}
}

Hidden Procedure
scr2up(yfirst, ylast, n)
int yfirst, ylast, n;
{
	Putstr(tgoto(cs_str, ylast, yfirst));
	cur_y = cur_x = Undefined;
	move(ylast, 0);
	while (n-- > 0) {
		Putstr(sf_str);
		if (has_db && ylast == lines-1)
			clr_to_eol();
	}
	Putstr(tgoto(cs_str, lines-1, 0));
	cur_y = cur_x = Undefined;
}

Hidden Procedure
scr2down(yfirst, ylast, n)
int yfirst, ylast, n;
{
	Putstr(tgoto(cs_str, ylast, yfirst));
	cur_y = cur_x = Undefined;
	move(yfirst, 0);
	while (n-- > 0) {
		Putstr(sr_str);
		if (has_da && yfirst == 0)
			clr_to_eol();
	}
	Putstr(tgoto(cs_str, lines-1, 0));
	cur_y = cur_x = Undefined;
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Synchronization, move cursor to given position (or previous if < 0).     */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

Visible Procedure
trmsync(y, x)
	int y;
	int x;
{
#ifdef TRACE
fprintf(stderr, "\ttrmsync(%d, %d);\n", y, x);
#endif
	check_started("trmsync");
	
	if (0 <= y && y < lines && 0 <= x && x < cols) {
		move(y, x);
		if (no_cursor) {
			Putstr(ve_str);
			no_cursor = No;
		}
	}
	else if (no_cursor == No) {
		Putstr(vi_str);
		no_cursor = Yes;
	}
	VOID fflush(stdout);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Send a bell, visible if possible.                                        */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

Visible Procedure
trmbell()
{
#ifdef TRACE
fprintf(stderr, "\ttrmbell();\n");
#endif
	check_started("trmbell");
	
	Putstr(vb_str);
	VOID fflush(stdout);
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* Show the current internal statuses of the screen on stderr.              */
/* For debugging only.                                                      */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

#ifdef SHOW
Visible Procedure
trmshow(s)
char *s;
{
	int y, x;
	
	fprintf(stderr, "<<< %s >>>\n", s);
	for (y = 0; y < lines; y++) {
		for (x = 0; x <= lenline[y] /*** && x < cols-1 ***/ ; x++) {
			fputc(line[y][x]&CHAR, stderr);
		}
		fputc('\n', stderr);
		for (x = 0; x <= lenline[y] && x < cols-1; x++) {
			if (line[y][x]&SOBIT)
				fputc('-', stderr);
			else
				fputc(' ', stderr);
		}
		fputc('\n', stderr);
		for (x = 0; x <= lenline[y] && x < cols-1; x++) {
			if (line[y][x]&XSBIT)
				fputc('+', stderr);
			else
				fputc(' ', stderr);
		}
		fputc('\n', stderr);
	}
	fprintf(stderr, "CUR_Y = %d, CUR_X = %d.\n", cur_y, cur_x);
	VOID fflush(stderr);
}
#endif
