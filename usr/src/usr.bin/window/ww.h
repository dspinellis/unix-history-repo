/*
 *	@(#)ww.h	3.16 83/09/15	
 */

#include <stdio.h>
#include <sgtty.h>

#define NWW	30

struct ww_dim {
	int nr;			/* number of rows */
	int nc;			/* number of columns */
	int t, b;		/* top, bottom */
	int l, r;		/* left, right */
};

struct ww_pos {
	int r;			/* row */
	int c;			/* column */
};

struct ww {
	struct ww *ww_forw;	/* doubly linked list, for overlapping info */
	struct ww *ww_back;
	char ww_state;		/* state of window creation */
	char ww_wstate;		/* state for printing charcters */
	char ww_modes;		/* current printing modes */
	char ww_insert :1;	/* insert mode, for printing */
	char ww_mapnl :1;	/* map \n to \r\n */
	char ww_haspty :1;	/* has pty */
	char ww_hascursor :1;	/* has fake cursor */
	char ww_hasframe :1;	/* frame it */
	char ww_index;		/* the index, for wwindex[] */
	char ww_order;		/* the overlapping order */

		/* sizes and positions */
	struct ww_dim ww_w;	/* window size and pos */
	struct ww_dim ww_b;	/* buffer size and pos */
	struct ww_dim ww_i;	/* the part inside the screen */
	struct ww_pos ww_cur;	/* the cursor position, relative to ww_w */

		/* arrays */
	char **ww_win;		/* the window */
	union ww_char **ww_buf;	/* the buffer */
	char **ww_cov;		/* the covered-by array */
	char **ww_fmap;		/* map for frame and box windows */
	short *ww_nvis;		/* how many ww_buf chars are visible per row */

		/* things for the window process */
	int ww_pty;		/* file descriptor of pty */
	int ww_tty;		/* . . . tty */
	int ww_pid;		/* pid of process, if WWS_HASPROC true */
	char ww_ttyname[11];	/* "/dev/ttyp?" */

		/* things for the user, they really don't belong here */
	char ww_center :1;	/* center the label */
	int ww_id;		/* the user window id */
	char *ww_label;		/* the user supplied label */
	struct ww_pos ww_altpos;/* alternate position */
};

struct ww_tty {
	struct sgttyb ww_sgttyb;
	struct tchars ww_tchars;
	struct ltchars ww_ltchars;
	int ww_lmode;
	int ww_ldisc;
	int ww_pgrp;
};

union ww_char {
	short c_w;		/* as a word */
	struct {
		char C_c;	/* the character part */
		char C_m;	/* the mode part */
	} c_un;
};
#define c_c c_un.C_c
#define c_m c_un.C_m

	/* parts of ww_char */
#define WWC_CMASK	0x00ff
#define WWC_MMASK	0xff00
#define WWC_MSHIFT	8

	/* c_m bits */
#define WWM_REV		0x01	/* reverse video */
#define WWM_BLK		0x02	/* blinking */
#define WWM_UL		0x04	/* underlined */
#define WWM_GLS		0x10	/* window only, glass, i.e. transparent */
#define WWM_COV		0x20	/* window only, covered */

	/* ww_state values */
#define WWS_INITIAL	0	/* just opened */
#define WWS_HASPROC	1	/* forked, in parent */
#define WWS_INCHILD	2	/* forked, in child */
#define WWS_DEAD	3	/* child died */

	/* flags for ww_fmap */
#define WWF_U		0x01
#define WWF_R		0x02
#define WWF_D		0x04
#define WWF_L		0x08
#define WWF_MASK	(WWF_U|WWF_R|WWF_D|WWF_L)
#define WWF_LABEL	0x40
#define WWF_TOP		0x80

	/* flags to wwopen() */
#define WWO_PTY		0x01		/* want pty */
#define WWO_REVERSE	0x02		/* make it all reverse video */
#define WWO_GLASS	0x04		/* make it all glass */
#define WWO_FRAME	0x08		/* this is a frame window */

	/* special ww_index value */
#define WWX_NOBODY	NWW

#define WWE_NOERR	0
#define WWE_SYS		1		/* system error */
#define WWE_NOMEM	2		/* out of memory */
#define WWE_TOOMANY	3		/* too many windows */
#define WWE_NOPTY	4		/* no more ptys */
#define WWE_SIZE	5		/* bad window size */
#define WWE_BADTERM	6		/* bad terminal type */
#define WWE_CANTDO	7		/* dumb terminal */

struct ww wwhead;
struct ww *wwindex[NWW + 1];		/* last location is for wwnobody */
struct ww wwnobody;

struct ww_tty wwoldtty;		/* the old (saved) terminal settings */
struct ww_tty wwnewtty;		/* the new (current) terminal settings */
struct ww_tty wwwintty;		/* the terminal settings for windows */
char *wwterm;			/* the terminal name */
char wwtermcap[1024];		/* place for the termcap */
char wwkeys[512];		/* termcap fields for the function keys */

int wwnrow, wwncol;		/* the screen size */
char wwavailmodes;		/* actually supported modes */
char wwcursormodes;		/* the modes for the fake cursor */
char wwwrap;			/* terminal has auto wrap around */
int wwdtablesize;		/* result of getdtablesize() call */
char **wwsmap;			/* the screen map */
union ww_char **wwos;		/* the old (current) screen */
union ww_char **wwns;		/* the new (desired) screen */
char *wwtouched;		/* wwns changed flags */
int wwbaudmap[];		/* maps stty() baud rate code into number */
int wwbaud;			/* wwbaudmap[wwoldtty.ww_sgttyb.sg_ospeed] */
int wwcursorrow, wwcursorcol;	/* where we want the cursor to be */
int wwerrno;			/* error number */

	/* statistics */
int wwnwrite, wwnwritec;
int wwnupdate, wwntouched, wwnmiss;

	/* quicky macros */
#define wwsetcursor(r,c) (wwcursorrow = (r), wwcursorcol = (c))
#define wwcurtowin(w)	wwsetcursor((w)->ww_cur.r, (w)->ww_cur.c)
#define wwbell()	putchar(CTRL(g))
#define wwunbox(w)	wwunframe(w)
#define wwclreol(w,r,c)	wwclreol1((w), (r), (c), 0)
#define wwredrawwin(w)	wwredrawwin1((w), (w)->ww_i.t, (w)->ww_i.b, 0)

	/* the window virtual terminal */
#define WWT_TERM	"TERM=window"
#define WWT_TERMCAP	"WW|window|window package:\
	:cr=^M:nl=^J:bl=^G:\
	:al=\\EL:am:le=^H:bs:cd=\\EJ:ce=\\EK:cl=\\EE:cm=\\EY%+ %+ :\
	:da:db:dc=\\EN:dl=\\EM:do=\\EB:ei=\\EO:ho=\\EH:im=\\E@:mi:\
	:nd=\\EC:ta=^I:pt:up=\\EA:"
#define WWT_REV		"se=\\Eq:so=\\Ep:"
#define WWT_UL		"ue=\\Es:us=\\Er:"

	/* our functions */
struct ww *wwopen();
struct ww *wwfind();
int wwchild();
int wwsuspend();
char *unctrl();
char **wwalloc();
char *wwerror();

	/* c library functions */
char *malloc();
char *calloc();
char *getenv();
char *tgetstr();
char *rindex();
char *strcpy();
char *strcat();

#undef MIN
#undef MAX
#define MIN(x, y)	((x) > (y) ? (y) : (x))
#define MAX(x, y)	((x) > (y) ? (x) : (y))

#undef CTRL
#define CTRL(c)		('c'&0x1f)
#define DEL		0x7f
#define ISCTRL(c)	((c) < ' ' || (c) >= DEL)
