/*
 *	@(#)ww.h	2.1.1.1 83/08/09	
 */

#include <stdio.h>
#include <sgtty.h>
#include "tt.h"

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

union ww_char {
	short c_w;		/* as a word */
	struct {
		char C_c;	/* the character part */
		char C_m;	/* the mode part */
	} c_un;
};
#define c_c c_un.C_c
#define c_m c_un.C_m

#define WWC_CMASK	0x00ff
#define WWC_MMASK	0xff00
#define WWC_MSHIFT	8

	/* c_m bits */
#define WWM_REV		0x01	/* reverse video */
#define WWM_BLK		0x02	/* blinking */
#define WWM_UL		0x04	/* underlined */
#define WWM_GLS		0x10	/* window only, glass, i.e. transparent */
#define WWM_COV		0x20	/* window only, covered */

struct ww {
	struct ww *ww_forw;	/* doubly linked list, for overlapping info */
	struct ww *ww_back;
	char ww_state;		/* state of window creation */
	char ww_wstate;		/* state for printing charcters */
	int ww_insert :1;	/* insert mode, for printing */
	int ww_mapnl :1;	/* map \n to \r\n */
	int ww_haspty :1;	/* has pty */
	char ww_index;		/* the index, for wwindex[] */
	char ww_order;		/* the overlapping order */
	struct ww_dim ww_w;	/* window dimemsions */
	short ww_nline;		/* size of the buffer */
	short ww_scroll;	/* where the window is relative to the buffer */
	struct ww_pos ww_cur;	/* the cursor position, relative to ww_w */
	char **ww_win;		/* the window */
	union ww_char **ww_buf;	/* the buffer */
	char **ww_cov;		/* the covered-by array */
	short *ww_nvis;		/* how many ww_buf chars are visible per row */
	int ww_pty;		/* file descriptor of pty */
	int ww_tty;		/* . . . tty */
	int ww_pid;		/* pid of process, if WWS_HASPROC true */
	char ww_ttyname[11];	/* /dev/ttyp? */
	/* below are things for the user */
	char ww_hasframe :1;	/* frame it */
	char ww_center :1;	/* center the label */
	int ww_id;		/* the user id */
	char *ww_label;		/* the user supplied label */
};

	/* ww_state values */
#define WWS_INITIAL	0	/* just opened */
#define WWS_HASPROC	1	/* forked, in parent */
#define WWS_INCHILD	2	/* forked, in child */
#define WWS_DEAD	3	/* child died */

struct ww_tty {
	struct sgttyb ww_sgttyb;
	struct tchars ww_tchars;
	struct ltchars ww_ltchars;
	int ww_lmode;
	int ww_ldisc;
	int ww_pgrp;
};

	/* flags to wwopen() */
#define WWO_PTY		0x01		/* want pty */
#define WWO_REVERSE	0x02		/* make it all reverse video */
#define WWO_GLASS	0x04		/* make it all glass */

struct ww wwhead;
struct ww *wwindex[NWW + 1];
struct ww wwnobody;
#define WWX_NOBODY	NWW

struct ww_tty wwoldtty, wwnewtty, wwwintty;

char *wwterm;
char wwtermcap[1024];
char wwkeys[512];
int wwnrow, wwncol;		/* the screen size */
int wwcursorrow, wwcursorcol;	/* the terminal cursor position */
int wwdtablesize;
char **wwsmap;			/* the screen map */
union ww_char **wwns;
union ww_char **wwos;
int wwbaud;
int wwbaudmap[];

	/* statistics */
int wwnwrite;
int wwnwritec;

#define wwcurrow(w)	((w)->ww_cur.r + (w)->ww_w.t)
#define wwcurcol(w)	((w)->ww_cur.c + (w)->ww_w.l)
#define wwsetcursor(r,c) (wwcursorrow = (r), wwcursorcol = (c))
#define wwcurtowin(w)	wwsetcursor(wwcurrow(w), wwcurcol(w))
#define wwbell()	putchar(CTRL(g))

struct ww *wwopen();
struct ww *wwfind();
int wwchild();
int wwsuspend();
char *unctrl();
char **wwalloc();

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
