/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

struct window {
	Window	*w_prev,	/* circular list */
		*w_next;
	Buffer	*w_bufp;	/* buffer associated with this window */
	Line	*w_top,		/* top line */
		*w_line;	/* current line */
	int	w_char,
		w_height,	/* window height */
		w_topnum,	/* line number of the topline */
		w_dotcol,	/* UpdWindow sets this ... */
		w_dotline,	/* ... and this */
		w_flags,
#define	W_TOPGONE	01
#define	W_CURGONE	02	/* topline (curline) of window has been deleted
				   since the last time a redisplay was called */
#define W_VISSPACE	04
#define W_NUMLINES	010
		w_LRscroll;	/* amount of LeftRight scrolling in window */
#ifdef	MAC
	int	w_topline;	/* row number of top line in window */
	char **w_control;	/* scroll bar for window */
#endif
};

extern Window	*fwind,		/* first window in list */
		*curwind;	/* current window */

#define one_windp()	(fwind->w_next == fwind)
#define HALF(wp)	(((wp)->w_height - 1) / 2)
#define SIZE(wp)	((wp)->w_height - 1)

extern int
	FLine proto((struct window *w)),
	in_window proto((struct window *windes,struct line *line));

extern Window
	*div_wind proto((struct window *wp,int n)),
	*windbp proto((struct buffer *bp));

extern void
	CalcWind proto((struct window *w)),
	CentWind proto((struct window *w)),
	SetTop proto((struct window *w,struct line *line)),
	SetWind proto((struct window *new)),
	WindSize proto((struct window *w,int inc)),
	del_wind proto((struct window *wp)),
	pop_wind proto((char *name,int clobber,int btype)),
	tiewind proto((struct window *w,struct buffer *bp)),
	winit proto((void));
