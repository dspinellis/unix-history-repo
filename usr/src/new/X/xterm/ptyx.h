/*
 *	$Source: /u1/X/xterm/RCS/ptyx.h,v $
 *	$Header: ptyx.h,v 10.101 86/12/01 16:57:15 swick Rel $
 */

#include <X/mit-copyright.h>

/*	Copyright	Massachusetts Institute of Technology	1984, 1985 */

/* ptyx.h */
/* @(#)ptyx.h	1.4 (Berkeley/CSRG) 9/17/87 */
/* @(#)ptyx.h       X10/6.6B 12/26/86 */

#define	FALSE		0
#define	TRUE		1

#define MAX_COLS	200
#define MAX_ROWS	128

/*
 * The origin of a screen is 0, 0.  Therefore, the number of rows
 * on a screen is screen->max_row + 1, and similarly for columns.
 */

typedef char **ScrnBuf;

/*
 * ANSI emulation.
 */
#define INQ	0x05
#define	FF	0x0C			/* C0, C1 control names		*/
#define	LS1	0x0E
#define	LS0	0x0F
#define	CAN	0x18
#define	SUB	0x1A
#define	ESC	0x1B
#define US	0x1F
#define	DEL	0x7F
#define HTS     ('H'+0x40)
#define	SS2	0x8E
#define	SS3	0x8F
#define	DCS	0x90
#define	OLDID	0x9A			/* ESC Z			*/
#define	CSI	0x9B
#define	ST	0x9C
#define	OSC	0x9D
#define	PM	0x9E
#define	APC	0x9F
#define	RDEL	0xFF

#define	NBOX	5			/* Number of Vertices in box	*/
#define	NPARAM	10			/* Max. parameters		*/

#define	MINHILITE	32
#define	TITLEPAD	4

typedef struct {
	unsigned char	a_type;
	unsigned char	a_pintro;
	unsigned char	a_final;
	unsigned char	a_inters;
	char	a_nparam;		/* # of parameters		*/
	char	a_dflt[NPARAM];		/* Default value flags		*/
	short	a_param[NPARAM];	/* Parameters			*/
	char	a_nastyf;		/* Error flag			*/
} ANSI;

typedef struct {
	int		row;
	int		col;
	unsigned	flags;	/* Vt100 saves graphics rendition. Ugh! */
	char		curgl;
	char		curgr;
	char		gsets[4];
} SavedCursor;

#define	TEKNUMFONTS	5
/* Actually there are 5 types of lines, but four are non-solid lines */
#define	TEKNUMLINES	4

typedef struct {
	int	x;
	int	y;
	int	fontsize;
	int	linetype;
} Tmodes;

typedef struct {
	int Twidth;
	int Theight;
} T_fontsize;

typedef struct {
	Window tbar;			/* major window			*/
	Window left;			/* left hilited window		*/
	Window right;			/* right hilited window		*/
	int hilited;			/* in hilite state		*/
	int x;				/* x position of title		*/
	int y;				/* y position of title		*/
	int fullwidth;			/* full width of title		*/
	int width;			/* width of visible part of title */
} TitleBar;

typedef struct {
	short *bits;
	int x;
	int y;
	int width;
	int height;
} BitmapBits;

typedef struct {
/* These parameters apply to both windows */
	Display		*display;	/* X display for screen		*/
	int		respond;	/* socket for responses
					   (position report, etc.)	*/
	long		pid;		/* pid of process on far side   */
	int		uid;		/* user id of actual person	*/
	int		gid;		/* group id of actual person	*/
	int		color;		/* colors used			*/
	int		foreground;	/* foreground color		*/
	int		background;	/* Background color		*/
	int		cursorcolor;	/* Cursor color			*/
	int		mousecolor;	/* Mouse color			*/
	Pixmap		bgndtile;	/* background tile pixmap	*/
	int		border;		/* inner border			*/
	int 	    	borderwidth;	/* outer border	    	    	*/
	Pixmap		bordertile;	/* tile pixmap for border	*/
	Pixmap		graybordertile;	/* tile pixmap for border when
						window is unselected	*/
	Cursor		arrow;		/* arrow cursor			*/
	int		send_mouse_pos;	/* user wants mouse transition  */
					/* and position information	*/
	int		select;		/* xterm selected		*/
	struct timeval	*timeout;	/* timeout value for select	*/
	FontInfo	*titlefont;	/* font info for title font	*/
	int		titleheight;	/* height of title		*/
	int		title_n_size;	/* width on 'n' in title font	*/
	Pixmap		hilitetile;	/* tile pixmap for title hilite	*/
	int		autoraise;	/* auto raise window mode	*/
	Window		autowindow;	/* the window to autoraise	*/
	int		timer;		/* timer function		*/
	int		holdoff;	/* delay select and unselects	*/
	int		audiblebell;	/* audible bell mode		*/
	int		visualbell;	/* visual bell mode		*/
	int		visbelldelay;	/* visual bell delay		*/
	int		icon_show;	/* icon currently showing	*/
        int             textundericon;  /* text under icon              */
        int             bitmapwidth;    /* width of icon bitmap         */
        int             bitmapheight;   /* height of icon bitmap        */
        int             icon_text_x;    /* x position of text           */
        int             icon_text_y;    /* y position of text           */
	BitmapBits	iconbitmap;	/* bitmap for icon		*/
        int             iconinput;      /* got input while iconified    */
	int		active_icon;	/* icon is miniature copy	*/
	int		deiconwarp;	/* warp mouse on deiconify	*/
	int		logging;	/* logging mode			*/
	int		logfd;		/* file descriptor of log	*/
	char		*logfile;	/* log file name		*/
	char		*logstart;	/* current start of log buffer	*/
	int		inhibit;	/* flags for inhibiting changes	*/

/* VT window parameters */
	int		show;		/* window showing		*/
	int		iconunmap;	/* unmapped because of icon	*/
	struct {
		Window	window;		/* X window id			*/
		int	width;		/* width of columns		*/
		int	height;		/* height of rows		*/
		int	fullwidth;	/* full width of window		*/
		int	fullheight;	/* full height of window	*/
		int	f_width;	/* width of fonts in pixels	*/
		int	f_height;	/* height of fonts in pixels	*/
		int	titlebar;	/* title bar(s) showing		*/
	} fullVwin, iconVwin, *mappedVwin;
	Font		fnt_icon;	/* icon font			*/
	int		minrows;	/* minimun number of rows to
						accommodate scrollbar	*/
	Cursor		curs;		/* cursor resource from X	*/
	/* Terminal fonts must be of the same size and of fixed width */
	Font		fnt_norm;	/* normal font of terminal	*/
	Font		fnt_bold;	/* bold font of terminal	*/
	int		enbolden;	/* overstrike for bold font	*/
	Vertex		*box;		/* draw unselected cursor	*/

	int		cursor_state;	/* ON or OFF			*/
	int		cursor_set;	/* requested state		*/
	int		cursor_col;	/* previous cursor column	*/
	int		cursor_row;	/* previous cursor row		*/
	int		cur_col;	/* current cursor column	*/
	int		cur_row;	/* current cursor row		*/
	int		max_col;	/* rightmost column		*/
	int		max_row;	/* bottom row			*/
	int		top_marg;	/* top line of scrolling region */
	int		bot_marg;	/* bottom line of  "	    "	*/
	int		scrollbar;	/* if > 0, width of scrollbar, and
						scrollbar showing	*/
	int		topline;	/* line number of top, <= 0	*/
	int		savelines;	/* number of lines off top to save */
	int		scrollinput;	/* scroll to bottom on input	*/
	int		scrollkey;	/* scroll to bottom on key	*/
	
	ScrnBuf		buf;		/* screen buffer (main)		*/
	ScrnBuf		allbuf;		/* screen buffer (may include
					   lines scrolled off top	*/
	ScrnBuf		altbuf;		/* alternate screen buffer	*/
	int		alternate;	/* true if using alternate buf	*/
	ScrollBar	*sb;		/* pointer to scrollbar struct	*/
	int		do_wrap;	/* true if cursor in last column
					   and character just output    */
	int		incopy;		/* 0 if no RasterCopy exposure
					   event processed since last
					   RasterCopy			*/
	int		c132;		/* allow change to 132 columns	*/
	int		curses;		/* cludge-ups for more and vi	*/
	int		marginbell;	/* true if margin bell on	*/
	int		nmarginbell;	/* columns from right margin	*/
	int		bellarmed;	/* cursor below bell margin	*/
#ifdef CROCKSCROLL
	int		scrollincr;	/* scroll increment		*/
#endif
	int		multiscroll;	/* true if multi-scroll		*/
	int		scrolls;	/* outstanding scroll count	*/
	SavedCursor	sc;		/* data for restore cursor	*/
	TitleBar	title;		/* title bar			*/
	int		statusline;	/* status line showing		*/
	int		statusheight;	/* status line height		*/
	int		instatus;	/* cursor in status line	*/
	SavedCursor	statussc;	/* status line restore cursor	*/
	int		reversestatus;	/* status line reversed		*/
	char		*winname;	/* name of window (and icons)	*/
	int		winnamelen;	/* length of window name	*/
	int		save_modes[19];	/* save dec private modes	*/
	int		pagemode;	/* true if page mode		*/
	int		pagecnt;	/* count of lines in page mode	*/
	int		pageoverlap;	/* lines to overlap (less one)	*/

	/* Improved VT100 emulation stuff.				*/
	char		gsets[4];	/* G0 through G3.		*/
	char		curgl;		/* Current GL setting.		*/
	char		curgr;		/* Current GR setting.		*/
	char		curss;		/* Current single shift.	*/
	int		scroll_amt;	/* amount to scroll		*/
	int		refresh_amt;	/* amount to refresh		*/
	int		jumpscroll;	/* whether we should jumpscroll */

/* Tektronix window parameters */
	int		Tforeground;	/* foreground color		*/
	int		Tbackground;	/* Background color		*/
	int		Tcursorcolor;	/* Cursor color			*/
	Pixmap		Tbgndtile;	/* background tile pixmap	*/
	int		Tcolor;		/* colors used			*/
	int		planeused;	/* is xorplane being used	*/
	int		cellsused;	/* are color cells being used	*/
	Color		colorcells[3];	/* color cells for Tek		*/
	int		Tshow;		/* Tek window showing		*/
	int		Ticonunmap;	/* unmapped because of icon	*/
	int		waitrefresh;	/* postpone refresh		*/
	struct {
		Window	window;		/* X window id			*/
		int	width;		/* width of columns		*/
		int	height;		/* height of rows		*/
		int	fullwidth;	/* full width of window		*/
		int	fullheight;	/* full height of window	*/
		int	titlebar;	/* title bar(s) showing		*/
		double	tekscale;	/* scale factor Tek -> vs100	*/
	} fullTwin, iconTwin, *mappedTwin;
	BitmapBits	Ticonbitmap;	/* bitmap for icon		*/
	Vertex		**Tbox;		/* draw unselected cursor	*/
	int		xorplane;	/* z plane for inverts		*/
	Pattern		linepat[TEKNUMLINES]; /* line patterns		*/
	Font		Tfont[TEKNUMFONTS]; /* Tek fonts		*/
	int		tobaseline[TEKNUMFONTS]; /* top to baseline for
							each font	*/
	int		TekEmu;		/* true if Tektronix emulation	*/
	int		cur_X;		/* current x			*/
	int		cur_Y;		/* current y			*/
	Tmodes		cur;		/* current tek modes		*/
	Tmodes		page;		/* starting tek modes on page	*/
	int		margin;		/* 0 -> margin 1, 1 -> margin 2	*/
	int		pen;		/* current Tektronix pen 0=up, 1=dn */
	char		*TekGIN;	/* nonzero if Tektronix GIN mode*/
	TitleBar	Ttitle;		/* title bar			*/
	char		*Twinname;	/* name of window		*/
	int		Twinnamelen;	/* length of window name	*/
} Screen;

/* meaning of bits in screen.select flag */
#define	INWINDOW	01	/* the mouse is in one of the windows */
#define	FOCUS		02	/* one of the windows is the focus window */

typedef struct
{
	unsigned	offset;		/* status of shift, control, meta */
#define SHIFT	0x0001
#define META	0x0002
#define CONTROL	0x0004

	unsigned	flags;
} Keyboard;

/* define masks for flags */
#define CAPS_LOCK	0x01
#define KYPD_APL	0x02
#define CURSOR_APL	0x04


#define N_MARGINBELL	10
#define MAX_TABS	320
#define TAB_ARRAY_SIZE	10	/* number of ints to provide MAX_TABS bits */

typedef unsigned Tabs [TAB_ARRAY_SIZE];


#define BUF_SIZE 4096

typedef struct
{
	Keyboard	keyboard;	/* terminal keyboard		*/
	Screen		screen;		/* terminal screeen		*/
	unsigned	flags;		/* mode flags			*/
	unsigned	initflags;	/* initial mode flags		*/
	Tabs		tabs;		/* tabstops of the terminal	*/
} Terminal;


/* masks for terminal flags */

#define INVERSE		0x01	/* invert the characters to be output */
#define UNDERLINE	0x02	/* true if underlining */
#define BOLD		0x04
#define WRAPAROUND	0x08
#define REVERSE_VIDEO	0x10	/* true if screen white on black */
#define ORIGIN		0x20	/* true if in origin mode */
#define INSERT		0x40	/* true if in insert mode */
#define SMOOTHSCROLL	0x80	/* true if in smooth scroll mode */
#define AUTOREPEAT	0x100	/* true if in autorepeat mode */
#define IN132COLUMNS	0x200	/* true if in 132 column mode */
#define LINEFEED	0x400
#define	REVERSEWRAP	0x800	/* true if reverse wraparound mode */
#define ICONINPUT	0x1000	/* true if mini icon accepts kbd input */

#define	ATTRIBUTES	0x07	/* attributes mask */
#define CHAR		0177

#define VWindow(screen)		(screen->mappedVwin->window)
#define TWindow(screen)		(screen->mappedTwin->window)
#define Width(screen)		(screen->mappedVwin->width)
#define Height(screen)		(screen->mappedVwin->height)
#define FullWidth(screen)	(screen->mappedVwin->fullwidth)
#define FullHeight(screen)	(screen->mappedVwin->fullheight)
#define FontWidth(screen)	(screen->mappedVwin->f_width)
#define FontHeight(screen)	(screen->mappedVwin->f_height)
#define TWidth(screen)		(screen->mappedTwin->width)
#define THeight(screen)		(screen->mappedTwin->height)
#define TFullWidth(screen)	(screen->mappedTwin->fullwidth)
#define TFullHeight(screen)	(screen->mappedTwin->fullheight)
#define TekScale(screen)	(screen->mappedTwin->tekscale)
#define Titlebar(screen)	(screen->mappedVwin->titlebar)
#define TTitlebar(screen)	(screen->mappedTwin->titlebar)
#define ActiveIcon(screen)	(screen->active_icon && \
				(screen->mappedVwin == &screen->iconVwin))
#define TActiveIcon(screen)	(screen->active_icon && \
				(screen->mappedTwin == &screen->iconTwin))

#define CursorX(screen,col) ((col) * FontWidth(screen) + screen->border)
#define CursorY(screen,row) ((screen->instatus ? \
			((row) * FontHeight(screen) + 1)\
			: (((row) - screen->topline) * FontHeight(screen))) +\
			screen->border + Titlebar(screen))

#define TICONWINDOWEVENTS  (ExposeWindow | ButtonPressed)

#define	ICONWINDOWEVENTS   (TICONWINDOWEVENTS | ExposeRegion | ExposeCopy)
				 
#define ICONINPUTEVENTS	(KeyPressed | EnterWindow | LeaveWindow | FocusChange)

#define	TWINDOWEVENTS	(KeyPressed | ExposeWindow | ButtonPressed |\
			 ButtonReleased | UnmapWindow | EnterWindow |\
			 LeaveWindow | FocusChange)

#define	WINDOWEVENTS	(TWINDOWEVENTS | ExposeRegion | ExposeCopy)

#define TEK_LINK_BLOCK_SIZE 1024

typedef struct Tek_Link
{
	struct Tek_Link	*next;	/* pointer to next TekLink in list
				   NULL <=> this is last TekLink */
	short count;
	char *ptr;
	char data [TEK_LINK_BLOCK_SIZE];
} TekLink;

/* flags for cursors */
#define	OFF		0
#define	ON		1
#define	CLEAR		0
#define	TOGGLE		1

/* flags for color */
#define	C_FOREGROUND	0x01
#define	C_BACKGROUND	0x02
#define	C_FBMASK	0x03
#define	C_CURSOR	0x04
#define	C_MOUSE		0x08
#define	C_BORDER	0x10

/* flags for inhibit */
#define	I_LOG		0x01
#define	I_SIGNAL	0x02
#define	I_TEK		0x04

extern Bitmap make_icon();
extern Cursor make_tcross();
extern Cursor make_xterm();
extern Cursor make_wait();
extern Cursor make_arrow();
