#include <X/mit-copyright.h>

/*	Copyright	Massachusetts Institute of Technology	1984, 1985 */

/* ptyx.h */

#define MAX_COLS	200
#define MAX_ROWS	128

/*
 * The origin of a screen is 0, 0.  Therefore, the number of rows
 * on a screen is screen->max_row + 1, and similarly for columns.
 */

typedef short **ScrnBuf;

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

#define	NPARAM	10			/* Max. parameters		*/

typedef struct {
	unsigned char	a_type;
	unsigned char	a_pintro;
	unsigned char	a_final;
	unsigned char	a_inters;
	char	a_nparam;		/* # of parameters		*/
	char	a_dflt[NPARAM];		/* Default value flags		*/
	short	a_param[NPARAM];	/* Parameters			*/
	char	a_nastyf;		/* Error flag			*/
}	ANSI;

typedef struct {
	int		row;
	int		col;
	unsigned	flags;	/* Vt100 saves graphics rendition. Ugh! */
} SavedCursor;

typedef struct {
	Window		window;		/* X window for screen		*/
	Window		iconwindow;	/* window for icon		*/
	Bitmap		iconmask;	/* mask for icon outline	*/
	int		border;		/* inner border			*/
	int 	    	borderwidth;	/* outer border	    	    	*/
	int		width;		/* width of window - borders (pixels)*/
	int		height;		/* height of window -borders (pixels)*/
	Display		*display;	/* X display for screen		*/
	int		respond;	/* socket for responses
					   (position report, etc.)	*/

	/* Terminal fonts must be of the same size and of fixed width */
	Font		fnt_norm;	/* normal font of terminal	*/
	Font		fnt_bold;	/* bold font of terminal	*/
	int		f_width;	/* width of fonts in pixels	*/
	int		f_height;	/* height of fonts in pixels	*/

	int		cur_col;	/* current cursor column	*/
	int		cur_row;	/* current cursor row		*/
	int		max_col;	/* rightmost column		*/
	int		max_row;	/* bottom row			*/
	int		top_marg;	/* top line of scrolling region */
	int		bot_marg;	/* bottom line of  "	    "	*/
	
	ScrnBuf		buf;		/* screen buffer		*/
	Bitmap		cursor;		/* cursor bits			*/
	Bitmap		mask;		/* mask cursor bits		*/
	Cursor		curs;		/* cursor resource from X	*/
	Cursor		rcurs;		/* reverse version of cursor	*/
	int		foreground;	/* foreground color		*/
	int		background;	/* Background color		*/
	int		cursorcolor;	/* Cursor color			*/
	int		mousecolor;	/* Mouse color			*/
	Pixmap		bordertile;	/* tile pixmap for border	*/
	Pixmap		bgndtile;	/* background tile pixmap	*/
	unsigned	xorplane;	/* z plane for inverts		*/
	unsigned short	do_wrap;	/* true if cursor in last column
					   and character just output    */
	short	incopy;			/* 0 if no RasterCopy exposure
					   event processed since last
					   RasterCopy			*/
#ifdef CROCKSCROLL
	short		scrollincr;	/* scroll increment		*/
#endif
	unsigned short	multiscroll;	/* true if multi-scroll		*/
	int		scrolls;	/* outstanding scroll count	*/
	SavedCursor	sc;		/* data for restore cursor	*/
	long		pid;		/* pid of process on far side   */

	/* Improved VT100 emulation stuff.				*/
	ANSI		ansi;		/* ANSI parsing variables.	*/
	char		gsets[4];	/* G0 through G3.		*/
	char		curgl;		/* Current GL setting.		*/
	char		curgr;		/* Current GR setting.		*/
	char		curss;		/* Current single shift.	*/
	char		rx8bit;		/* TRUE if Rx is 8 bit.		*/
	char		tx8bit;		/* TRUE if Tx is 8 bit.		*/
	/* Tektronix plotting information */
	int		TekEmu;		/* true if Tektronix emulation	*/
	int		cur_X;		/* current screen x		*/
	int		cur_Y;		/* current screen y		*/
	int		cur_x;		/* current Tektronix x		*/
	int		cur_y;		/* current Tektronix y		*/
	int		pen;		/* current Tektronix pen 0=up, 1=dn */
	double		TekScale;	/* scale factor Tek -> vs100	*/
	int		TekGMode;	/* plot mode switch		*/
	int		TekAMode;	/* alpha after plot mode switch */
	int		TekIMode;	/* incremental plot mode switch */
	int		TekPMode;	/* point plot mode switch	*/
#ifdef JUMPSCROLL
	int		scroll_amt;	/* amount to scroll		*/
	int		refresh_amt;	/* amount to refresh		*/
	int		jumpscroll;	/* whether we should jumpscroll */
#endif JUMPSCROLL
	int		(*mode)();	/* this will be THE mode	*/
	unsigned short	send_mouse_pos;	/* user wants mouse transition  */
					/* and position information	*/
} Screen;

/* meaning of bits for entries in screen buffer */
#define CHAR		0177
#define BOLDbit		0200
#define INVERSEbit	0400


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


#define MAX_TABS	320
#define TAB_ARRAY_SIZE	10	/* number of ints to provide MAX_TABS bits */

typedef unsigned Tabs [TAB_ARRAY_SIZE];


#define BUF_SIZE 4096

typedef struct
{
	int		cnt;		/* count of char's left in buf	*/
	unsigned char	*ptr;		/* pointer to next char in buf	*/
	unsigned char	buf[BUF_SIZE];	/* buffer			*/
	int		fildes;		/* file descriptor to read from	*/
} Buffer;


typedef struct
{
	Keyboard	keyboard;	/* terminal keyboard		*/
	Screen		screen;		/* terminal screeen		*/
	Buffer		buf;		/* buffer for data to process	*/
	unsigned	flags;		/* mode flags			*/
	Tabs		tabs;		/* tabstops of the terminal	*/
} Terminal;


/* masks for terminal flags */

#define INVERSE		0x01	/* invert the characters to be output */
#define LINEFEED	0x02
#define BOLD		0x04
#define WRAPAROUND	0x08
#define REVERSE_VIDEO	0x10	/* true if screen white on black */
#define ORIGIN		0x20	/* true if in origin mode */
#define INSERT		0x40	/* true if in insert mode */
#define SMOOTHSCROLL	0x80	/* true if in smooth scroll mode */

	/* must be a power of two */
#define TEK_LINK_BLOCK_SIZE 1024

typedef struct _TekLink
{
	struct _TekLink	*next;	/* pointer to next TekLink in list
				   NULL <=> this is last TekLink */
	unsigned char	data [TEK_LINK_BLOCK_SIZE];
} TekLink;

#define TekBufPut(chr)					   	\
	/* if out of memory malloc some more */		   	\
	{							\
		if (tb_end & TEK_LINK_BLOCK_SIZE)		\
			TekBufExtend ();			\
		tb_end_link->data [tb_end++] = chr;		\
	}


#define CursorX(screen) (screen->cur_col * screen->f_width + screen->border)
#define CursorY(screen) (screen->cur_row * screen->f_height + screen->border)

/*
 * To handle Tektronix plotting well, add graphics cursor position
 * to get correct position for characters on the screen.
 * screen->border is taken into account in screen->cur[XY]
 */

#define TCursorX(screen) (screen->cur_X)
#define TCursorY(screen) (screen->cur_Y - screen->f_height)

int	ANSInormal();
int	ANSIparse();
int	ANSIstring();
