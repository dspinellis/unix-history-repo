/*	fed.h	4.1	83/03/09	*/
/*
 * fed.h: global definitions for font editor.
 */

#include <stdio.h>
#include <ctype.h>
#include <vfont.h>
#include <signal.h>
#include <setjmp.h>

/* current windows - what's on the screen */

#define SCRHI	360		/* number of pixels on the screen */
#define SCRWID	720		/* width in pixels of the screen */
#define NROW	3		/* number of rows of glyphs on the screen */
#define NCOL	7		/* number of cols of glyphs in a row */
#define NWIND	(NROW*NCOL)	/* number of windows */
#define GLCOL	100		/* width of a glyph window */
#define GLROW	100		/* height of a glyph window */
#define GLPAD	3		/* number of pixels between windows */
#define WINDSIZE (((GLCOL+7)>>3)*GLROW)	/* size in bytes of a window */
#define BASELINE 22		/* number of pixels below baseline in window */
#define SLOPE	(3.5)		/* Amount to slant italic vertical line */
				/* equal to about 15.94 degrees */
				/* for 5 degree caligraphy slant use 11.43 */

#define ESC	'\033'	/* The ASCII escape character */

#define abs(x)		((x) <  0  ? (-(x)) : (x))
#define max(x,y)	((x) > (y) ?   (x)  : (y))
#define min(x,y)	((x) < (y) ?   (x)  : (y))

typedef char *bitmat;

int	changes;	/* nonzero if changes since last write */
char	curchar;	/* current character being edited */
int	curcurs;	/* 1 if cursor is on now */
int	currb;		/* 1 if rubber band lie is on now */
int	curs_r, curs_c;	/* position in current window of graphics cursor */
int	curwind;	/* current open window number */
int	curzoom;	/* 1 to 9 - current zoom level of screen */
int	editing;	/* in file I/O command, true if editing font */
jmp_buf	env;
long	fbase;		/* first loc in font file of bits */
FILE *	fontdes;	/* open for reading, current font */
char	fontfile[100];	/* name of the font file */
int	hpensize;	/* size of heavy pen in pixels diameter of dot */
char	msgbuf[100];	/* scratch space to sprintf into for messages */
int	nextwind;	/* the next free window to grab */
int	oldzoom;	/* the value of curzoom before a message */
int	pen_r, pen_c;	/* row/col in current glyph of logical pen */
int	pencolor;	/* 0=erase, 1=draw */
int	penweight;	/* 0=fine, 1=heavy */
int	pointsize;	/* point size of current font */
int	QUIET;		/* true if -q flag */
char	stoutbuf[BUFSIZ];	/* for speed */
FILE	*trace;		/* for debugging output */
char	tracebuf[BUFSIZ];	/* for speed */
int	und_p_r, und_p_c;	/* pen_r, pen_c for undo */
int	und_c_r, und_c_c;	/* curs_r, curs_c for undo */

struct	header		FontHeader;
struct	dispatch	disptable[256];

struct	cwind	{
	bitmat	val;		/* what we are making it into */
	bitmat	onscreen;	/* what's currently on the screen */
	bitmat	undval;		/* the previous version */
	char	used;		/* the character using this window */
} wind[NROW * NCOL];

struct cht {
	int	wherewind;	/* >=0: window # on screen, -1: in file, -2: use whereat */
	bitmat	whereat;	/* where it can be found */
	int	nrow, ncol;	/* size of char */
	int	rcent, ccent;	/* bit location of lower left corner of main part of char */
} cht[256];

struct place {
	int	c, r;
} base[NROW * NCOL];		/* lower left corner of each window */

char	penmat[10][10];		/* 0 or 1 as the pen is.  5,5 is center */
float	sqrtmat[10][10];	/* table of sqrt(i**2+j**2) for speed */

char	*rdchar();
char	esccmd();
bitmat	newmat();
bitmat	findbits();
int	onsig();
int	onintr();
float	sqrt();

int	matcnt[10];
