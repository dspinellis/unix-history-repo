/* @(#)graphics1.c	1.3	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *
 * This file contains primitive functions to manipulate an AED512
 * color display.
 *
 * (Modified from software written by John Ousterhout for the caesar
 *  program)
 */
#include "gremlin.h"
#include <sgtty.h>

/* The following variables are used to hold state that we keep around
 * between procedure calls in order to reduce the amount of information
 * that must be shipped to the terminal.
 */

static char dbuf[BUFSIZ];	/* Used to buffer display characters */
static struct sgttyb sgttyb;	/* Used to save terminal control bits */
static int sgflags;		/* Used to save flags from sgttyb */
static char sgispeed, sgospeed;	/* Used to save baud rates */
static int localmode;		/* Used to save terminal local mode word */
static curcharsize;		/* Current character size */
static int wmask;		/* Current write mask value */


/* The following strings are used to as the colors for drawing and
 * erasing.  They permit layer inversion, such that the drawing of
 * the layer causes zeroes to be written and erasing causes ones to
 * be written.
 */

static char draw[4], erase[4];

/* The following arrays are used to define the line styles we use for
 * for drawing.  Color and stipple are used as a representation of
 * styles on the AED but are not necessarily accurate with the final
 * output.
 */

static int stylecolor[11] = { 0, 041, 041, 043, 041, 041, 042, 
                              044, 044, 0200, 0100 };
static int stipple[11]  = { 255, 136, 228, 255, 240, 255, 255, 
                            255, 255, 136, 255 };

/* The following table contains the color lookup used to represent
 * different fonts on the AED.
 */

static int fontmap[8] = { 0, 041, 044, 045, 046, 0, 0, 0 };

/* The following variables are made available to the outside world. */

int GrXMax = 511;		/* Maximum x-coordinate of screen */
int GrYMax = 482;		/* Maximum y-coordinate of screen */
FILE *display;			/* The file for the AED512 */
int charxsize;			/* Character x dimension */
int charysize;			/* Character y dimension */
int descenders;			/* Character descender length */
int curx, cury;			/* Current access position */
int rmask;                      /* read mask */

/* The following table is used to convert numbers to hex.  We cannot use
 * standard C library conversion because it generates lower case letters
 * which are bad news to the AED512.
 */

static char hex[] = "0123456789ABCDEF";


#ifndef FASTIO
GRchex(val, string, nchars)
int val;			/* Integer value to be converted. */
char *string;			/* Pointer to string area to be used for
				 * converted result.
				 */
int nchars;			/* Number of characters to be converted. */

/*---------------------------------------------------------
 *	This is a routine that converts an integer to a string
 *	of hexadecimal characters.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The string contains the value of the low-order nchars 4-bit chunks
 *	of val, as represented in hexadecimal.  String is zero-filled.
 *---------------------------------------------------------
 */

{
    string = &(string[nchars]);
    *string = '\0';
    for (; nchars>0 ; nchars--)
    {
	*(--string) = hex[val & 017];
	val >>= 4;
    }
}
#endif

GRsetwmask(mask)
int mask;			/* New value for write mask */

/*---------------------------------------------------------
 *	This is a routine that resets the value of the current
 *	write mask, if necessary.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	If wmask is different from mask, then the new mask is output to
 *	the display and stored in wmask.
 *
 *	Errors:		None.
 *---------------------------------------------------------
 */

{
    char s[4];
    wmask = mask;
#ifndef FASTIO
    GRchex(wmask, s, 2);
    fprintf(display, "L%s", s);
#else
    putc('L', display);
    putc(mask&0377, display);
#endif
}

GRsetcolor(color)
int color;
/* 
 *      This routine sets the current color for the graphics display
 */

{

#ifndef FASTIO
	char s1[3];

	GRchex(color, s1, 2);
	fprintf(display, "C%s",s1);
#else
	fprintf(display,"C%c", color&0377);
#endif

}  /* end setcolor */


GRoutxy20(x, y)
int x,y;			/* The coordinates to be output */

/*---------------------------------------------------------
 *	This routine outputs an x-y coordinate pair in the standard
 *	format required by the AED display.
 *
 *	Results:	None.
 *	
 *	Side Effects:
 *	Characters are output to the AED512 in the standard way required
 *	for values indicated by "xy20" in the user manual.
 *
 *	Errors:		None.
 *
 * (Modified from software written by John Ousterhout for the caesar
 *  program)
 *---------------------------------------------------------
 */

{
#ifndef FASTIO
    char s1[4], s2[4], s3[4];
    GRchex(((y>>8)&03) | ((x>>6)&014), s1, 1);
    GRchex(x&0377, s2, 2);
    GRchex(y&0377, s3, 2);
    fprintf(display, "%s%s%s", s1, s2, s3);
#else
    putc(((x>>4)&020)+((y>>8)&01), display);
    putc(x&0377, display);
    putc(y&0377, display);
#endif
}

GRsetpos(x, y)
int x, y;			/* Screen coordinates.

/*---------------------------------------------------------
 *	This routine sets the current access position, if necessary.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	If x and y are equal to curx and cury, respectively, then nothing
 *	happens.  Otherwise, x and y are stored into curx and cury and the
 *	current access position of the AED is set to those coordinates.
 *
 *	Errors:		None.
 *---------------------------------------------------------
 */

{
    if (x==curx && y==cury) return;
    curx = x;
    cury = y;
    putc('Q', display);
    GRoutxy20(x, y);
}


GRsetcharstyle(style)
int style;			/* New font. */

/*---------------------------------------------------------
 *	This routine sets the current character style.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	A new character style is output to the display.
 *---------------------------------------------------------
 */

{
    GRsetcolor(fontmap[style]);
}

GRsetlinestyle(style)
int style;			/* New stipple pattern for lines. */

/*---------------------------------------------------------
 *	This routine sets the current line style.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	A new line style is output to the display.
 *---------------------------------------------------------
 */

{
    char s[4];

    GRsetcolor(stylecolor[style]);

#ifndef FASTIO
    GRchex(stipple[style], s, 2);
    fprintf(display, "1%sFF", s);
#else
    putc('1', display);
    putc(stipple[style]&0377, display);
    putc(0377, display);
#endif
}


GRsetcharsize(size)
int size;			/* character size (1 - 4) */

/*---------------------------------------------------------
 *	This routine sets the character size in the display,
 *	if necessary.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	If the current display character size isn't already equal to size,
 *	then it is made so.
 *---------------------------------------------------------
 */

{
    if (size == curcharsize) return;
    curcharsize = size;
#ifndef FASTIO
    if (curcharsize == 4)
    {
        fputs("^270F18L",display);
	charxsize = 15;
	charysize = 24;
        descenders = 6;
    }
    else if (curcharsize == 3)
         {
             fputs("^250A0EL",display);
             charxsize = 10;
             charysize = 14;
             descenders = 2;
         }
         else if (curcharsize == 2) 
              {
	          fputs("^17070CL", display);
	          charxsize = 8;
	          charysize = 12;
                  descenders = 3;
              }
              else 
              {
	          fputs("^15050BL", display);
	          charxsize = 6;
	          charysize = 7;
                  descenders = 1;
               };
#else
    if (curcharsize == 4)
    {
        fputs("^27\17\30L",display);
	charxsize = 15;
	charysize = 24;
        descenders = 6;
    }
    else if (curcharsize == 3)
         {
             fputs("^25\12\16L",display);
             charxsize = 10;
             charysize = 14;
             descenders = 2;
         }
         else if (curcharsize == 2) 
              {
	          fputs("^17\10\14L", display);
	          charxsize = 8;
	          charysize = 12;
                  descenders = 3;
              }
              else 
              {
	          fputs("^15\6\7L", display);
	          charxsize = 6;
	          charysize = 7;
                  descenders = 1;
               };
#endif
}


GRInit(stream, invert)
FILE *stream;			/* A pointer to the graphics display
				 * file descriptor.  The file must have
				 * been opened by the caller.
				 */
int invert;			/* An integer whose low-order eight bits
				 * are ones iff the corresponding layers
				 * are to be inverted (drawing means write
				 * zeroes and erasing means write ones).
				 */

/*---------------------------------------------------------
 *	GRInit initializes the graphics display and clears its screen.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The display is re-initialized and the file is remembered for
 *	use in all subsequent calls to this module.  The display's
 *	color map is reset.  The display is put into raw mode, but
 *	the previous mode bits are saved.
 *
 *	Errors:		None.
 *---------------------------------------------------------
 */

{
#ifdef FASTIO
    static int litout = LLITOUT;
#endif
    static int ldisc = NTTYDISC;

    /* First, grab up the display modes, then reset them to put it
     * into cooked mode.  Also, lock the terminal.  If doing fast I/O
     * then set the LLITOUT bit.  Note:  setting the LLITOUT bit only
     * works if it happens before the stty.  Also forces the display to
     * run at 9600 baud.
     */

    (void) ioctl(fileno(stream), TIOCSETD, (char *) &ldisc);
    (void) ioctl(fileno(stream), TIOCLGET, (char *) &localmode);
#ifdef FASTIO
    (void) ioctl(fileno(stream), TIOCLBIS, (char *) &litout);
#endif
    (void) gtty(fileno(stream), &sgttyb);
    sgflags = sgttyb.sg_flags;
    sgispeed = sgttyb.sg_ispeed;
    sgospeed = sgttyb.sg_ospeed;
    sgttyb.sg_flags = (sgttyb.sg_flags &
	~(RAW | CBREAK | ECHO | LCASE)) | EVENP | ODDP | CRMOD;
    sgttyb.sg_ispeed = B9600;
    sgttyb.sg_ospeed = B9600;
    (void) stty(fileno(stream), &sgttyb);
    (void) ioctl(fileno(stream), TIOCEXCL, (char *) &sgttyb);

    /* Save the file pointer around for later use, then output an
     * initialization string to the display.  The initialization
     * string resets the terminal, sets formats, clears the display,
     * and initializes the read and write masks.
     */
    display = stream;
    setbuf(display, dbuf);
#ifndef FASTIO
    GRchex(invert&0377, erase, 2);
    GRchex((~invert)&0377, draw, 2);
    fputs("\33\60", display);
    (void) fflush(display);
    (void) system("sleep 1");
    fprintf(display, "\33\33G1HDHN[%sLFF\14\33C%sM7FFFFFFF", erase, draw);
    fprintf(display, "c404022]+00002019001F02828");
#else
    *erase = invert&0377;
    *draw = (~invert)&0377;
    fputs("\33\60", display);
    (void) fflush(display);
    (void) system("sleep 1");
    fputs("\33\33G18D8N[", display);
    putc(*erase, display);
    fputs("L\377\14\33C", display);
    putc(*draw, display);
    fputs("M\177\377\377\377c\100\100\42]+", display);
    putc('\0', display);
    putc('\0', display);
    fputs("2\01\220\01\360\50\50", display);
#endif
    putc('3', display);			/* Make sure the crosshair is off */
    putc('\0', display);
    curx = -1;
    cury = -1;
    curcharsize = -1;
    wmask = 0177;
    rmask = 0177;
    (void) fflush(display);
}


GRClose()

/*---------------------------------------------------------
 *	GRClose does whatever is necessary to reset the characteristics
 *	of the AED512 after the program is finished.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The graphics display modes are reset.
 *---------------------------------------------------------
 */

{
    sgttyb.sg_flags = sgflags;
    (void) stty(fileno(display), &sgttyb);
    (void) ioctl(fileno(display), TIOCNXCL, (char *) &sgttyb);
    (void) ioctl(fileno(display), TIOCLSET, (char *) &localmode);
}


GRSetMap(pmap)
char *pmap;			/* A pointer to 256*3 bytes containing the
				 * new values for the color map.  The first
				 * three values are red, green, and blue
				 * intensities for color 0, and so on.
				 */

/*---------------------------------------------------------
 *	GrSetMap outputs new values to the AED512 color map.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	The values in the color map are set from the array indicated
 *	by pmap.  The back1 and back2 strings are set so that the
 *	routines GrBack1 and GrBack2 will switch the background color
 *	to color 0 and color 256,respectively.
 *
 *	Errors:		None.
 *
 * (Modified from software written by John Ousterhout for the caesar
 *  program)
 *---------------------------------------------------------
 */

{
    char s[4], *p;
    int i;
     
    p = pmap;
#ifndef FASTIO
    fputs("K0000", display);
    for (i = 0; i<256*3; ++i)
    {
	GRchex(*p++&0377, s, 2);
	fprintf(display, "%s", s);
    }
#else
    putc('K', display);
    putc('\0', display);
    putc('\0', display);
    for (i = 0; i<256*3; ++i)
    {
	putc(*p++&0377, display);
    }
#endif
    (void) fflush(display);

}

