/* Font description file producer:  David Slattengren
 * Taken from vfontinfo by Andy Hertzfeld  4/79
 *
 *	Use:  mkfnt [-s] [-p#] [-r#] [-ddirectory] font
 *
 *	Mkfnt takes the font named "font" and produces a ditroff description
 *	file from it.  The -s option tells mkfnt that this is a special font,
 *	and sould substitute special character names for the normal ones.  The
 *	-p# option tells what point size the DESC file has as it's "unitwidth"
 *	argument (default: 36).  The -r# option is the resolution of the device
 *	(default: 200, in units/inch).  The -d option tells where to look for
 *	fonts (default: /usr/lib/vfont).
 */

/*
 *  Here's an ascii character set, just in case you need it:

     | 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|
     | 08 bs | 09 ht | 0a nl | 0b vt | 0c np | 0d cr | 0e so | 0f si |
     | 10 dle| 11 dc1| 12 dc2| 13 dc3| 14 dc4| 15 nak| 16 syn| 17 etb|
     | 18 can| 19 em | 1a sub| 1b esc| 1c fs | 1d gs | 1e rs | 1f us |
     | 20 sp | 21  ! | 22  " | 23  # | 24  $ | 25  % | 26  & | 27  ' |
     | 28  ( | 29  ) | 2a  * | 2b  + | 2c  , | 2d  - | 2e  . | 2f  / |
     | 30  0 | 31  1 | 32  2 | 33  3 | 34  4 | 35  5 | 36  6 | 37  7 |
     | 38  8 | 39  9 | 3a  : | 3b  ; | 3c  < | 3d  = | 3e  > | 3f  ? |
     | 40  @ | 41  A | 42  B | 43  C | 44  D | 45  E | 46  F | 47  G |
     | 48  H | 49  I | 4a  J | 4b  K | 4c  L | 4d  M | 4e  N | 4f  O |
     | 50  P | 51  Q | 52  R | 53  S | 54  T | 55  U | 56  V | 57  W |
     | 58  X | 59  Y | 5a  Z | 5b  [ | 5c  \ | 5d  ] | 5e  ^ | 5f  _ |
     | 60  ` | 61  a | 62  b | 63  c | 64  d | 65  e | 66  f | 67  g |
     | 68  h | 69  i | 6a  j | 6b  k | 6c  l | 6d  m | 6e  n | 6f  o |
     | 70  p | 71  q | 72  r | 73  s | 74  t | 75  u | 76  v | 77  w |
     | 78  x | 79  y | 7a  z | 7b  { | 7c  | | 7d  } | 7e  ~ | 7f del|

 *
 */

#include <stdio.h>
#include <ctype.h>
#include <vfont.h>

char 	sccsid[] = "@(#)makevfont.c	1.1	(Berkeley)	%G%";

#define MAGICN		0436	/* font file magic number */
#define PCNTUP		62	/* percent of maximum height for an ascender */
#define PCNTDOWN	73	/* percent of maximum droop for a descender */
#define FONTDIR		"/usr/lib/vfont"
#define IMDIR		"."
#define POINTSIZE	36	/* this is the "unitwidth" point size */
#define MINSIZE		6	/* the minimum and maximum point size values */
#define MAXSIZE		36	/*    acceptible for use as "unitwidth"s */
#define RESOLUTION	200	/* resolution of versatec (dots/inch) */
#define MINRES		10	/* check up on resolution input by setting */
#define MAXRES		100000	/*    absurdly out-of-range limits on them */
#define APOINT		72	/* 1/APOINT inches = 1 point */

struct header	FontHeader;
struct dispatch	disptable[256];

int	res = RESOLUTION;	/* resolution of the device (units/inch) */
int	pointsize = POINTSIZE;	/* point size being used for unitwidth */
int	psize;			/* point size of font actually used */
int	psizelist[] = { 36, 28, 24, 22, 20, 18, 16,
			14, 12, 11, 10, 9, 8, 7, 6, 0 };

char	*fontdir = FONTDIR;	/* place to look for fonts */
char	IName[100];		/* input file name put here */
char	*rdchar ();		/* function makes strings for ascii */
int	FID = -1;		/* input file number */

int	maxdown = 0;		/* size of the most "droopy" character */
int	maxup = 0;		/* size of the tallest character */
int	type;			/* 1, 2, or 3 for type of ascend/descending */
int	specialf = 0;		/* flag "is this a special font?";  used to
				   determine which mapping array to look in. */

				/* following are the character maps for */
				/* ascii code-conversion to printables... */
char	**charmap;
char *vregular[] = {

	"", "fi", "fl", "ff", "\\-", "ru", "em", "bu", "sq", "Fi", "Fl", "de",
	"dg", "fm", "co", "rg", "ct", "14", "12", "34", "^T", "^U", "^V", "^W",
	"^X", "^Y", "^Z", "^[", "^\\", "^]", "^^", "^_", " ", "!", "\"", "#",
	"$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1",
	"2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?",
	"@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
	"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[",
	"\\", "]", "^", "_", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i",
	"j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
	"x", "y", "z", "{", "|", "}", "~"

};
char *vspecial[] = {

	"", "if", "ip", "pt", "rh", "cu", "rn", "bs", "+-", "<=", ">=", "sr",
	"ts", "is", "sl", "bv", "lf", "rf", "lc", "rc", "lt", "lb", "rt", "rb",
	"lk", "rk", "sb", "sp", "ca", "no", "lh", "mo", " ", "!", "\"", "#",
	"$", "%", "&", "aa", "gr", ")", "mu", "pl", ",", "mi", ".", "di", "==",
	"~=", "ap", "!=", "<-", "->", "ua", "da", "sc", "**", ":", ";", "<",
	"eq", ">", "?", "@", "*A", "*B", "*G", "*D", "*E", "*Z", "*Y", "*H",
	"*I", "*K", "*L", "*M", "*N", "*C", "*O", "*P", "*R", "*S", "*T", "*U",
	"*F", "*X", "*Q", "*W", "dd", "br", "ib", "\\", "ci", "^", "ul", "ga",
	"*a", "*b", "*g", "*d", "*e", "*z", "*y", "*h", "*i", "*k", "*l", "*m",
	"*n", "*c", "*o", "*p", "*r", "*s", "*t", "*u", "es", "*x", "*q", "*w",
	"pd", "*f", "{", "|", "}", "~"
};


main (argc, argv)
int argc;
char **argv;
{
    register i, j;

    while (*(*(++argv)) == '-') {		/* do options... */
	switch (*(++(*argv))) {

	  case 's': specialf = 1;		/* special font */
		    break;
	    
	  case 'd': fontdir = ++*argv;		/* directory */
		    break;

	  case 'p': pointsize = atoi(++*argv);	/* point size */
		    if (pointsize < MINSIZE || pointsize > MAXSIZE) {
			fprintf(stderr, "Illegal point size: %d\n", pointsize);
			exit(1);
		    }
		    break;

	  case 'r': res = atoi(++*argv);	/* resolution */
		    if (res < MINRES || res > MAXRES) {
			fprintf(stderr, "Illegal resolution: %d\n", res);
			exit(1);
		    }
		    break;

	   default: fprintf(stderr, "Bad option: %c", **argv);
		    exit(1);
	}
    }
			/* set character map */
    charmap = specialf ? vspecial : vregular;

							/* open font file */
    for (i = 0; FID < 0 && (psize = psizelist[i]) > 0; i++) {
	sprintf (IName, "%s/%s.%d", fontdir, *argv, psize);
	FID = open (IName, 0);
    }
    if (FID < 0) { 
	printf ("Can't find %s\n", *argv);
	exit (8); 
    }


    if (read (FID, &FontHeader, sizeof FontHeader) != sizeof FontHeader)
	error ("Bad header in Font file.");
    if (read (FID, &disptable[0], sizeof disptable) != sizeof disptable)
	error ("Bad dispatch table in Font file");
    if (FontHeader.magic != MAGICN)
	printf ("Magic number %o wrong\n", FontHeader.magic);


    printf ("# Font %s, ", IName);			/* head off the file */
    printf ("max width %d, max height %d\n",
		FontHeader.maxx, FontHeader.maxy);
    printf ("name %s\ninternalname #\n", *argv);
    if (specialf) {
	printf ("special\n");
    } else {
	printf ("ligatures ff fl fi ffl ffi 0\n");
    }
    printf ("# char	width	u/d	octal\ncharset\n");
    printf ("\\|	%4d	 0	0\n\\^	%4d	 0	0\n",
		(res * pointsize / APOINT + 4) / 6,
		(res * pointsize / APOINT + 7) / 12);

    for (j=0; j<256; j++) {
	if (disptable[j].nbytes != 0) {
	    if (disptable[j].up > maxup) maxup = disptable[j].up;
	    if (disptable[j].down > maxdown) maxdown = disptable[j].down;
	}
    }
    if (maxdown == 0) maxdown = 1;

/*******************************************************************************

	`type' is used to determine overhangs (up/down) from percentage of
	the maximum heights and dips.  Ascenders are higher than PCNTUP%
	of the highest, as descenders are more than PCNTDOWN%.
	widths [i = f(width)] are calculated from the definition point
	size (pointsize) and the one from this font (psize).

*******************************************************************************/

    for (j=0; j<256; j++) {
	if (disptable[j].nbytes != 0) {
	    type = (int) (((disptable[j].up * 100) / maxup) > PCNTUP) * 2 |
		    (int) (((disptable[j].down * 100) / maxdown) > PCNTDOWN);
	    i = ((pointsize * disptable[j].width) + psize / 2) / psize;
	    printf ("%s	%4d	 %d	0%o\n", charmap[j], i, type, j);
	}
    }
}

error(string)
char *string;

{ 
    printf("\nmakefont: %s\n",string);
    exit(8);
}
