/* Font description file producer:  David Slattengren
 * Taken from vfontinfo by Andy Hertzfeld  4/79
 *
 *	Use:  mkfnt [-s] [-m] [-p#] [-r#] [-ddirectory] font
 *
 *	Mkfnt takes the font named "font" and produces a ditroff description
 *	file from it.  The -s option tells mkfnt that this is a special font,
 *	and sould substitute special character names for the normal ones.  The
 *	-m option switches to the math font character map.  -m and -s together
 *	will get the math font at the moment.  The
 *	-p# option tells what point size the DESC file has as it's "unitwidth"
 *	argument (default: 36).  The -r# option is the resolution of the device
 *	(default: 240, in units/inch).  The -d option tells where to look for
 *	fonts (default: /usr/src/local/imagen/fonts/raster).
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
#include "rst.h"

char 	sccsid[] = "@(#)makeifont.c	1.1	(Berkeley)	%G%";

#define PCNTUP		62	/* percent of maximum height for an ascender */
#define PCNTDOWN	73	/* percent of maximum droop for a descender */
#define FONTDIR		"/usr/src/local/imagen/fonts/raster"
#define POINTSIZE	40	/* this is the "unitwidth" point size */
#define MINSIZE		6	/* the minimum and maximum point size values */
#define MAXSIZE		36	/*    acceptible for use as "unitwidth"s */
#define MINRES		10	/* check up on resolution input by setting */
#define MAXRES		100000	/*    absurdly out-of-range limits on them */


unsigned char *idstrings;	/* place for identifying strings */
unsigned char *endstring;	/* points to end of id strings */
double	fixtowdth;		/* "fix" and magnification conversion factor */
glyph_dir g[DIRSIZ];		/* directory of glyph definitions */
preamble p;			/* set of variables for preamble */

int	res = RES;		/* resolution of the device (units/inch) */
int	pointsize = POINTSIZE;	/* point size being used for unitwidth */
int	psize;			/* point size of font actually used */
int	psizelist[] = { 40, 36, 28, 24, 22, 20, 18, 16,
			14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 0 };

char	*fontdir = FONTDIR;	/* place to look for fonts */
char	IName[100];		/* input file name put here */
char	*rdchar ();		/* function makes strings for ascii */
int	FID = -1;		/* input file number */

int	maxdown = 0;		/* size of the most "droopy" character */
int	maxup = 0;		/* size of the tallest character */
int	type;			/* 1, 2, or 3 for type of ascend/descending */
int	mathf = 0;		/* flag "is this a math font?"; */
int	specialf = 0;		/* flag "is this a special font?";  used to
				   determine which mapping array to look in. */

				/* following are the character maps for */
				/* ascii code-conversion to printables... */
char	**charmap;
char *iregular[] = {

	"*G", "*D", "*H", "*L", "*C", "*P", "*S", "*U", "*F", "*Q", "*W",
	"id", "ij", "ga", "aa", "^", "d^", "hc", "\\-", "..", "~", "->",
	"im", "de", "tc", "tl", "hs", "fe", "ae", "oe", "AE", "OE", "/o",
	"!", "\"", "fm", "ft", "%", "&", "'", "(", ")", "*", "+", ",", "-",
	".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";",
	"<", "=", ">", "?",
	"/O", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
	"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[",
	"b\"", "]", "\\_", "_", "`", "a", "b", "c", "d", "e", "f", "g", "h",
	"i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v",
	"w", "x", "y", "z", "ff", "fi", "fl", "Fi", "Fl",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""
};

char *ispecial[] = {

	"mi", "m.", "mu", "**", "\\", "de", "+-", "-+", "O+", "O-", "OX", "O/",
	"O.", "di", "ht", "bu", "ut", "==", "ib", "ip", "<=", ">=", "(=", ")=",
	"ap", "~~", "sb", "sp", "!=", "eq", "((", "))", "<-", "->", "ua", "da",
	"<>", "<<", ">>", "~=", "<_", "_>", "Ua", "Da", "><", "uL", "uR", "lR",
	"fm", "if", "mo", "!m", "0/", "ru", "al", ")(", "fa", "te", "no", "~N",
	"~R", "~T", "cr", "", "sl", "A", "B", "C", "D", "E", "F", "G", "H", "I",
	"J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W",
	"X", "Y", "Z", "cu", "ca", "c+", "an", "or", "|-", "-|", "lf", "rf",
	"lc", "rc", "{", "}", "<", ">", "bv", "||", "[[", "]]", "", "", "sr",
	"#", "gr", "is", "ux", "dx", "rx", "dm", "sc", "dg", "dd", "pp", "@",
	"co", "", "$",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""
};

char *imath[] = {

	"", "ct", "dd", "aa", "ga", "?1", "?2", "?3", "?4", "?5", "co", "rg",
	"tm", "?6", "pp", "fe", "ma", "bu", "bk", "bb", "ci", "sq", "#", "te",
	"rh", "lh", "*a", "*b", "*q", "*d", "*e", "*f", "*g", "*y", "*i", "*c",
	"*k", "*l", "*m", "*n", "*o", "*p", "*r", "*s", "*t", "*h", "*w", "*x",
	"*u", "*z", "*G", "*D", "*F", "*G", "*C", "*L", "*H", "*W", "pl", "mi",
	"mu", "eq", "di", "+-", "de", "fm", "*X", "es", "?7", "pt", "ts", "gr",
	"pd", ">", "<", ">=", "<=", "or", "sl", "\\\\", "ap", "~=", "~~", "==",
	"po", "**", "?8", "{", "}", "br", "sr", "is", "*S", "*P", "sb", "sp",
	"ca", "cu", "ib", "ip", "if", "?9", "ru", "?0", "??", "bs", "b4", "b9",
	"->", "<-", "ua", "da", "!=", "lf", "rf", "lc", "rc", "ul", "bv", "lt",
	"rt", "lb", "rb", "lk", "rk", "no", "fa", "ti",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""
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

	  case 'm': mathf = 1;			/* math font */
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
    charmap = mathf ? imath : specialf ? ispecial : iregular;

							/* open font file */
    for (i = 0; FID < 0 && (psize = psizelist[i]) > 0; i++) {
	sprintf (IName, "%s/%s.r%d", fontdir, *argv, psize);
	FID = open (IName, 0);
    }
    if (FID < 0) { 
	printf ("Can't find %s\n", *argv);
	exit (8); 
    }

    i = read(FID, &filemark[0], FMARK);
    if (strncmp(filemark, "Rast", 4) || i != FMARK)
	    error("Bad File Mark in Font file.");

    p.p_size = rd2();
    p.p_version = rd1();
    if (p.p_version)
	    error("Wrong version of Font file.");
    p.p_glyph = rd3();
    p.p_first = rd2();
    p.p_last = rd2();
    p.p_mag = rd4();
    p.p_desiz = rd4();
    p.p_linesp = rd4();
    p.p_wordsp = rd4();
    p.p_rot = rd2();
    p.p_cadv = rd1();
    p.p_ladv = rd1();
    p.p_id = rd4();
    p.p_res = rd2();
    if (p.p_res != res)
	    error("Wrong resolution in Font file.");

    i = p.p_glyph - 44;
    idstrings = (unsigned char *) malloc (i);
    endstring = idstrings + i;
    if (read(FID, idstrings, i) != i)
	    error("Bad preamble in Font file.");

    for (i = p.p_first; i <= p.p_last; i++) {
	    g[i].g_height = rd2();
	    g[i].g_width = rd2();
	    g[i].g_up = rd2();
	    g[i].g_left = rd2();
	    g[i].g_pwidth = rd4();
	    g[i].g_bitp = rd3();
    }


    if ((fixtowdth = FIXPIX * p.p_mag / 1000.0) == 0.0)
	fixtowdth = FIXPIX;

    printf("# Font %s, size %.2f, ", IName, p.p_desiz * FIX);
    printf("first %d, last %d, res %d, ", p.p_first, p.p_last, p.p_res);
    printf("mag %.2f\n", fixtowdth / FIXPIX);

    printf("spacewidth %d\n", (int) (p.p_wordsp * fixtowdth));
    printf("name XX\ninternalname #\n");
    if (specialf || mathf) {
	printf ("special\n");
    } else {
	printf ("ligatures ff fl fi ffl ffi 0\n");
    }
    printf ("# char	width	u/d	octal\ncharset\n");
    printf ("\\|	%4d	 0	0\n\\^	%4d	 0	0\n",
 		(int) (p.p_wordsp * fixtowdth) / 2,
 		(int) (p.p_wordsp * fixtowdth) / 4);
    for (j = p.p_first; j <= p.p_last; j++) {
	if (g[j].g_bitp != 0) {
	    if (g[j].g_up > maxup) maxup = g[j].g_up;
	    if ((i = g[j].g_height - (g[j].g_up + 1)) > maxdown) maxdown = i;
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
	if (g[j].g_bitp != 0) {
	    type = (int) (((g[j].g_up * 100) / maxup) > PCNTUP) * 2 | (int)
	    	((((g[j].g_height - (g[j].g_up+1)) * 100)/maxdown) > PCNTDOWN);
	    i = pointsize * g[j].g_pwidth * fixtowdth / psize;
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

rd1()
{
    unsigned char i;

    if(read (FID, &i, 1) != 1) error("File read error");
    return (int) i;
}

rd2()
{
    register int i = rd1() << 8;

    return i + rd1();
}

rd3()
{
    register int i = rd2() << 8;

    return i + rd1();
}

rd4()
{
    register int i = rd2() << 16;

    return i + rd2();
}
