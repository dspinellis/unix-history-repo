/*	makeifont.c	(Berkeley)	1.8	86/03/04
 *
 * Font description file producer for imagen fonts:  David Slattengren
 * Taken from vfontinfo by Andy Hertzfeld  4/79
 *
 *  Use:  makeifont [ -nNAME ]  [ -i -s -a -o -l -c ]  [ "-xs1,s2[;s1,s2...]" ]
 *		[ "-ys1,s2[;s1,s2...]" ]  [ -p# ]  [ -r# ]  [ -ddir ]  font
 *
 *	Mkfnt takes the font named "font" and produces a ditroff description
 *	file from it.  The -n option takes the 1 or 2 letter troff name to put
 *	the description (default = XX).  The -s, -o, -i, -a options select a
 *	different character mapping than for a "roman" font.  s = special;
 *	o = math;  i = italics;  a = ascii.  The -l option tells if the font
 *	has ligatures.  The -c option tells makeifont that the font is a
 *	constant width one and sets parameters appropriately.
 *
 *	Both -x and -y options allow character name mapping.  A semi-colon
 *	separated list of comma-separated character-name pairs follows the
 *	x or y.  Notice that there are no spaces in the -x or -y command.  It
 *	is also IMPORTANT to enclose these arguments in single quotes to stop
 *	the cshell from interpretting the contents.  A -x pair REPLACES the
 *	definition for s1 by s2.  A -y pair creates a synonym for s1 and calls
 *	it s2.  -x and -y MUST be sent after -s, -m, -i, or -a  if one of them
 *	is used.  Some synonyms are defaulted.  To remove a synonym or char-
 *	acter, leave out s2.
 *
 *	The -p# option tells what point size the DESC file has
 *	as it's "unitwidth" argument (default: 40).  The -r# option is the
 *	resolution of the device (default: 240, in units/inch).  The -d option
 *	tells where to find fonts (default: /usr/src/local/imagen/fonts/raster).
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

char 	sccsid[] = "@(#)makeifont.c	1.8	(Berkeley)	%G%";

#define PCNTUP		62	/* percent of maximum height for an ascender */
#define PCNTDOWN	73	/* percent of maximum droop for a descender */
#ifndef BITDIR
#define BITDIR		"/usr/src/local/imagen/fonts/raster"
#endif
#define POINTSIZE	40	/* this is the "unitwidth" point size */
#define MINSIZE		6	/* the minimum and maximum point size values */
#define MAXSIZE		36	/*    acceptible for use as "unitwidth"s */
#define MINRES		10	/* check up on resolution input by setting */
#define MAXRES		100000	/*    absurdly out-of-range limits on them */
#define MAXLAST		127	/* highest character code allowed */
#define SYNON		100	/* number of entries in a synonym table. */
				/*    equals twice the number of pairs. */


unsigned char *idstrings;	/* place for identifying strings */
unsigned char *endstring;	/* points to end of id strings */
double	fixtowdth;		/* "fix" and magnification conversion factor */
glyph_dir g[DIRSIZ];		/* directory of glyph definitions */
preamble p;			/* set of variables for preamble */

int	res = RES;		/* resolution of the device (units/inch) */
double	fixpix = FIXPIX;	/* conversion factor "fix"es to pixels */
int	pointsize = POINTSIZE;	/* point size being used for unitwidth */
int	psize;			/* point size of font actually used */
int	psizelist[] = { 40, 36, 28, 24, 22, 20, 18, 16,
			14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 0 };

char	*fontname = "XX";	/* troff name of font - set on command line */
char	*fontdir = BITDIR;	/* place to look for fonts */
char	IName[100];		/* input file name put here */
char	*rdchar ();		/* function makes strings for ascii */
FILE *	FID = NULL;		/* input file number */

int	maxdown = 0;		/* size of the most "droopy" character */
int	maxup = 0;		/* size of the tallest character */
int	type;			/* 1, 2, or 3 for type of ascend/descending */
int	ligsf = 0;		/* flag "does this font have ligatures?" */
int	constant = 0;		/* flag constant width font (spacewidth, etc.)*/

				/* following are the character maps for */
				/* ascii code-conversion to printables... */
char	**charmap;
char	**synonyms;
int	numsyn;

char *iregular[] = {
	"*G", "*D", "*H", "*L", "*C", "*P", "*S", "*U", "*F", "*Q", "*W",
	"id", "ij", "ga", "aa", "^", "d^", "hc", "rn", "..", "~", "ve",
	"im", "de", "ce", "tl", "ar", "fb", "ae", "oe", "AE", "OE", "o/",
	"!", "\"", "fm", "ft", "%", "&", "'", "(", ")", "*", "+", ",", "hy",
	".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";",
	"<", "=", ">", "?",
	"es", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
	"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[",
	"b\"", "]", "\\-", "em", "`", "a", "b", "c", "d", "e", "f", "g", "h",
	"i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v",
	"w", "x", "y", "z", "ff", "fi", "fl", "Fi", "Fl"
};
int	nregular = 14;
char *sregular[SYNON] = {
	"A", "*A",	"B", "*B",	"E", "*E",	"H", "*Y",
	"I", "*I",	"K", "*K",	"M", "*M",	"N", "*N",
	"O", "*O",	"P", "*R",	"T", "*T",	"X", "*X",
	"Z", "*Z",	"hy", "-"
};

char *iascii[] = {
	"m.", "da", "*a", "*b", "an", "no", "mo", "*p", "*l", "*g", "*d",
	"is", "+-", "O+", "if", "pd", "sb", "sp", "ca", "cu", "fa", "te",
	"OX", "<>", "<-", "->", "ap", "!=", "<=", ">=", "==", "or", "",
	"!", "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-",
	".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";",
	"<", "=", ">", "?",
	"@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
	"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[",
	"\\", "]", "^", "em", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i",
	"j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
	"x", "y", "z", "{", "|", "}", "~", "dm"
};
int	nascii = 2;
char *sascii[SYNON] = {
	"-", "hy",	"-", "\\-"
};

char *ispecial[] = {
	"mi", "m.", "mu", "**", "\\", "ci", "+-", "-+", "O+", "O-", "OX", "O/",
	"O.", "di", "ht", "bu", "pe", "==", "ib", "ip", "<=", ">=", "(=", ")=",
	"ap", "pt", "sb", "sp", "!=", ".=", "((", "))", "<-", "->", "ua", "da",
	"<>", "<<", ">>", "~=", "lh", "rh", "Ua", "Da", "><", "uL", "uR", "lR",
	"fm", "if", "mo", "!m", "0/", "ul", "al", ")(", "fa", "te", "no", "?0",
	"?1", "?2", "cr", "", "/", "A", "B", "C", "D", "E", "F", "G", "H", "I",
	"J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W",
	"X", "Y", "Z", "cu", "ca", "c+", "an", "or", "|-", "-|", "lf", "rf",
	"lc", "rc", "{", "}", "<", ">", "br", "||", "[[", "]]", "", "", "sr",
	"#", "gr", "is", "ux", "dx", "rx", "dm", "sc", "dg", "dd", "pp", "@",
	"co", "", "$"
};
int	nspecial = 2;
char *sspecial[SYNON] = {
	"lh", "La",	"rh", "Ra"
};

char *imath[] = {
	"Bl", "Br", "LT", "RT", "LB", "RB", "rt", "rk", "rb", "lt", "lk", "lb",
	"rc", "lc", "rf", "lf", "bv", "ci", "^R", "^S", "^T", "^U", "^V", "^W",
	"^X", "^Y", "^Z", "^[", "^\\", "^]", "^^", "^_",
	" ", "!", "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-",
	".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";",
	"<", "=", ">", "?",
	"@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
	"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[",
	"\\", "]", "^", "em", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i",
	"j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
	"x", "y", "z", "{", "|", "}", "~", "dm"
};
int	nmath = 0;
char *smath[SYNON] = {
	"",""
};

char *iitalics[] = {
	"*G", "*D", "*H", "*L", "*C", "*P", "*S", "*U", "*F", "*Q", "*W",
	"*a", "*b", "*g", "*d", "*e", "*z", "*y", "*h", "*i", "*k", "*l",
	"*m", "*n", "*c", "*p", "*r", "*s", "*t", "*u", "*f", "*x", "id",
	"!", "\"", "el", "Fi", "pd", "&", "'", "(", ")", "*", "+", ",", "hy",
	".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";",
	"<", "=", ">", "?",
	"id", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
	"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[",
	"ff", "]", "fi", "fl", "`", "a", "b", "c", "d", "e", "f", "g", "h",
	"i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v",
	"w", "x", "y", "z", "*q", "*w", "?2", "?1", "Fl"
};
int	nitalics = 15;
char *sitalics[SYNON] = {
	"A", "*A",	"B", "*B",	"E", "*E",	"H", "*Y",
	"I", "*I",	"K", "*K",	"M", "*M",	"N", "*N",
	"O", "*O",	"P", "*R",	"T", "*T",	"X", "*X",
	"Z", "*Z",	"o", "*o",	"hy", "-"
};



main (argc, argv)
int argc;
char **argv;
{
    register int i;		/* two indexes */
    register int j;
    register char *ptr;		/* string traveller */
    register char delimit;	/* place for delemiters on command-line */
    char *replacelist = NULL;	/* list of character-name replacements */
    char *synonymlist = NULL;	/* list of synonym entries */
    char tostring();		/* function makes string */
    char *nextstring();		/* moves to next string on list */
    char *operand();		/* reads operands from commandline */

    charmap = iregular;			/* default character map */
    synonyms = sregular;
    numsyn = nregular;
    while (--argc > 0 && *(*(++argv)) == '-') {	/* do options... */
	switch ((*argv)[1]) {

	  case 's': charmap = ispecial;		/* special font */
		    synonyms = sspecial;
		    numsyn = nspecial;
		    break;

	  case 'o': charmap = imath;		/* math font */
		    synonyms = smath;
		    numsyn = nmath;
		    break;

	  case 'i': charmap = iitalics;		/* italics font */
		    synonyms = sitalics;
		    numsyn = nitalics;
		    break;

	  case 'a': charmap = iascii;		/* ascii font */
		    synonyms = sascii;
		    numsyn = nascii;
		    break;

	  case 'c': constant = 1;		/* constant width font */
		    break;

	  case 'l': ligsf = 1;			/* ascii font */
		    break;

	  case 'n': fontname = operand(&argc, &argv);	/* troff font name */
		    break;

	  case 'x': replacelist = operand(&argc, &argv);   /* replacements */
		    break;

	  case 'y': synonymlist = operand(&argc, &argv);	/* synonyms */
		    break;

	  case 'd': fontdir = operand(&argc, &argv);		/* directory */
		    break;

	  case 'p': pointsize = atoi(operand(&argc, &argv));	/* point size */
		    if (pointsize < MINSIZE || pointsize > MAXSIZE)
			error("illegal point size: %d", pointsize);
		    break;

	  case 'r': res = atoi(operand(&argc, &argv));	/* resolution */
		    if (res < MINRES || res > MAXRES)
			error("illegal resolution: %d", res);
		    fixpix = (FIXIN * res);		/* pixels per fix */
		    break;

	   default: error("bad option: %c", **argv);
	}
    }

    if (replacelist != NULL) {
	ptr = replacelist;
	while (delimit = tostring(ptr, ',')) {		/* get s1 */
	    for (i = 0; i <= MAXLAST; i++)		/* search for match */
		if (strcmp (charmap[i], ptr) == 0)
		    break;
	    if (i > MAXLAST) error("-x option: no match");
	    charmap[i] = ptr = nextstring(ptr);		/* replace s1 */
	    delimit = tostring(ptr, ':');		/* with string s2 */
	    if (delimit) ptr = nextstring(ptr);
	}
    }

    if (synonymlist != NULL) {
	ptr = synonymlist;
	while (delimit = tostring(ptr, ',')) {	/* get s1 */
	    synonyms[2 * numsyn] = ptr;		/* set on end of list */
	    ptr = nextstring(ptr);		/* get string s2 */
	    delimit = tostring(ptr, ':');
	    if (*ptr) {				/* if something there */
		synonyms[2 * numsyn++ + 1] = ptr;	/* add to list */
	    } else {				/* otherwise */
		for (i = 0; i < numsyn; i++) {		/* remove from list */
		    if (!strcmp(synonyms[2*i],synonyms[2*numsyn])) {
			numsyn--;
			for (j = i--; j < numsyn; j++) {
			    synonyms[2 * j] = synonyms[2 * (j+1)];
			    synonyms[2*j + 1] = synonyms[2*j + 3];
			}
		    }
		}
	    }
	    if (delimit) ptr = nextstring(ptr);
	    if (numsyn > SYNON) error("out of synonym space");
	}
    }

    if (argc != 1)					/* open font file */
	error("An RST font filename must be the last option");
    for (i = 0; FID == NULL && (psize = psizelist[i]) > 0; i++) {
	sprintf (IName, "%s/%s.r%d", fontdir, *argv, psize);
	FID = fopen (IName, "r");
    }
    if (FID == NULL)
	error("can't find %s", *argv);

    for (i = 0; i < FMARK; filemark[i++] = getc(FID));
    if (strncmp(filemark, "Rast", 4))
	    error("bad File Mark in Font file.");

    p.p_size = rd2();
    p.p_version = rd1();
    if (p.p_version)
	error("wrong version of Font file.");
    p.p_glyph = rd3();
    p.p_first = rd2();
    p.p_last = rd2();
    if (p.p_last > MAXLAST) {
	fprintf(stderr, "truncating from %d to %d\n", p.p_last, MAXLAST);
	p.p_last = MAXLAST;
    }
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
	    error("wrong resolution in Font file.");

    i = p.p_glyph - 44;
    idstrings = (unsigned char *) malloc (i);
    endstring = idstrings + i;
    while (i--) if (getc(FID) == EOF)
	    error("bad preamble in Font file.");

    for (i = p.p_first; i <= p.p_last; i++) {
	    g[i].g_height = rd2();
	    g[i].g_width = rd2();
	    g[i].g_up = rd2();
	    g[i].g_left = rd2();
	    g[i].g_pwidth = rd4();
	    g[i].g_bitp = rd3();
    }


    if ((fixtowdth = fixpix * p.p_mag / 1000.0) == 0.0)
	fixtowdth = fixpix;

    printf("# Font %s\n# size %.2f, ", IName, p.p_desiz * FIX);
    printf("first %d, last %d, res %d, ", p.p_first, p.p_last, p.p_res);
    printf("mag %.2f\n", fixtowdth / fixpix);

    printf("name %s\n", fontname);
    if (ligsf)
	printf ("ligatures ff fl fi ffl ffi 0\n");
    if ((i = (pointsize * p.p_wordsp * fixtowdth) / psize) > 127) i = 127;
    printf("spacewidth %d\n", i);
    printf ("# char	width	u/d	octal\ncharset\n");
			/* the octal values for the following characters are */
			/* purposefully OUT of the range of characters (128) */
    printf ("\\|	%4d	 0	0%o\n\\^	%4d	 0	0%o\n",
		(constant ? i : i/3), DIRSIZ, (constant ? 0 : i/6), DIRSIZ);

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

    for (j=0; j<DIRSIZ; j++) {
	if (g[j].g_bitp != 0) {
	    type = (int) (((g[j].g_up * 100) / maxup) > PCNTUP) * 2 | (int)
	    	((((g[j].g_height - (g[j].g_up+1)) * 100)/maxdown) > PCNTDOWN);
	    if (*(ptr = charmap[j])) {
		printf ("%s	%4d	 %d	0%o\n", ptr, (int) (pointsize
			* g[j].g_pwidth * fixtowdth / psize), type, j);
		for (i = 0; i < numsyn; i++)
		    if (strcmp (ptr, synonyms[2 * i]) == 0)
			printf ("%s	\"\n", synonyms[2 * i + 1]);
	    }
	}
    } /* for j */
    exit(0);
}


/*----------------------------------------------------------------------------*
 | Routine:	char  * operand (& argc,  & argv)
 |
 | Results:	returns address of the operand given with a command-line
 |		option.  It uses either "-Xoperand" or "-X operand", whichever
 |		is present.  The program is terminated if no option is present.
 |
 | Side Efct:	argc and argv are updated as necessary.
 *----------------------------------------------------------------------------*/

char *operand(argcp, argvp)
int * argcp;
char ***argvp;
{
	if ((**argvp)[2]) return(**argvp + 2); /* operand immediately follows */
	if ((--*argcp) <= 0)			/* no operand */
	    error("command-line option operand missing.");
	return(*(++(*argvp)));			/* operand operand */
}


/*----------------------------------------------------------------------------*
 | Routine:	char  tostring (pointer, delimitter)
 |
 | Results:	checks string pointed to by pointer and turns it into a
 |		string at 'delimitter' by replacing it with '\0'.  If the
 |		end of the string is found first, '\0' is returned; otherwise
 |		the delimitter found there is returned.
 |
 *----------------------------------------------------------------------------*/

char tostring(p, d)
register char *p;
register char d;
{
    while (*p && *p != d) p++;
    d = *p;
    *p = '\0';
    return d;
}


/*----------------------------------------------------------------------------*
 | Routine:	char  * nextstring (pointer)
 |
 | Results:	returns address of next string after one pointed to by
 |		pointer.  The next string is after the '\0' byte.
 |
 *----------------------------------------------------------------------------*/

char *nextstring(p)
register char *p;
{
    while (*(p++));
    return p;
}


/*VARARGS1*/
error(string, a1, a2, a3, a4)
char *string;
{ 
    fprintf(stderr, "makefont: ");
    fprintf(stderr, string, a1, a2, a3, a4);
    fprintf(stderr, "\n");
    exit(8);
}

rd1()
{
    int i;

    if((i = getc(FID)) == EOF) error("file read error");
    return i;
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
