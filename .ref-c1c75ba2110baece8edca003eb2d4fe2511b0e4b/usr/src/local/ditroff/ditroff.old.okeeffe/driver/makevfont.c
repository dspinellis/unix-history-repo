/*	makevfont.c	(Berkeley)	85/05/03	1.6
 *
 * Font description file producer for versatec fonts:  David Slattengren
 * Taken from vfontinfo by Andy Hertzfeld  4/79
 *
 *	Use:  makevfont [ -nNAME ]  [ -s -a -o -l -c -p# -r# -f# -ddir ]
 *		[ "-xs1,s2[;s1,s2...]" ]  [ "-ys1,s2[;s1,s2...]" ]  font
 *
 *	Makefont takes the font named "font" (with or without pointsize
 *	extension on the filename) and produces a ditroff description file
 *	from it.  The -n option takes the 1 or 2 letter troff name to put
 *	the description (default = XX).  The -f option takes an integer per-
 *	centage factor to multiply widths by.  The -s, -o and -a options select
 *	a different character mapping than for a "roman" font.  s = special;
 *	o = otimespecal; a = ascii.  The -l option indicates it has ligatures.
 *	The -c option tells makevfont that the font is constant width and
 *	will set parameters appropriately.
 *
 *	Both -x and -y options allow character name mapping.  A colon separated
 *	list of comma-separated character-name pairs follows the x or y.
 *	Notice that there are no spaces in the -x or -y command.  A -x pair
 *	REPLACES the definition for s1 by s2.  A -y pair creates a synonym for
 *	s1 and calls it s2.  -x and -y MUST be sent after -s, -m, -i, or -a
 *	if one of them is used.  Some synonyms are defaulted.  To remove a
 *	synonym or character, leave out s2.
 *
 *	The -p# option tells what point size the DESC file has as it's
 *	"unitwidth" argument (default: 36).  The -r# option is the resolution
 *	of the device (default: 200, in units/inch).  The -d option tells
 *	where to find fonts (default: /usr/lib/vfont).
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
#include <strings.h>

char 	sccsid[] = "@(#)makevfont.c	1.6	(Berkeley)	%G%";

#define MAGICN		0436	/* font file magic number */
#define PCNTUP		62	/* percent of maximum height for an ascender */
#define PCNTDOWN	73	/* percent of maximum droop for a descender */
#ifndef BITDIR
#define BITDIR		"/usr/lib/vfont"
#endif
#define POINTSIZE	36	/* this is the "unitwidth" point size */
#define MINSIZE		6	/* the minimum and maximum point size values */
#define MAXSIZE		36	/*    acceptible for use as "unitwidth"s */
#define RESOLUTION	200	/* resolution of versatec (dots/inch) */
#define MINRES		10	/* check up on resolution input by setting */
#define MAXRES		100000	/*    absurdly out-of-range limits on them */
#define APOINT		72	/* 1/APOINT inches = 1 point */
#define SYNON		100	/* 2 * pairs allowed in synonym table */


struct header	FontHeader;
struct dispatch	disptable[256];

int	res = RESOLUTION;	/* resolution of the device (units/inch) */
int	pointsize = POINTSIZE;	/* point size being used for unitwidth */
int	factor = 100;		/* percent for magnifying (shrinking) widths */
int	psize;			/* point size of font actually used */
int	psizelist[] = { 36,24,22,20,18,16,14,28,12,11,10,9,8,7,6,0 };

char	*fontname = "XX";	/* troff font name - set on command line */
char	*fontdir = BITDIR;	/* place to look for fonts */
char	IName[100];		/* input file name put here */
char	*rdchar ();		/* function makes strings for ascii */
int	FID = -1;		/* input file number */

int	maxdown = 0;		/* size of the most "droopy" character */
int	maxup = 0;		/* size of the tallest character */
int	type;			/* 1, 2, or 3 for type of ascend/descending */
int	nullchar = -1;		/* finds non-existant character in the font */
int	ligsf = 0;		/* flag "does this font have ligatures?" */
int	constant = 0;		/* flag constant width font (spacewidth, etc.)*/

				/* following are the character maps for */
				/* ascii code-conversion to printables... */
char	**charmap;
char	**synonyms;
int	numsyn;

char *vregular[] = {

	"??", "fi", "fl", "ff", "\\-", "ru", "em", "bu", "sq", "Fi", "Fl", "de",
	"dg", "fm", "co", "rg", "ct", "14", "12", "34", "^T", "^U", "^V", "^W",
	"^X", "^Y", "^Z", "^[", "^\\", "^]", "^^", "^_", "", "!", "\"", "#",
	"$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1",
	"2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?",
	"@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
	"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[",
	"\\", "]", "^", "_", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i",
	"j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
	"x", "y", "z", "{", "|", "}", "~", "^?",
	(char *) 0
};
int nregular = 2;
char *sregular[SYNON] = {
	"-", "hy",	"_", "\\_"
};

char *vspecial[] = {

	"??", "if", "ip", "pt", "rh", "cu", "rn", "bs", "+-", "<=", ">=", "sr",
	"ts", "is", "sl", "bv", "lf", "rf", "lc", "rc", "lt", "lb", "rt", "rb",
	"lk", "rk", "sb", "sp", "ca", "no", "lh", "mo", "", "!", "\"", "#",
	"$", "%", "&", "aa", "gr", ")", "mu", "pl", ",", "mi", ".", "di", "==",
	"~=", "ap", "!=", "<-", "->", "ua", "da", "sc", "**", ":", ";", "<",
	"eq", ">", "?", "@", "*A", "*B", "*G", "*D", "*E", "*Z", "*Y", "*H",
	"*I", "*K", "*L", "*M", "*N", "*C", "*O", "*P", "*R", "*S", "*T", "*U",
	"*F", "*X", "*Q", "*W", "dd", "br", "ib", "\\", "ci", "^", "ul", "ga",
	"*a", "*b", "*g", "*d", "*e", "*z", "*y", "*h", "*i", "*k", "*l", "*m",
	"*n", "*c", "*o", "*p", "*r", "*s", "*t", "*u", "es", "*x", "*q", "*w",
	"pd", "*f", "{", "|", "}", "~", "^?",
	(char *) 0
};
int nspecial = 0;
char *sspecial[SYNON] = {
	"",""
};

char *vtimes[] = {

	"??", "if", "ip", "pt", "rh", "cu", "rn", "bs", "+-", "<=", ">=", "mi",
	"**", "pl", "eq", "gr", "lt", "lk", "lb", "rt", "rk", "rb", "ap", "mo",
	"br", "rk", "sb", "sp", "ca", "no", "~=", "mo", "", "da", "no", "ua",
	"sc", "dd", "if", "pd", "sb", "sp", "mu", "+-", "ca", "cu", "<-", "di",
	"->",
	"!=", "sr", "<=", ">=", "==", "or", "is", "bv", "lc", "rc", "lf", "rf",
	"~=", "_", "ib", "ul", "rn", "ip", "*G", "*D", "*E", "*F", "*G", "*H",
	"*I", "??", "*L", "*L", "*N", "*C", "*O", "*P", "*H", "*S", "*S", "*U",
	"*U", "*X", "*W", "*C", "*Q", "br", "ib", "ga", "aa", "^", "ul", "ga",
	"*a", "*b", "*g", "*d", "*e", "*z", "*y", "*h", "*i", "*k", "*l", "*m",
	"*n", "*c", "*o", "*p", "*r", "*s", "*t", "*u", "es", "*x", "*q", "*w",
	"pd", "*f", "{", "|", "}", "~", "^?",
	(char *) 0
};
int ntimes = 0;
char *stimes[SYNON] = {
	"",""
};


char *vascii[] = {
	"", "da", "*a", "*b", "an", "no", "mo", "*p", "*l", "*g", "*d",
	"ua", "+-", "O+", "if", "pd", "sb", "sp", "ca", "cu", "fa", "te",
	"OX", "<>", "<-", "->", "!=", "ap", "<=", ">=", "==", "or", "",
	"!", "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-",
	".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";",
	"<", "=", ">", "?",
	"@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
	"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[",
	"\\", "]", "^", "_", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i",
	"j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w",
	"x", "y", "z", "{", "|", "}", "~", "??",
	(char *)0
};
int	nascii = 3;
char *sascii[SYNON] = {
	"-", "hy",	"-", "\\-",	"_", "\\_"
};


main (argc, argv)
int argc;
char **argv;
{
    register int i;		/* two indexes */
    register int j;
    register char *ptr;		/* string traveler */
    register char delimit;	/* place for delimiters on command line */
    char *replacelist = NULL;	/* string of character name replacements */
    char *synonymlist = NULL;	/* string of synonym entries */
    char tostring();		/* function makes a string */
    char *nextstring();		/* moves to next string on list */
    char *operand();


    charmap = vregular;			/* default character map */
    synonyms = sregular;
    numsyn = nregular;
    while (--argc > 0 && *(*(++argv)) == '-') {		/* do options... */
	switch ((*argv)[1]) {

	  case 's': charmap = vspecial;		/* special font */
		    synonyms = sspecial;
		    numsyn = nspecial;
		    break;

	  case 'o': charmap = vtimes;		/* times special font */
		    synonyms = stimes;
		    numsyn = ntimes;
		    break;

	  case 'a': charmap = vascii;		/* ascii font */
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

	  case 'd': fontdir = operand(&argc, &argv);	/* directory */
		    break;

	  case 'p': pointsize = atoi(operand(&argc, &argv));	/* point size */
		    if (pointsize < MINSIZE || pointsize > MAXSIZE)
			error("Illegal point size: %d", pointsize);
		    break;

	  case 'f': factor = atoi(operand(&argc, &argv));    /*  % reduction */
		    if (factor < 1 || factor > 1000)
			error("Illegal factor: %d", factor);
		    break;

	  case 'r': res = atoi(operand(&argc, &argv));	/* resolution */
		    if (res < MINRES || res > MAXRES)
			error("Illegal resolution: %d", res);
		    break;

	   default: error("Bad option: %c", **argv);
	}
    }
				/* do character name replacements */
    if (replacelist != NULL) {
	ptr = replacelist;
	while (delimit = tostring(ptr, ',')) {		/* get s1 */
	    for (i = 0; charmap[i] != 0; i++)		/* search for match */
		if (strcmp (charmap[i], ptr) == 0)
		    break;
	    if (!charmap[i]) error("-x option: no match");
	    charmap[i] = ptr = nextstring(ptr);		/* replace s1 */
	    delimit = tostring(ptr, ':');		/* with string s2 */
	    if (delimit) ptr = nextstring(ptr);
	}
    }
				/* do the synonym list */
    if (synonymlist != NULL) {
	ptr = synonymlist;
	while (delimit = tostring(ptr, ',')) {	/* get s1 */
	    synonyms[2 * numsyn] = ptr;		/* set on end of list */
	    ptr = nextstring(ptr);		/* get string s2 */
	    delimit = tostring(ptr, ':');
	    if (*ptr) {				 /* if something there */
		synonyms[2 * numsyn++ + 1] = ptr;	/* add to list */
	    } else {				   /* otherwise remove */
		for (i = 0; i < numsyn; i++) {		  /* from list */
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
	error("A vfont filename must be the last operand.");
    if (ptr = rindex(*argv, '.')) ptr++;
    if (ptr && *ptr <= '9' && *ptr >= '0') {
	psize = atoi(ptr);
	if (psize < MINSIZE || psize > MAXSIZE)
	    error("point size of file \"%s\" out of range", *argv);
	sprintf (IName, "%s/%s", fontdir, *argv);
	FID = open (IName, 0);
    } else {
	for (i = 0; FID < 0 && (psize = psizelist[i]) > 0; i++) {
	    sprintf (IName, "%s/%s.%d", fontdir, *argv, psize);
	    FID = open (IName, 0);
	}
    }
    if (FID < 0)
	error ("Can't open %s", *argv);

						/* read font width table */
    if (read (FID, &FontHeader, sizeof FontHeader) != sizeof FontHeader)
	error("Bad header in Font file.");
    if (read (FID, &disptable[0], sizeof disptable) != sizeof disptable)
	error("Bad dispatch table in Font file");
    if (FontHeader.magic != MAGICN)
	printf ("Magic number %o wrong\n", FontHeader.magic);


    printf ("# Font %s, ", IName);			/* head off the file */
    printf ("max width %d, max height %d\n",
		FontHeader.maxx, FontHeader.maxy);
    printf ("name %s\n", fontname);
    if (ligsf)
	printf ("ligatures ff fl fi ffl ffi 0\n");

				/* pass 1 - set up maximums for ups and downs */
    for (j=0; j<256; j++) {	/* and find out constant width if requested */
	if (disptable[j].nbytes != 0) {
	    if (disptable[j].up > maxup) maxup = disptable[j].up;
	    if (disptable[j].down > maxdown) maxdown = disptable[j].down;
	    if (constant && disptable[j].width) constant = disptable[j].width;
	} else			/* find a non-existant character to put \| in */
	    if (nullchar < 0) nullchar = j;
    }
    if (maxdown == 0) maxdown = 1;

    if (constant) {
	constant = (factor * (pointsize * constant + psize/2) / psize) / 100;
	printf ("spacewidth %d\n", constant);
    }
    printf ("# char	width	u/d	octal\ncharset\n");
    if (nullchar >= 0) {
	printf ("\\|	%4d	 0	0%o\n\\^	%4d	 0	0%o\n",
		constant ? constant : (res*pointsize / APOINT + 4)/6, nullchar,
		constant ? 0 : (res * pointsize / APOINT + 7) / 12, nullchar);
    }

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
	    if (*(ptr = charmap[j])) {
		printf ("%s	%4d	 %d	0%o\n", ptr, (factor *
			(pointsize * disptable[j].width + psize/2) / psize)/100,
			type, j);
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
	return(*(++(*argvp)));			/* operand next word */
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


error(s, a1, a2, a3, a4, a5)
char *s;
{ 
    fprintf(stderr, "makefont: ");
    fprintf(stderr, s, a1, a2, a3, a4);
    fprintf(stderr, "\n");
    exit(8);
}
