/*	qms2ch.c	1.1	87/02/05
 *
 * Font translation for QMS-style fonts (qms' QUIC format) to character format.
 *
 *	Use:  qms2ch [ -b# ] fontfile [ character_list ]
 *
 *		Reads "fontfile" from current directory (or if not found,
 *	from BITDIR defined below) and converts it to a character font format
 *	editable by real people, and convertable back to QUIC format by the
 *	ch2qms program.  The "-b" flag sets a baseline unless the font has
 *	one itself.  Output goes to stdout.
 */

#include <stdio.h>
#include <ctype.h>


#define  RES		300	/* resolution, in pixels per inch */
#define  DIRSIZ		256	/* maximum number of characters in font */
#define  HEADER		"^PY^-^F^-^DF"
#define  BITDIR		"/usr/lib/font/devqms/fonts"


char	defascii[DIRSIZ];	/* list of ascii characters to print */
char	*fontdir = BITDIR;	/* place to look for fonts */
char	*header = HEADER;	/* string that must start each QUIC font file */
char	filename[100];		/* input file name put here */
char	input[100];		/* general-purpose input string */
FILE	*FID;			/* input file number */
int	height;			/* height of every character in the font */
int	fontid;			/* the "font number" */
int	landscape = 0;		/* flag:  is this a landscape font? */
int	insertion = 0;		/* flag:  is this an addition to a font? */
int	signed = 0;		/* flag:  can this font have signed offsets? */
int	version = 0;		/* version code */
int	baseline = 0;		/* baseline of font (if specified) */
int     printwidth;		/* printwidth of each character */
				/* variables to use when printing characters */
int	H, W, Hoff, Woff, lbound, rbound;


main(argc,argv)
int argc;
char **argv;
{
	register int i;
	register int j;
	register int k;
	register int Hpoint;
	register int Wpoint;


	if (argc < 2 || argc > 4) {
	    usage:
		error("usage: %s [ -b# ] filename [ charlist ]", *argv);
	}
	++argv;
	if (**argv == '-' && (*argv)[1]) {
		argc--;
		if (argv[0][1] != 'b')
			goto usage;
		baseline = atoi(&(argv[0][2]));
		argv++;
	}
	if (argc == 3) {
		do
			defascii[argv[1][0]] = 1;
		while (*++(argv[1]));
	} else {
		for (i = 0; i < DIRSIZ; i++)
			defascii[i] = 1;
	}

	sprintf(filename, "%s", *argv);
	if (filename[0] != '-' || filename[1]) {
		if ((FID = fopen(filename, "r")) == NULL) {
			sprintf(filename, "%s/%s", fontdir, *argv);
			if ((FID = fopen(filename, "r")) == NULL)
				error("can't find %s", *argv);
		}
	} else {
		FID = stdin;
	}

	j = strlen(header);
	(void) getnumber(j);
	if (strncmp(input, header, j))
		error("%s, not a QMS Font file.", filename);

	(void) getnumber(6);
	k = (int) input[0];
	if (k == 'i' || k == 'I') {
		fontid = atoi(input + 1);
		(void) getnumber(1);
		k = (int) input[0];
	} else {
		k = (int) input[5];
		input[5] = 0;
		fontid = atoi(input);
	}
	switch (k) {
		case 'y': case 'Y': case 'L': case 'l':
			landscape = 1;
			break;

		case 'x': case 'X': case 'P': case 'p':
			break;

		default:
			error("font is not portrait or landscape");
	}
	version = getnumber(1);
	(void) getnumber(4);
	printf("fontheader\nname %s\n", input);
	printf("version %d\n", version);
	printf("rot %d\n", landscape ? 1 : 0);
	printf("cadv %d\n", landscape ? 1 : 2);
	printf("ladv %d\n", landscape ? 2 : 1);
	height = getnumber(3);
	printf("linesp %d.00\n", height);
	printf("id %d\nres %d\n", fontid, RES);
	j = getnumber(1);
	if (isdigit(input[0])) {
		baseline = j * 100 + getnumber(2);
		printf("baseline %d.00\n", baseline);
		(void) getnumber(1);
		if (landscape)
		    baseline = height - baseline;
	}
	if (input[0] != ',') {
		signed = 1;
		(void) getnumber(1);
		if (input[0] != ',')
			error("error in header, expected \",\"");
	}

	for (;;) {	/* for each character */
	    k = getnumber(1);		/* first digit of character number */
	    if (input[0] == '^') {
		(void) getnumber(1);
		if (input[0] != 'G')
		    error("expected \"G\"");
		exit(0);
	    }

	    k = k * 16 + getnumber(1);	/* second digit of character number */
	    if (!defascii[k]) {
		do
		    i = getc(FID);
		while (i != EOF && i != ',');
		continue;		/* ignore characters not asked for */
	    }

	    printwidth = getnumber(3);
	    H = getnumber(2);
	    if (input[0] == '^') {
		switch (input[1]) {
		    case 'M': case 'm':
			break;
		    case 'R': case 'r':
			break;
		}
		H = getnumber(3);
	    } else {
		H = H * 10 + getnumber(1);
	    }
	    W = getnumber(3);
	    Hoff = getnumber(3);
	    if (ispunct(input[0])) {
		if (input[0] == '-')
		    Hoff = 10 * Hoff - getnumber(1);
		else
		    Hoff = 10 * Hoff + getnumber(1);
	    }
	    Woff = getnumber(3);
	    if (ispunct(input[0])) {
		if (input[0] == '-')
		    Woff = 10 * Woff - getnumber(1);
		else
		    Woff = 10 * Woff + getnumber(1);
	    }
	    printf(":%d, width = %d.00\n", k, printwidth);

	    if (landscape) {
		Hpoint = -Hoff;
		Wpoint = baseline - Woff;
	    } else {
		Hpoint = baseline - Hoff;
		Wpoint = -Woff;
	    }
	    if ((lbound = Wpoint) > 0) lbound = 0;
	    if ((rbound = Wpoint + 1) < W) rbound = W;

	    for (k = Hpoint; k < 0; k++) {
		for (i = lbound; i < rbound; i++)
		    printf("%c", k==Hpoint && i==Wpoint ? 'x':'.');
		putchar ('\n');
	    }
	    for (k = 0; k < H; k++) {
				/* read in one "line" at a time */
		(void) getnumber(((W + 15) / 16) * 4);

		for (i = lbound; i < 0; i++)
		    printf("%c", k==Hpoint && i==Wpoint ? 'x':'.');
		for (i = 0; i < W; i++)
		    printf("%c", k==Hpoint && i==Wpoint ?
			    (fbit(i) ? 'X':'x') : fbit(i) ? '@':'.');
		while (i < rbound) {
		    printf("%c", k==Hpoint && i==Wpoint ? 'x':'.');
		    i++;
		}
		putchar ('\n');
	    }
	    while (k <= Hpoint) {
		for (i = lbound; i < rbound; i++)
		    printf("%c", k==Hpoint && i==Wpoint ? 'x':'.');
		putchar ('\n');
		k++;
	    }
	    putchar ('\n');
	    (void) getnumber(1);
	    if (input[0] != ',')
		error("expected \",\"");
	}
}


/*----------------------------------------------------------------------------*
 | Routine:	error (format_string, argument1, argument2.... )
 |
 | Results:	fprints a message to standard error, then exits with error
 |		code 1
 |
 | Side Efct:	This routine does NOT return
 *----------------------------------------------------------------------------*/
/*VARARGS1*/
error(string, a1, a2, a3, a4)
char *string;
{ 
	fprintf(stderr, "qms2ch: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(1);
}


/*----------------------------------------------------------------------------*
 | Routine:	fbit (row)
 |
 | Results:	returns true (non-0) or false (0) to signify whether the bit
 |		in "row" of the current input line (hex digits) is on or off.
 |
 | Side Efct:	exits the program if the character in the line isn't valid hex
 *----------------------------------------------------------------------------*/

fbit(row)
int row;
{
	register int piece = row & 3;

	switch (input[row / 4]) {
		case '0': return 0;
		case '1': return (piece == 3);
		case '2': return (piece == 2);
		case '3': return (piece > 1);
		case '4': return (piece == 1);
		case '5': return (piece == 1 || piece == 3);
		case '6': return (piece == 1 || piece == 2);
		case '7': return (piece);
		case '8': return (!piece);
		case '9': return (piece == 3 || !piece);
		case 'A': case 'a': return (piece == 2 || !piece);
		case 'B': case 'b': return (piece != 1);
		case 'C': case 'c': return (piece < 2);
		case 'D': case 'd': return (piece != 2);
		case 'E': case 'e': return (piece != 3);
		case 'F': case 'f': return 1;

		default: error("expected HEX digit");
	}
}


/*----------------------------------------------------------------------------*
 | Routine:	getnumber (digits)
 |
 | Results:	read a string of "digits" length from the input file "FID"
 |		and return the decimal value of the string.  If the string
 |		is one character long, it may be interpreted as a hex number.
 |		In any case the null-terminated string is returned globally
 |		to "input".
 |
 | Side Efct:	This routine SKIPS WHITE SPACE, and exits upon error.
 *----------------------------------------------------------------------------*/

int
getnumber(digits)
int digits;
{
	register int i;
	register int j;

	for (i = 0; i < digits; ) {
		if ((j = getc(FID)) == EOF)
			error ("unexpected end of input");
		if (!isspace(input[i] = (char) j))
			i++;
	}
	input[digits] = (char) 0;
	if (digits > 1) {
		return (atoi(input));
	} else switch (input[0]) {
		case '0': return 0;
		case '1': return 1;
		case '2': return 2;
		case '3': return 3;
		case '4': return 4;
		case '5': return 5;
		case '6': return 6;
		case '7': return 7;
		case '8': return 8;
		case '9': return 9;
		case 'A': case 'a': return 10;
		case 'B': case 'b': return 11;
		case 'C': case 'c': return 12;
		case 'D': case 'd': return 13;
		case 'E': case 'e': return 14;
		case 'F': case 'f': return 15;

		default: return 0;
	}
}
