/*	scalech.c	(Berkeley)	1.3	84/01/04
 *
 * Font scaling for character format fonts.
 *
 *	Use:	scalech  [ -s# ]  charfile1  > charfile2
 *
 *		Takes input from charfile1 (which must be in the format
 *	written by rst2ch), scales by # (default = 50%) and writes to stdout.
 *	If charfile1 is missing, stdin is read.  The -s flag sets the scaling
 *	factor to # (which is a percentage REDUCTION - can't make fonts bigger)
 */

#include <stdio.h>
#include <ctype.h>


#define MAXLINE		200
#define MAXHEIGHT	200
#define SCALE		50


int	width, length, vdest, hdest, code;
int	refv, refh, spot, start, Bduring, Wduring;
int	notfirsttime, build, voffset, hoffset;

int	scale = SCALE;
FILE *	filep;
char	ibuff[MAXHEIGHT][MAXLINE], ebuff[MAXLINE];

unsigned char lastline[MAXLINE];	/* last printed line */
unsigned char Black[2][MAXLINE];	/* arrays to figure new */
unsigned char WtoB[2][MAXLINE];		/* scaled line based on */
unsigned char BtoW[2][MAXLINE];		/* pixels within lines */
unsigned char White[2][MAXLINE];


main(argc,argv)
int argc;
char **argv;
{
    register int i;
    register int j;
    register char *chp;
    float par;

    while (argc > 1 && argv[1][0] == '-') {
	switch(argv[1][1]) {
	case 's':
		scale = atoi (&(argv[1][2]));
		break;
	default:
		error("%s, unknown option flag", argv[1]);
	}
	argc--; argv++;
    }

    if (argc == 2) {
	if ((filep = fopen (argv[1], "r")) == NULL)
	    error("can't open file \"%s\"", argv[1]);
    } else filep = stdin;

    fgets(ibuff, MAXLINE, filep);
    if (strcmp(ibuff, "header\n"))
	error("not a character font file");
    printf("header\n");

    while (fgets(ibuff, MAXLINE, filep) != NULL) {
	if (index(ibuff, '\n') == 0)
	    error("input line too long");

	if (ibuff[0][0] != ':') {
	    sscanf (ibuff, "%s %f", ebuff, &par);
	    if (strcmp(ebuff, "mag") == 0)
		printf("mag %d\n", (int) (par * scale / 100 + 0.1));
	    else if (strcmp(ebuff, "linesp") == 0)
		printf("linesp %.2f\n", par * scale / 100 + 0.001);
	    else if (strcmp(ebuff, "wordsp") == 0)
		printf("wordsp %.2f\n", par * scale / 100 + 0.001);
	    else
		printf("%s", ibuff);
	} else {
	    if (sscanf (ibuff, ":%d, width = %f", &code, &par) != 2)
		error("bad glyph header, %s", ibuff);
	    printf(":%d, width = %.2f\n", code, (par * scale) / 100.0 + 0.001);

	    if (fgets(ibuff, MAXLINE, filep) == NULL)
		error("unexpected end of input");
	    width = strlen(&(ibuff[0][0])) - 1;
						/* first read in whole glyph */
	    refh = -1;				/* and put in ibuff */
	    for (length = 0; *(chp = &(ibuff[length][0])) != '\n'; length++) {
		for (j = 0; j < width; j++, chp++) {
		    switch (*chp) {
			case '@':
			case '*':
				*chp = 1;
				break;
			case '.':
				*chp = 0;
				break;
			case 'x':
				*chp = 0;
				goto mark;
			case 'X':
				*chp = 1;
		mark:		if (refh >= 0)
				    error ("glyph %d - two reference points",
									code);
				refh = j + 1;
				refv = length;
				break;
			default:
				error("illegal character '%c' in map.", *chp);
		    }
		}
		if (fgets(&(ibuff[length + 1][0]), MAXLINE, filep) == NULL)
			error("unexpected end of input");
	    }
	    if (refh < 0) error("No position point in glyph %d", code);


	    voffset = (refv * scale) / 100;
	    hoffset = (refh * scale) / 100;
	    vdest = 0;
	    notfirsttime = 0;
	    for (j = 0; j <= width; j++)
		Black[build][j] = BtoW[build][j] =
			WtoB[build][j] = White[build][j] = 0;

	    for (i = 0; i < length; i++) {
		chp = &(ibuff[i][0]);
		hdest = 1;
		start = 0;
		Bduring = 0;
		Wduring = 0;
		White[build][0]++;
		for (j = 0; j < width; j++, chp++) {
		    code = ((j-refh) * scale) / 100 + (j<refh ? 1:2) + hoffset;
		    if (hdest != code) {
			if ((start && !Wduring) ||
			    ((!start)&& Bduring &&!spot)) Black[build][hdest]++;
			if (start && !spot) BtoW[build][hdest]++;
			if ((!start) && spot) WtoB[build][hdest]++;
			if (((!start) && !Bduring) ||
			    (start && Wduring && spot)) White[build][hdest]++;

			hdest = code;
			start = spot;
			Bduring = 0;
			Wduring = 0;
		    }

		    spot = *chp;
		    Bduring |= spot;
		    Wduring |= !spot;
		} /* for j */

		if ((start && !Wduring)
		    || ((!start) && Bduring)) Black[build][hdest]++;
		if (start) BtoW[build][hdest]++;
		if ((!start) && !Bduring) White[build][hdest]++;

		code = voffset - (((refv - (i+1))*scale)/100 - (i>=refv ? 1:0));
		if (code != vdest || i == (length - 1)) {
		    if (notfirsttime)
			outline(vdest == (voffset + 1));
		    else
			notfirsttime = 1;
		    vdest = code;

		    build = !build;
		    for (j = 0; j <= width; j++)
			Black[build][j] = BtoW[build][j] =
				WtoB[build][j] = White[build][j] = 0;
		}
	    } /* for i */

	    for (j = 0; j <= width; j++) White[build][j] = 1;
	    outline(vdest == (voffset + 1));		/* output last line */
	    putchar('\n');
	} /* else */
    } /* while */
}

outline(i)
register int i;
{
	register int set;
	register int output = !build;
	register int j;
	int oldset = 0;

	for (j = 1; j <= hdest; j++) {
					/* decide whether to put out a '.' */
					/* or '@' spot for pixel j */
		set = ((!BtoW[output][j]) << 3) | ((!WtoB[output][j]) << 2) |
			((!White[output][j]) << 1) | !Black[output][j];
		switch (set) {
		case 14:	/* all black */
		case 11:	/* all white->black */
		case 10:	/* black and white->black */
		case  2:	/* everything but all white */
		    set = 1;
		    break;

		case 13:	/* all white */
		case  7:	/* all black->white */
		case  5:	/* white and black->white */
		    set = 0;
		    break;

		case  1:	/* everything but all black */
		    if (oldset && (Black[output][j+1] || WtoB[output][j+1]))
			set = 0;
		    else
			set = 1;
		    break;

		case 12:	/* black and white only */
		case  8:	/* black and white and white->black */
		case  4:	/* black and white and black->white */
		    if (Black[build][j])
			set = !lastline[j];
		    else
			set = 1;
		    break;

		case  6:	/* black and black->white */
		    if (lastline[j] && !White[build][j])
			set = 0;
		    else
			set = 1;
		    break;

		case  9:	/* white and white->black */
		    if ((!lastline[j] && !Black[build][j] && !WtoB[build][j])
				|| White[output][j+1] || BtoW[output][j+1])
			set = 1;
		    else
			set = 0;
		    break;

		case 15:	/* none of them */
		case  3:	/* black->white and white->black */
		case  0:	/* everything */
		    if (lastline[j-1] + lastline[j] + lastline[j+1] + oldset +
			(Black[output][j+1] || WtoB[output][j+1]) + 
			(Black[build][j-1] || WtoB[build][j-1]) + 
			(Black[build][j] || WtoB[build][j]) + 
			(Black[build][j+1] || WtoB[build][j+1]) > 5)
			set = 0;
		    else
			set = 1;
		    break;
		}

		if (set)  {
		    if (!i || j != (hoffset + 1))
			putchar('@');
		    else
			putchar('X');
		} else {
		    if (!i || j != (hoffset + 1))
			putchar('.');
		    else
			putchar('x');
		}
		lastline[j-1] = oldset;
		oldset = set;
	}
	lastline[j-1] = oldset;
	putchar('\n');
}

/*VARARGS1*/
error(string, a1, a2, a3, a4)
char *string;
{ 
	fprintf(stderr, "scalech: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(8);
};
