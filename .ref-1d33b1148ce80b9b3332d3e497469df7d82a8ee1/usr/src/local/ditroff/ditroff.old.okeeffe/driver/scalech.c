/*	scalech.c	1.6	85/02/04
 *
 * Font scaling for character format fonts.
 *
 *	Use:	scalech  [ -s# ]  [ charfile1 ]  > charfile2
 *
 *		Takes input from charfile1 (which must be in the format
 *	written by any of the xxx2ch programs), scales by # (default = 50%)
 *	and writes to stdout.  If charfile1 is missing, stdin is read.  The
 *	-s flag sets the scaling factor to # (which is a percentage REDUCTION:
 *	scalech can't make fonts bigger)
 */

#include <stdio.h>


#define MAXLINE		300
#define MAXHEIGHT	300
#define SCALE		50

					/* don't ask, really */
int	width, length, vdest, hdest, code;
int	refv, refh, spot, start, Bduring, Wduring;
int	notfirsttime, build, refvdest, refhdest, destwidth;

int	scale = SCALE;		/* percentage to scale by (in 100's) */
int	hscale;			/* half scale value:  used in computations */
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

    hscale = scale / 2;
    fgets(ibuff, MAXLINE, filep);
    if (strcmp(ibuff, "fontheader\n"))
	error("not a character font file");
    printf("fontheader\n");

    while (fgets(ibuff, MAXLINE, filep) != NULL) {
	if (index(ibuff, '\n') == 0)
	    error("input line too long");

	if (ibuff[0][0] != ':') {
	    sscanf (ibuff, "%s %f", ebuff, &par);
	    if (strcmp(ebuff, "mag") == 0)
		printf("mag %d\n", (int) (par * scale / 100.0 + 0.1));
	    else if (strcmp(ebuff, "linesp") == 0)
		printf("linesp %.2f\n", par * scale / 100.0 + 0.001);
	    else if (strcmp(ebuff, "wordsp") == 0)
		printf("wordsp %.2f\n", par * scale / 100.0 + 0.001);
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
		for (j = 0; j <= width; j++, chp++) {
		    switch (*chp) {
			case '@':
			case '*':
				*chp = 1;
				break;
			case '.':
			case '\n':
			case '\r':
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
				refh = j;
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


	    refvdest = (refv * scale + hscale) / 100;
	    refhdest = (refh * scale - hscale) / 100 + (refh ? 2 : 1);
	    destwidth = refhdest + ((width - refh) * scale - hscale) / 100;
	    vdest = 0;
	    notfirsttime = 0;
	    build = 0;
	    for (j = 0; j <= width; j++)
		Black[build][j] = BtoW[build][j] = WtoB[build][j] =
		    White[build][j] = lastline[j] = 0;

	    for (i = 0; i < length; i++) {
		chp = &(ibuff[i][0]);
		hdest = 1;
		start = 0;
		Bduring = 0;
		Wduring = 0;
		White[build][0]++;
		for (j = 0; j <= width; j++, chp++) {
		    code = ((j-refh)*scale+hscale)/100+refhdest-((j<refh)?1:0);
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
				/* look at what line we're gonna build for NEXT
				   and if that's different than what we WERE
				   building for, output the line BEFORE that */
		code = refvdest+((i>=refv)?1:0)-((refv-(i+1))*scale+hscale)/100;
		if (code != vdest || i == (length - 1)) {
		    if (notfirsttime)
			outline(vdest == (refvdest + 1));
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
	    outline(refv == length - 1);	/* output last line - ref */
	    putchar('\n');			/* point only if it's on the */
	} /* else */				/* last line of the block */
    } /* while */
    exit(0);
}


#define	upleft	(lastline[j-1])
#define	up	(lastline[j])
#define	upright	(lastline[j+1])

/*----------------------------------------------------------------------------*
 | Routine:	outline (baseline)
 |
 | Results:	outputs a row of dots, based on gobs of information gathered
 |		in the main program.  If "baseline" is set, then this line
 |		should have the reference point on it.
 |
 | Bugs:	this method smells
 *----------------------------------------------------------------------------*/

outline(baseline)
int baseline;
{
	register int set;
	register int output = !build;
	register int j;
	register int left = 0;
	int right, down, downright, downleft;

	for (j = 1; j <= destwidth; j++) {
					/* decide whether to put out a '.' */
					/* or '@' spot for pixel j */
	    right = Black[output][j+1] || WtoB[output][j+1];
	    downleft = Black[build][j-1] || WtoB[build][j-1];
	    down = Black[build][j] || WtoB[build][j];
	    downright = Black[build][j+1] || WtoB[build][j+1];

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
		case  1:	/* everything but all black */
		    set = 0;
		    break;

		case 12:	/* black and white */
		case  8:	/* black and white and white->black */
		case  4:	/* black and white and black->white */
		    if (down)
			set = !lastline[j];
		    else
			set = 1;
		    break;

		case  6:	/* black and black->white */
		    if (right && upright && downright && (down || up))
			set = 0;
		    else
			set = 1;
		    break;

		case  9:	/* white and white->black */
		    if ((up && upright) || (down && downright) || left || right)
			set = 1;
		    else
			set = 0;
		    break;

		case 15:	/* none of them */
		case  3:	/* black->white and white->black */
		case  0:	/* everything */
		    if((up+down+right+left+upright+downleft+upleft+downright)>4)
			set = 0;
		    else
			set = 1;
		    break;
		}

		if (set)  {
		    if (!baseline || j != refhdest)
			putchar('@');
		    else
			putchar('X');
		} else {
		    if (!baseline || j != refhdest)
			putchar('.');
		    else
			putchar('x');
		}
		lastline[j-1] = left;
		left = set;
	}
	lastline[j-1] = left;
	putchar('\n');
}


/*----------------------------------------------------------------------------*
 | Routine:	error (format_string, argument1, argument2.... )
 |
 | Results:	fprints a message to standard error, then exits with error
 |		code 1.
 |
 | Side Efct:	This routine does NOT return
 *----------------------------------------------------------------------------*/

/*VARARGS1*/
error(string, a1, a2, a3, a4)
char *string;
{ 
	fprintf(stderr, "scalech: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit (1);
}
