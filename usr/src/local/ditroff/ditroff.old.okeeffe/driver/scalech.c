/*	scalech.c	(Berkeley)	1.2	83/12/06
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
#define SCALE		50


int	width, length, vdest, hdest, code;
int	refh, spot, start, Bduring, Wduring;

int	scale = SCALE;
FILE *	filep;
char	ibuff[MAXLINE], ebuff[MAXLINE];

unsigned char Black[MAXLINE];	/* arrays to figure new scaled line based */
unsigned char BtoW[MAXLINE];	/* on various conditions of the changes in */
unsigned char WtoB[MAXLINE];	/* pixels within lines */
unsigned char WBW[MAXLINE];
unsigned char BWB[MAXLINE];
unsigned char White[MAXLINE];


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

	if (ibuff[0] != ':') {
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
	    width = strlen(ibuff) - 1;

	    vdest = 1;
	    refh = -1;
	    for (length = 1; *(chp = ibuff) != '\n'; length++) {
		hdest = 0;
		start = 0;
		Bduring = 0;
		Wduring = 0;
		for (j = 0; j < width; j++, chp++) {
		    if (hdest != (int) ((j * scale) / 100 + 0.1)) {
			if (start && !Wduring) Black[hdest]++;
			if (start && !spot) BtoW[hdest]++;
			if ((!start) && spot) WtoB[hdest]++;
			if ((!start) && Bduring && !spot) WBW[hdest]++;
			if (start && Wduring && spot) BWB[hdest]++;
			if ((!start) && !Bduring) White[hdest]++;

			hdest = (j * scale) / 100 + 0.1;
			start = spot;
			Bduring = 0;
			Wduring = 0;
		    }
		    spot = 0;
		    switch (*chp) {
			case '.':
				break;
			case 'X':
				spot = 1;
			case 'x':
				if (refh >= 0)
				    error ("glyph %d - two reference points",
									code);
				refh = hdest;
				break;
			case '@':
			case '*':
				spot = 1;
				break;
			default:
				error("illegal character '%c' in map.", *chp);
		    } /* switch */
		    Bduring |= spot;
		    Wduring |= !spot;
		} /* for j */

		if (start && !Wduring) Black[hdest]++;
		if (start) BtoW[hdest]++;
		if ((!start) && Bduring) WBW[hdest]++;
		if ((!start) && !Bduring) White[hdest]++;

		if (fgets(ibuff, MAXLINE, filep) == NULL)
			error("unexpected end of input");

		if (((int) ((length * scale) / 100 + 0.1)) == vdest
							|| ibuff[0] == '\n') {
		    i = (width * scale) / 100 + 0.1;
		    for (j = 0; i-- > 0; j++) {
			if (Black[j] || WBW[j]) spot = 1;
			else if (BWB[j]) spot = 0;
			else if (WtoB[j]) spot = 1;
			else spot = 0;
			if (spot)  {
			    if (j != refh) putchar('@');
			    else {
				putchar('X');
				refh = 2 * width;
			    }
			} else {
			    if (j != refh) putchar('.');
			    else {
				putchar('x');
				refh = 2 * width;
			    }
			}
		    }
		    putchar('\n');
		    for (j = 0; j <= width; j++)
			Black[j] = BtoW[j] = WtoB[j] =
				WBW[j] = BWB[j] = White[j] = 0;
		    vdest = (length * scale) / 100 + 1.1;
		}
	    } /* for length */
	    if (refh < 0) error("No position point in glyph %d", code);
	    putchar('\n');
	} /* else */
    } /* while */
}


/*VARARGS1*/
error(string, a1, a2, a3, a4)
char *string;
{ 
	fprintf(stderr, "ch2rst: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(8);
};
