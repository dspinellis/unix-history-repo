/*	doublech.c	(Berkeley)	1.1	85/02/04
 *
 * Double size of fonts in character format.
 *
 *	Use:	doublech [ charfile1 ] > charfile2
 *
 *		Takes input from charfile1 (which must be in the format
 *	written by one of the xxx2ch programs), scales by 2 and writes to
 *	stdout.  If charfile1 is missing, stdin is read.
 */

#include <stdio.h>


#define MAXLINE		1024


int	width, length, code;

FILE *	filep;
char	ibuff[MAXLINE];
char	ebuff[MAXLINE];


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
    if (strcmp(ibuff, "fontheader\n"))
	error("not a character font file");
    printf("fontheader\n");

    while (fgets(ibuff, MAXLINE, filep) != NULL) {
	if (index(ibuff, '\n') == 0)
	    error("input line too long");

	if (ibuff[0] != ':') {
	    sscanf (ibuff, "%s %f", ebuff, &par);
	    if (strcmp(ebuff, "mag") == 0)
		printf("mag %d\n", (int) (par * 2.0 + 0.1));
	    else if (strcmp(ebuff, "linesp") == 0)
		printf("linesp %.2f\n", par * 2.0 + 0.001);
	    else if (strcmp(ebuff, "wordsp") == 0)
		printf("wordsp %.2f\n", par * 2.0 + 0.001);
	    else
		printf("%s", ibuff);
	} else {
	    if (sscanf (ibuff, ":%d, width = %f", &code, &par) != 2)
		error("bad glyph header, %s", ibuff);
	    printf(":%d, width = %.2f\n", code, par * 2.0);

	    if (fgets(ibuff, MAXLINE, filep) == NULL)
		error("unexpected end of input");
	    width = strlen(ibuff) - 1;

	    for (length = 0; *(chp = ibuff) != '\n'; length++) {
		for (j = 0; j < width; j++, chp++) {
		    switch (*chp) {
			case '.':
			case 'x':
			case 'X':
			case '@':
				break;
			case 'a':
			case 'M':
			case '*':
				*chp = '@';
				break;
			default:
				error("illegal character '%c' in map.", *chp);
		    } /* switch */
		} /* for j */
							/* 1st line of double */
		for (chp = ibuff; *chp != '\n'; chp++) {
		    if (*chp == 'x') {	/* ignore reference points */
			putchar('.');
			putchar('.');
		    } else if (*chp == 'X') {
			putchar('@');
			putchar('@');
		    } else {
			putchar(*chp);
			putchar(*chp);
		    }
		}
		putchar('\n');			/* 2nd line of double */
		for (chp = ibuff; *chp != '\n'; chp++) {
		    if (*chp == 'x') {
			putchar('x');		/* reference points go only */
			putchar('.');		/*   on lower left of quad */
		    } else if (*chp == 'X') {
			putchar('X');
			putchar('@');
		    } else {
			putchar(*chp);
			putchar(*chp);
		    }
		}
		putchar('\n');
		if (fgets(ibuff, MAXLINE, filep) == NULL)
			error("unexpected end of input");
	    } /* for length */
	    putchar('\n');
	} /* else */
    } /* while */
    exit(0);
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
	fprintf(stderr, "doublech: ");
	fprintf(stderr, string, a1, a2, a3, a4);
	fprintf(stderr, "\n");
	exit(1);
}
