/* Font scaling for character format fonts.
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


int	width, length, refv, refh, code;

int	scale = SCALE;
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

	    for (j = 0; j < MAXLINE; ebuff[j++] = '.');
	    refv = 1;
	    for (length = 1; *(chp = ibuff) != '\n'; length++) {
		for (j = 0; j < width; j++, chp++) {
		    refh = (j * scale) / 100 + 0.1;
		    switch (*chp) {
			case '.':
				break;
			case 'x':
				if (ebuff[refh] == '.')
				    ebuff[refh] = 'x';
				else
				    ebuff[refh] = 'X';
				break;
			case 'X':
				ebuff[refh] = 'X';
				break;
			case '@':
			case 'a':
				if (ebuff[refh] == 'x')
				    ebuff[refh] = 'X';
				else if (ebuff[refh] != 'X')
				    ebuff[refh] = '@';
				break;
			default:
				error("illegal character '%c' in map.", *chp);
		    } /* switch */
		} /* for j */
		if (fgets(ibuff, MAXLINE, filep) == NULL)
			error("unexpected end of input");

		if (((int) ((length * scale) / 100 + 0.1)) == refv
							|| ibuff[0] == '\n') {
		    refh = (width * scale) / 100 + 0.1;
		    for (j = 0; refh-- > 0; putchar(ebuff[j++]));
		    putchar('\n');
		    for (j = 0; j <= width; ebuff[j++] = '.');
		    refv = (length * scale) / 100 + 1.1;
		}
	    } /* for length */
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
