/* Sc parse routine
 *
 * usage psc options
 * options:
 *   -L		Left justify strings.  Default is right justify.
 *   -r		Assemble data into rows first, not columns.
 *   -R	n	Increment by n between rows 
 *   -C n	Increment by n between columns
 *   -n n	Length of the row (column) should be n.
 *   -s v	Top left location in the spreadsheet should be v; eg, k5
 *   -d c       Use c as the delimiter between the fields.
 *   -k         Keep all delimiters - Default is strip multiple delimiters to 1.
 *   -f         suppress 'format' lines in output
 *   
 *  Author: Robert Bond
 *		$Revision: 6.8 $
 */

#include <ctype.h>
#include <stdio.h>
#include "sc.h"

#define END 0
#define NUM 1
#define ALPHA 2
#define SPACE 3
#define EOL 4

extern char *optarg;
extern int   optind;
char *coltoa();
char *progname;

#ifdef SYSV3
extern void exit();
#else
extern int exit();
#endif

int colfirst = 0;
int r0 = 0;
int c0 = 0;
int rinc = 1;
int cinc = 1;
int leftadj = 0;
int len = 20000;
char delim1 = ' ';
char delim2 = '\t';
int strip_delim = 1;
int drop_format = 0;
int *fwidth;
int *precision;
int maxcols;

char token[1000];

main(argc, argv)
int argc;
char **argv;
{
    int curlen;
    int curcol, coff;
    int currow, roff;
    int first;
    int c;
    register effr, effc;
    int i,j;
    register char *p;

    progname = argv[0];
    while ((c = getopt(argc, argv, "rfLks:R:C:n:d:")) != EOF) {
	switch(c) {
	case 'r':
	    colfirst = 1;
	    break;
	case 'L':
	    leftadj = 1;
	    break;
	case 's':
	    c0 = getcol(optarg);
	    r0 = getrow(optarg);
	    break;
	case 'R':
	    rinc = atoi(optarg);
	    break;
	case 'C':
	    cinc = atoi(optarg);
	    break;
	case 'n':
	    len = atoi(optarg);
	    break;
	case 'd':
	    delim1 = optarg[0];
	    delim2 = 0;
	    break;
	case 'k':
	    strip_delim = 0;
	    break;
	case 'f':
	    drop_format = 1;
	    break;
	default:
	    (void) fprintf(stderr,"Usage: %s [-rkfL] [-s v] [-R i] [-C i] [-n i] [-d c]\n", progname);
	    exit(1);
        }
    }

    if (optind < argc) {
	    (void) fprintf(stderr,"Usage: %s [-rL] [-s v] [-R i] [-C i] [-n i] [-d c]\n", progname);
	    exit(1);
    }

	/* setup the spreadsheet arrays */
    if (!growtbl(GROWNEW, 0, 0))
	exit(1);

    curlen = 0;
    curcol = c0; coff = 0;
    currow = r0; roff = 0;
    first = 1;

    while(1) {

	effr = currow+roff;
	effc = curcol+coff;

	switch(scan()) {
	case END:
	    if(drop_format) exit(0);
	    for (i = 0; i<maxcols; i++) {
		if (precision[i])
		    (void) printf("format %s %d %d\n", coltoa(i), 
			fwidth[i], precision[i]+1);
	    }
	    exit(0);
	case NUM:
	    first = 0;
	    (void) printf("let %s%d = %s\n", coltoa(effc), effr, token);
	    if (effc >= maxcols - 1)
	    {	if (!growtbl(GROWCOL, 0, 0))
		{	(void) fprintf(stderr, "Invalid column used: %s\n", coltoa(effc));
			continue;
		}
	    }
	    i = 0;
	    j = 0;
	    p = token;
	    while (*p && *p != '.') {
		p++; i++;
	    }
	    if (*p) {
		p++; i++;
	    }
	    while (*p) {
		p++; i++; j++;
	    }
	    if (precision[effc] < j)
		precision[effc] = j;
	    if (fwidth[effc] < i)
		fwidth[effc] = i;
	    break;
	case ALPHA:
	    first = 0;
	    if (leftadj)
		(void) printf("leftstring %s%d = \"%s\"\n", coltoa(effc),effr,token); 
	    else
		(void) printf("rightstring %s%d = \"%s\"\n",coltoa(effc),effr,token); 
	    if (effc >= maxcols - 1)
	    {	if (!growtbl(GROWCOL, 0, 0))
		{	(void) fprintf(stderr, "Invalid column used: %s\n", coltoa(effc));
			continue;
		}
	    }
	    i = strlen(token);
	    if (i > precision[effc])
		precision[effc] = i;
	    break;
	case SPACE:
	    if (first && strip_delim)
		break;
	    if (colfirst)
		roff++;
	    else
		coff++;
	    break;
	case EOL:
	    curlen++;
	    roff = 0;
	    coff = 0;
	    first = 1;
	    if (colfirst) {
		if (curlen >= len) {
		    curcol = c0;
		    currow += rinc;
		    curlen = 0;
		} else {
		    curcol += cinc;
		}
	    } else {
		if (curlen >= len) {
		    currow = r0;
		    curcol += cinc;
		    curlen = 0;
		} else {
		    currow += rinc;
		}
	    }
	    break;
	}
    }
}

scan()
{
    register int c;
    register char *p;

    p = token;
    c = getchar();

    if (c == EOF)
	return(END);

    if (c == '\n')
	return(EOL);

    if (c == delim1 || c == delim2) {
        if (strip_delim) {
	    while ((c = getchar()) && (c == delim1 || c == delim2))
	        ;
	    (void)ungetc(c, stdin);
	} 
	return(SPACE);
    }

    if (c == '\"') {
	while ((c = getchar()) && c != '\"' && c != '\n' && c != EOF)
	    *p++ = c;
	if (c != '\"')
	    (void)ungetc(c, stdin);
	*p = 0;
	return(ALPHA);
    }

    while (c != delim1 && c != delim2 && c!= '\n' && c != EOF) {
	*p++ = c;
	c = getchar();
    }
    *p = 0;
    (void)ungetc(c, stdin);

    p = token;
    c = *p;
    if (isdigit(c) || c == '.' || c == '-' || c == '+') {
	while(isdigit(c) || c == '.' || c == '-' || c == '+' || c == 'e'
	    || c == 'E') {
		c = *p++;
	}
	if (c == 0)
	    return(NUM);
	else
	    return(ALPHA);
    }

    return(ALPHA);
}
    
getcol(p)
char *p;
{
    register  col;

    if (!p)
	return(0);
    while(*p && !isalpha(*p)) 
	p++; 
    if (!*p)
	return(0);
    col = ((*p & 0137) - 'A');
    if (isalpha(*++p)) 
	col = (col + 1)*26 + ((*p & 0137) - 'A');
    return(col);
}

getrow(p)
char *p;
{
    int row;

    if (!p)
	return(0);
    while(*p && !isdigit(*p))
	p++; 
    if (!*p)
	return(0);
    if (sscanf(p, "%d", &row) != 1)
	return(0);
    return(row);
}

char *
coltoa(col)
int col;
{
    static char rname[3];
    register char *p = rname;

    if (col < 0 || col > 25*26) 
	(void) fprintf(stderr,"coltoa: invalid col: %d", col);

    if (col > 25) {
	*p++ = col/26 + 'A' - 1;
	col %= 26;
    }
    *p++ = col+'A';
    *p = 0;
    return(rname);
}

