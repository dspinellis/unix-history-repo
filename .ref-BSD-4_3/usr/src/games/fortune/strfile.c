# include	<stdio.h>
# include	<ctype.h>
# include	"strfile.h"

/*
 *	This program takes a file composed of strings seperated by
 * lines starting with two consecutive delimiting character (default
 * character is '%') and creates another file which consists of a table
 * describing the file (structure from "strfile.h"), a table of seek
 * pointers to the start of the strings, and the strings, each terinated
 * by a null byte.  Usage:
 *
 *	% strfile [ - ] [ -cC ] [ -sv ] [ -oir ] sourcefile [ datafile ]
 *
 *	- - Give a usage summary useful for jogging the memory
 *	c - Change delimiting character from '%' to 'C'
 *	s - Silent.  Give no summary of data processed at the end of
 *	    the run.
 *	v - Verbose.  Give summary of data processed.  (Default)
 *	o - order the strings in alphabetic order
 *	i - if ordering, ignore case 
 *	r - randomize the order of the strings
 *
 *		Ken Arnold	Sept. 7, 1978 --
 *
 *	Added method to indicate dividers.  A "%-" will cause the address
 * to be added to the structure in one of the pointer elements.
 *
 *		Ken Arnold	Nov., 1984 --
 *
 *	Added ordering options.
 */

# define	TRUE	1
# define	FALSE	0

# define	DELIM_CH	'-'

typedef struct {
	char	first;
	long	pos;
} STR;

char	*Infile		= NULL,		/* input file name */
	Outfile[100]	= "",		/* output file name */
	Delimch		= '%',		/* delimiting character */
	*Usage[]	= {		/* usage summary */
       "usage:	strfile [ - ] [ -cC ] [ -sv ] [ -oir ] inputfile [ datafile ]",
       "	- - Give this usage summary",
       "	c - Replace delimiting character with 'C'",
       "	s - Silent.  Give no summary",
       "	v - Verbose.  Give summary.  (default)",
       "	o - order strings alphabetically",
       "	i - ignore case in ordering",
       "	r - randomize the order of the strings",
       "	Default \"datafile\" is inputfile.dat",
	NULL
	};

int	Sflag		= FALSE;	/* silent run flag */
int	Oflag		= FALSE;	/* ordering flag */
int	Iflag		= FALSE;	/* ignore case flag */
int	Rflag		= FALSE;	/* randomize order flag */
int	Delim		= 0;		/* current delimiter number */

long	*Seekpts;

FILE	*Sort_1, *Sort_2;		/* pointers for sorting */

STRFILE	Tbl;				/* statistics table */

STR	*Firstch;			/* first chars of each string */

char	*fgets(), *malloc(), *strcpy(), *strcat();

long	ftell();

main(ac, av)
int	ac;
char	**av;
{
	register char		*sp, dc;
	register long		*lp;
	register unsigned int	curseek;	/* number of strings */
	register long		*seekpts, li;	/* table of seek pointers */
	register FILE		*inf, *outf;
	register int		first;
	register char		*nsp;
	register STR		*fp;
	static char		string[257];

	getargs(ac, av);		/* evalute arguments */

	/*
	 * initial counting of input file
	 */

	dc = Delimch;
	if ((inf = fopen(Infile, "r")) == NULL) {
		perror(Infile);
		exit(-1);
	}
	for (curseek = 0; (sp = fgets(string, 256, inf)) != NULL; )
		if (*sp++ == dc && (*sp == dc || *sp == DELIM_CH))
			curseek++;
	curseek++;

	/*
	 * save space at begginning of file for tables
	 */

	if ((outf = fopen(Outfile, "w")) == NULL) {
		perror(Outfile);
		exit(-1);
	}

	/*
	 * Allocate space for the pointers, adding one to the end so the
	 * length of the final string can be calculated.
	 */
	++curseek;
	seekpts = (long *) malloc(sizeof *seekpts * curseek);	/* NOSTRICT */
	if (seekpts == NULL) {
		perror("calloc");
		exit(-1);
	}
	if (Oflag) {
		Firstch = (STR *) malloc(sizeof *Firstch * curseek);
		if (Firstch == NULL) {
			perror("calloc");
			exit(-1);
		}
	}

	(void) fseek(outf, (long) (sizeof Tbl + sizeof *seekpts * curseek), 0);
	(void) fseek(inf, (long) 0, 0);		/* goto start of input */

	/*
	 * write the strings onto the file
	 */

	Tbl.str_longlen = 0;
	Tbl.str_shortlen = (unsigned int) 0xffffffff;
	lp = seekpts;
	first = Oflag;
	*seekpts = ftell(outf);
	fp = Firstch;
	do {
		sp = fgets(string, 256, inf);
		if (sp == NULL ||
		    (*sp == dc && (sp[1] == dc || sp[1] == DELIM_CH))) {
			putc('\0', outf);
			*++lp = ftell(outf);
			li = ftell(outf) - lp[-1] - 1;
			if (Tbl.str_longlen < li)
				Tbl.str_longlen = li;
			if (Tbl.str_shortlen > li)
				Tbl.str_shortlen = li;
			if (sp && sp[1] == DELIM_CH && Delim < MAXDELIMS)
				Tbl.str_delims[Delim++] = lp - seekpts;
			first = Oflag;
		}
		else {
			if (first) {
				for (nsp = sp; !isalnum(*nsp); nsp++)
					continue;
				if (Iflag && isupper(*nsp))
					fp->first = tolower(*nsp);
				else
					fp->first = *nsp;
				fp->pos = *lp;
				fp++;
				first = FALSE;
			}
			fputs(sp, outf);
		}
	} while (sp != NULL);

	/*
	 * write the tables in
	 */

	(void) fclose(inf);
	Tbl.str_numstr = curseek - 1;

	if (Oflag)
		do_order(seekpts, outf);
	else if (Rflag)
		randomize(seekpts);

	(void) fseek(outf, (long) 0, 0);
	(void) fwrite((char *) &Tbl, sizeof Tbl, 1, outf);
	(void) fwrite((char *) seekpts, sizeof *seekpts, curseek, outf);
	(void) fclose(outf);

	if (!Sflag) {
		printf("\"%s\" converted to \"%s\"\n", Infile, Outfile);
		if (curseek == 0)
			puts("There was 1 string");
		else
			printf("There were %u strings\n", curseek - 1);
		printf("Longest string: %u byte%s\n", Tbl.str_longlen,
		       Tbl.str_longlen == 1 ? "" : "s");
		printf("Shortest string: %u byte%s\n", Tbl.str_shortlen,
		       Tbl.str_shortlen == 1 ? "" : "s");
	}
	exit(0);
}

/*
 *	This routine evaluates arguments from the command line
 */
getargs(ac, av)
register int	ac;
register char	**av;
{
	register char	*sp;
	register int	i;
	register int	bad, j;

	bad = 0;
	for (i = 1; i < ac; i++)
		if (*av[i] == '-' && av[i][1]) {
			for (sp = &av[i][1]; *sp; sp++)
				switch (*sp) {
				  case 'c': /* new delimiting char */
					if ((Delimch = *++sp) == '\0') {
						--sp;
						Delimch = *av[++i];
					}
					if (Delimch <= 0 || Delimch > '~' ||
					    Delimch == DELIM_CH) {
						printf("bad delimiting character: '\\%o\n'",
						       Delimch);
						bad++;
					}
					break;
				  case 's':	/* silent */
					Sflag++;
					break;
				  case 'v':	/* verbose */
					Sflag = 0;
					break;
				  case 'o':	/* order strings */
					Oflag++;
					break;
				  case 'i':	/* ignore case in ordering */
					Iflag++;
					break;
				  case 'r':	/* ignore case in ordering */
					Rflag++;
					break;
				  default:	/* unknown flag */
					bad++;
					printf("bad flag: '%c'\n", *sp);
					break;
				}
		}
		else if (*av[i] == '-') {
			for (j = 0; Usage[j]; j++)
				puts(Usage[j]);
			exit(0);
		}
		else if (Infile)
			(void) strcpy(Outfile, av[i]);
		else
			Infile = av[i];
	if (!Infile) {
		bad++;
		puts("No input file name");
	}
	if (*Outfile == '\0' && !bad) {
		(void) strcpy(Outfile, Infile);
		(void) strcat(Outfile, ".dat");
	}
	if (bad) {
		puts("use \"strfile -\" to get usage");
		exit(-1);
	}
}

/*
 * do_order:
 *	Order the strings alphabetically (possibly ignoring case).
 */
do_order(seekpts, outf)
long	*seekpts;
FILE	*outf;
{
	register int	i;
	register long	*lp;
	register STR	*fp;
	extern int	cmp_str();

	(void) fflush(outf);
	Sort_1 = fopen(Outfile, "r");
	Sort_2 = fopen(Outfile, "r");
	Seekpts = seekpts;
	qsort((char *) Firstch, Tbl.str_numstr, sizeof *Firstch, cmp_str);
	i = Tbl.str_numstr;
	lp = seekpts;
	fp = Firstch;
	while (i--)
		*lp++ = fp++->pos;
	(void) fclose(Sort_1);
	(void) fclose(Sort_2);
	Tbl.str_flags |= STR_ORDERED;
}

/*
 * cmp_str:
 *	Compare two strings in the file
 */
cmp_str(p1, p2)
STR	*p1, *p2;
{
	register int	c1, c2;

	c1 = p1->first;
	c2 = p2->first;
	if (c1 != c2)
		return c1 - c2;

	(void) fseek(Sort_1, p1->pos, 0);
	(void) fseek(Sort_2, p2->pos, 0);

	while (!isalnum(c1 = getc(Sort_1)) && c1 != '\0')
		continue;
	while (!isalnum(c2 = getc(Sort_2)) && c2 != '\0')
		continue;

	while (c1 != '\0' && c2 != '\0') {
		if (Iflag) {
			if (isupper(c1))
				c1 = tolower(c1);
			if (isupper(c2))
				c2 = tolower(c2);
		}
		if (c1 != c2)
			return c1 - c2;
		c1 = getc(Sort_1);
		c2 = getc(Sort_2);
	}
	return c1 - c2;
}

/*
 * randomize:
 *	Randomize the order of the string table.  We must be careful
 *	not to randomize across delimiter boundaries.  All
 *	randomization is done within each block.
 */
randomize(seekpts)
register long	*seekpts;
{
	register int	cnt, i, j, start;
	register long	tmp;
	register long	*origsp;

	Tbl.str_flags |= STR_RANDOM;
	srnd(time((long *) NULL) + getpid());
	origsp = seekpts;
	for (j = 0; j <= Delim; j++) {

		/*
		 * get the starting place for the block
		 */

		if (j == 0)
			start = 0;
		else
			start = Tbl.str_delims[j - 1];

		/*
		 * get the ending point
		 */

		if (j == Delim)
			cnt = Tbl.str_numstr;
		else
			cnt = Tbl.str_delims[j];

		/*
		 * move things around randomly
		 */

		for (seekpts = &origsp[start]; cnt > start; cnt--, seekpts++) {
			i = rnd(cnt - start);
			tmp = seekpts[0];
			seekpts[0] = seekpts[i];
			seekpts[i] = tmp;
		}
	}
}
