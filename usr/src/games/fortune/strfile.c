# include	<stdio.h>
# include	"strfile.h"

/*
 *	This program takes a file composed of strings seperated by
 * lines starting with two consecutive delimiting character (default
 * character is '%') and creates another file which consists of a table
 * describing the file (structure from "strfile.h"), a table of seek
 * pointers to the start of the strings, and the strings, each terinated
 * by a null byte.  Usage:
 *
 *	% strfile [ - ] [ -cC ] [ -sv ] sourcefile [ datafile ]
 *
 *	- - Give a usage summary useful for jogging the memory
 *	c - Change delimiting character from '%' to 'C'
 *	s - Silent.  Give no summary of data processed at the end of
 *	    the run.
 *	v - Verbose.  Give summary of data processed.  (Default)
 *
 *		Ken Arnold	Sept. 7, 1978
 *
 *	Added method to indicate dividers.  A "%-" will cause the address
 * to be added to the structure in one of the pointer elements.
 */

# define	reg	register

# define	DELIM_CH	'-'

char	*infile		= 0,		/* input file name		*/
	outfile[100]	= "",		/* output file name		*/
	delimch		= '%',		/* delimiting character		*/
	*usage[]	= {		/* usage summary		*/
       "usage:	strfile [ - ] [ -cC ] [ -sv ] inputfile [ datafile ]",
       "	- - Give this usage summary",
       "	c - Replace delimiting character with 'C'",
       "	s - Silent.  Give no summary",
       "	v - Verbose.  Give summary.  (default)",
       "	Default \"datafile\" is inputfile.dat",
	0
	},
	*fgets();

short	sflag		= 0;		/* silent run flag		*/

long	ftell(), *calloc();

STRFILE	tbl;				/* statistics table		*/

main(ac, av)
int	ac;
char	*av[]; {

	reg char	*sp, dc;
	reg long	*lp;
	char		string[257];
	int		curseek,	/* number of strings		*/
			delim;		/* current delimiter number	*/
	long		*seekpts,li;	/* table of seek pointers	*/
	FILE		*inf, *outf;

	getargs(ac, av);		/* evalute arguments		*/

	/*
	 * initial counting of input file
	 */

	dc = delimch;
	if ((inf = fopen(infile, "r")) == NULL) {
		perror(infile);
		exit(-1);
	}
	for (curseek = 0; (sp = fgets(string, 256, inf)) != NULL; )
		if (*sp++ == dc && (*sp == dc || *sp == DELIM_CH))
			curseek++;
	curseek++;

	/*
	 * save space at begginning of file for tables
	 */

	if ((outf = fopen(outfile, "w")) == NULL) {
		perror(outfile);
		exit(-1);
	}
	if ((seekpts = calloc(sizeof *seekpts, curseek)) == NULL) {
		perror("calloc");
		exit(-1);
	}
	fwrite(&tbl, sizeof tbl, 1, outf);
	fwrite(seekpts, sizeof *seekpts, curseek, outf);
	*seekpts = ftell(outf);
	fseek(inf, (long) 0, 0);		/* goto start of input	*/

	/*
	 * write the strings onto the file
	 */

	tbl.str_longlen = -1;
	tbl.str_shortlen = 0077777;
	lp = seekpts;
	do {
		sp = fgets(string, 256, inf);
		if (sp == NULL
		    || (*sp == dc && (sp[1] == dc || sp[1] == DELIM_CH))) {
			putc('\0', outf);
			lp++;
			if (sp != NULL)
				*lp = ftell(outf);
			li = ftell(outf) - lp[-1] - 1;
			if (tbl.str_longlen < li)
				tbl.str_longlen = li;
			if (tbl.str_shortlen > li)
				tbl.str_shortlen = li;
			if (sp[1] == DELIM_CH && delim < MAXDELIMS)
				tbl.str_delims[delim++] = lp - seekpts;
		}
		else
			fputs(sp, outf);
	} while (sp != NULL);

	/*
	 * write the tables in
	 */

	fclose(inf);
	tbl.str_numstr = curseek;
	fseek(outf, (long) 0, 0);
	fwrite(&tbl, sizeof tbl, 1, outf);
	fwrite(seekpts, sizeof *seekpts, curseek, outf);
	fclose(outf);
	if (!sflag) {
		printf("\"%s\" converted to \"%s\"\n", infile, outfile);
		if (curseek == 1)
			puts("There was 1 string");
		else
			printf("There were %d strings\n", curseek);
		printf("Longest string: %d byte%s", tbl.str_longlen, tbl.str_longlen == 1 ? "\n" : "s\n");
		printf("Shortest string: %d byte%s", tbl.str_shortlen, tbl.str_shortlen == 1 ? "\n" : "s\n");
	}
}
/*
 *	This routine evaluates arguments from the command line
 */
getargs(ac, av)
int	ac;
char	*av[]; {

	reg char	**argv, *sp;
	reg int		i;
	int		bad, j;

	bad = 0;
	argv = &av[0];
	for (i = 1; i < ac; i++)
		if (*argv[i] == '-')
			if (argv[i][1]) for (sp = &argv[i][1]; *sp; sp++)
				switch (*sp) {
				case 'c': /* new delimiting char	*/
					if ((delimch = *++sp) == '\0') {
						--sp;
						delimch = *argv[++i];
					}
					if (delimch <= 0 || delimch > '~' || delimch == DELIM_CH) {
						printf("bad delimiting character: \\%o\n", delimch);
						bad++;
					}
					break;
				case 's':	/* silent		*/
					sflag++;
					break;
				case 'v':	/* verbose		*/
					sflag = 0;
					break;
				default:	/* unknown flag		*/
					bad++;
					printf("bad flag: '%c'\n", *sp);
					break;
				}
			else {
				for (j = 0; usage[j]; j++)
					puts(usage[j]);
				exit(0);
			}
		else if (infile)
			strcpy(outfile, argv[i]);
		else
			infile = argv[i];
	if (!infile) {
		bad++;
		puts("No input file name");
	}
	if (*outfile == '\0' && !bad) {
		strcpy(outfile, infile);
		strcat(outfile, ".dat");
	}
	if (bad) {
		puts("use \"strfile -\" to get usage");
		exit(-1);
	}
}
