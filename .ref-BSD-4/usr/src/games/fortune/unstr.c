# include	<stdio.h>
# include	"strfile.h"

/*
 *	This program un-does what "strfile" makes, thereby obtaining the
 * original file again.  This can be invoked with the name of the output
 * file, the input file, or both. If invoked with only a single argument
 * ending in ".dat", it is pressumed to be the input file and the output
 * file will be the same stripped of the ".dat".  If the single argument
 * doesn't end in ".dat", then it is presumed to be the output file, and
 * the input file is that name prepended by a ".dat".  If both are given
 * they are treated literally as the input and output files.
 *
 *	Ken Arnold		Aug 13, 1978
 */

# define	reg	register

# define	DELIM_CH	'-'

char	infile[50],			/* name of input file		*/
	outfile[50],			/* name of output file		*/
	*rindex();

long	*calloc();

main(ac, av)
int	ac;
char	*av[]; {

	reg char	c;
	reg FILE	*inf, *outf;
	int		nstr, delim;
	long		*seekpts;
	STRFILE		tbl;		/* description table		*/

	getargs(ac, av);
	if ((inf = fopen(infile, "r")) == NULL) {
		perror(infile);
		exit(-1);
	}
	fread(&tbl,sizeof tbl,1,inf);
	nstr = tbl.str_numstr;
	if ((seekpts = calloc(sizeof *seekpts, nstr)) == NULL) {
		perror("calloc");
		exit(-1);
	}
	fread(seekpts, (sizeof seekpts[0]), nstr, inf);
	if ((outf = fopen(outfile, "w")) == NULL) {
		perror(outfile);
		exit(-1);
	}
	delim = 0;
	while ((c = getc(inf)) != EOF)
		if (c != '\0')
			putc(c, outf);
		else if (--nstr)
			if (ftell(inf) == tbl.str_delims[delim]) {
				fputs("%-\n", outf);
				delim++;
			}
			else
				fputs("%%\n", outf);
}
getargs(ac, av)
reg int		ac;
reg char	**av; {

	reg char	*sp;

	if (ac < 2) {
		printf("usage: %s datafile[.dat] [ outfile ]\n",av[0]);
		exit(-1);
	}
	strcpy(infile,av[1]);
	if (ac < 3) {
		strcpy(outfile,infile);
		if ((sp = rindex(av[1])) && strcmp(sp, ".dat") == 0)
			outfile[strlen(outfile) - 4] = '\0';
		else
			strcat(infile, ".dat");
	}
	else
		strcpy(outfile, av[2]);
}
