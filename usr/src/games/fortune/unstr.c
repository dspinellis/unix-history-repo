/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)unstr.c	1.1 (Berkeley) 12/9/86";
#endif not lint

# include	<stdio.h>
# include	"strfile.h"

# define	TRUE	1
# define	FALSE	0

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

# define	DELIM_CH	'-'

char	Infile[100],			/* name of input file */
	Outfile[100];			/* name of output file */

short	Oflag = FALSE;			/* use order of initial table */

FILE	*Inf, *Outf;

char	*rindex(), *malloc(), *strcat(), *strcpy();

main(ac, av)
int	ac;
char	**av;
{
	register char	c;
	register int	nstr, delim;
	static STRFILE	tbl;		/* description table */

	getargs(ac, av);
	if ((Inf = fopen(Infile, "r")) == NULL) {
		perror(Infile);
		exit(-1);
		/* NOTREACHED */
	}
	if ((Outf = fopen(Outfile, "w")) == NULL) {
		perror(Outfile);
		exit(-1);
		/* NOTREACHED */
	}
	(void) fread((char *) &tbl, sizeof tbl, 1, Inf);
	if (Oflag) {
		order_unstr(&tbl);
		exit(0);
		/* NOTREACHED */
	}
	nstr = tbl.str_numstr;
	(void) fseek(Inf, (long) (sizeof (long) * (nstr + 1)), 1);
	delim = 0;
	for (nstr = 0; (c = getc(Inf)) != EOF; nstr++)
		if (c != '\0')
			putc(c, Outf);
		else if (nstr != tbl.str_numstr - 1)
			if (nstr == tbl.str_delims[delim]) {
				fputs("%-\n", Outf);
				delim++;
			}
			else
				fputs("%%\n", Outf);
	exit(0);
	/* NOTREACHED */
}

getargs(ac, av)
register int	ac;
register char	**av;
{
	register char	*sp;

	if (ac > 1 && strcmp(av[1], "-o") == 0) {
		Oflag++;
		ac--;
		av++;
	}
	if (ac < 2) {
		printf("usage: %s datafile[.dat] [ outfile ]\n", av[0]);
		exit(-1);
	}
	(void) strcpy(Infile, av[1]);
	if (ac < 3) {
		(void) strcpy(Outfile, Infile);
		if ((sp = rindex(av[1], '.')) && strcmp(sp, ".dat") == 0)
			Outfile[strlen(Outfile) - 4] = '\0';
		else
			(void) strcat(Infile, ".dat");
	}
	else
		(void) strcpy(Outfile, av[2]);
}

order_unstr(tbl)
STRFILE	*tbl;
{
	register int	i, c;
	register int	delim;
	register long	*seekpts;

	seekpts = (long *) malloc(sizeof *seekpts * tbl->str_numstr);	/* NOSTRICT */
	if (seekpts == NULL) {
		perror("malloc");
		exit(-1);
		/* NOTREACHED */
	}
	(void) fread((char *) seekpts, sizeof *seekpts, tbl->str_numstr, Inf);
	delim = 0;
	for (i = 0; i < tbl->str_numstr; i++, seekpts++) {
		if (i != 0)
			if (i == tbl->str_delims[delim]) {
				fputs("%-\n", Outf);
				delim++;
			}
			else
				fputs("%%\n", Outf);
		(void) fseek(Inf, *seekpts, 0);
		while ((c = getc(Inf)) != '\0')
			putc(c, Outf);
	}
}
