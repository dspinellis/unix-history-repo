/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ken Arnold.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)unstr.c	5.3 (Berkeley) %G%";
#endif /* not lint */

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

# include	<sys/types.h>
# include	<sys/param.h>
# include	"strfile.h"
# include	<stdio.h>
# include	<ctype.h>

# define	TRUE	1
# define	FALSE	0

# ifndef MAXPATHLEN
# define	MAXPATHLEN	1024
# endif	/* MAXPATHLEN */

# ifdef SYSV
# define	rename(a1,a2)	(-1)
# endif

char	Infile[MAXPATHLEN],		/* name of input file */
	Outfile[MAXPATHLEN],		/* name of output file */
	Tmpfile[MAXPATHLEN],		/* name of temporary file */
	Delimch;			/* delimiter character */

FILE	*Inf, *Outf, *Textf;

char	*rindex(), *malloc(), *strcat(), *strcpy(), *mktemp();

main(ac, av)
int	ac;
char	**av;
{
	static STRFILE	tbl;		/* description table */

	getargs(ac, av);
	if ((Inf = fopen(Infile, "r")) == NULL) {
		perror(Infile);
		exit(-1);
		/* NOTREACHED */
	}
	(void) fread((char *) &tbl, sizeof tbl, 1, Inf);
	if (!(tbl.str_flags & (STR_ORDERED | STR_RANDOM))) {
		fprintf(stderr, "nothing to do -- table in file order\n");
		exit(1);
	}
	Delimch = tbl.str_delim;
	if ((Textf = fopen(Outfile, "r")) == NULL) {
		perror(Outfile);
		exit(-1);
		/* NOTREACHED */
	}
	(void) strcpy(Tmpfile, mktemp("unstrXXXXXX"));
	if ((Outf = fopen(Tmpfile, "w")) == NULL) {
		perror(Tmpfile);
		exit(-1);
		/* NOTREACHED */
	}
	order_unstr(&tbl);
	fclose(Outf);
	fclose(Textf);
	fclose(Inf);
	if (rename(Tmpfile, Outfile) < 0 && mv(Tmpfile, Outfile) < 0) {
		fprintf(stderr, "could not rename %s to %s\n", Tmpfile, Outfile);
		exit(-1);
	}
	exit(0);
}

getargs(ac, av)
register int	ac;
register char	*av[];
{
	register int	i;
	register char	*sp;
	register short	bad;

	bad = 0;
	for (i = 1; i < ac; i++)  {
		if (av[i][0] != '-') {
			(void) strcpy(Infile, av[i]);
			if (i + 1 >= ac) {
				(void) strcpy(Outfile, Infile);
				if ((sp = rindex(av[i], '.')) &&
				    strcmp(sp, ".dat") == 0)
					Outfile[strlen(Outfile) - 4] = '\0';
				else
					(void) strcat(Infile, ".dat");
			}
			else
				(void) strcpy(Outfile, av[i + 1]);
			break;
		}
		else if (av[i][1] == '\0') {
			printf("usage: unstr datafile[.dat] [outfile]\n");
			exit(0);
			/* NOTREACHED */
		}
		else
			for (sp = &av[i][1]; *sp != '\0'; sp++)
				switch (*sp) {
				  default:
					fprintf(stderr, "unknown flag: '%c'\n",
						*sp);
					bad++;
					break;
				}
	}
	if (bad) {
		printf("use \"%s -\" to get usage\n", av[0]);
		exit(-1);
	}
}

mv(file1, file2)
char	*file1, *file2;
{
	char	buf[BUFSIZ];

	sprintf(buf, "mv %s %s", file1, file2);
	return system(buf) != 0 ? -1 : 0;
}

order_unstr(tbl)
register STRFILE	*tbl;
{
	register int	i;
	register char	*sp;
	auto off_t	pos;
	char		buf[BUFSIZ];

	for (i = 0; i < tbl->str_numstr; i++) {
		fread((char *) &pos, 1, sizeof pos, Inf);
		fseek(Textf, pos, 0);
		if (i != 0)
			fprintf(Outf, "%c%c\n", Delimch, Delimch);
		for (;;) {
			sp = fgets(buf, sizeof buf, Textf);
			if (sp == NULL || STR_ENDSTRING(sp, *tbl))
				break;
			else
				fputs(sp, Outf);
		}
	}
}
