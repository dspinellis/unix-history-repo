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
static char sccsid[] = "@(#)unstr.c	5.6 (Berkeley) 12/15/89";
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

# include	<machine/endian.h>
# include	<sys/param.h>
# include	"strfile.h"
# include	<stdio.h>
# include	<ctype.h>

# ifndef MAXPATHLEN
# define	MAXPATHLEN	1024
# endif	/* MAXPATHLEN */

char	*Infile,			/* name of input file */
	Datafile[MAXPATHLEN],		/* name of data file */
	Delimch;			/* delimiter character */

FILE	*Inf, *Dataf;

char	*strcat(), *strcpy();

/* ARGSUSED */
main(ac, av)
int	ac;
char	**av;
{
	static STRFILE	tbl;		/* description table */

	getargs(av);
	if ((Inf = fopen(Infile, "r")) == NULL) {
		perror(Infile);
		exit(1);
	}
	if ((Dataf = fopen(Datafile, "r")) == NULL) {
		perror(Datafile);
		exit(1);
	}
	(void) fread((char *) &tbl, sizeof tbl, 1, Dataf);
	tbl.str_version = ntohl(tbl.str_version);
	tbl.str_numstr = ntohl(tbl.str_numstr);
	tbl.str_longlen = ntohl(tbl.str_longlen);
	tbl.str_shortlen = ntohl(tbl.str_shortlen);
	tbl.str_flags = ntohl(tbl.str_flags);
	if (!(tbl.str_flags & (STR_ORDERED | STR_RANDOM))) {
		fprintf(stderr, "nothing to do -- table in file order\n");
		exit(1);
	}
	Delimch = tbl.str_delim;
	order_unstr(&tbl);
	(void) fclose(Inf);
	(void) fclose(Dataf);
	exit(0);
}

getargs(av)
register char	*av[];
{
	if (!*++av) {
		(void) fprintf(stderr, "usage: unstr datafile\n");
		exit(1);
	}
	Infile = *av;
	(void) strcpy(Datafile, Infile);
	(void) strcat(Datafile, ".dat");
}

order_unstr(tbl)
register STRFILE	*tbl;
{
	register int	i;
	register char	*sp;
	auto off_t	pos;
	char		buf[BUFSIZ];

	for (i = 0; i < tbl->str_numstr; i++) {
		(void) fread((char *) &pos, 1, sizeof pos, Dataf);
		(void) fseek(Inf, ntohl(pos), 0);
		if (i != 0)
			(void) printf("%c\n", Delimch);
		for (;;) {
			sp = fgets(buf, sizeof buf, Inf);
			if (sp == NULL || STR_ENDSTRING(sp, *tbl))
				break;
			else
				fputs(sp, stdout);
		}
	}
}
