/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "yy.h"

int	line 1;

/*
 * Yymain initializes each of the utility
 * clusters and then starts the processing
 * by calling yyparse.
 */
yymain()
{
#ifdef PI0
	char strings[STRINC];
	int trspace[ITREE];
	int hshtab[HASHINC];

/*
	clear(strings, sizeof strings);
	clear(trspace, sizeof trspace);
*/
	clear(hshtab, sizeof hshtab);
#endif

	/*
	 * Initialize the scanner
	 */
	if (getline() == -1) {
		Perror(filename, "No lines in file");
		pexit(NOSTART);
	}

	/*
	 * Initialize the clusters
	 */
#ifdef PI0
	initstring(strings);
	inithash(hshtab);
	inittree(trspace);
	send(RINIT);
#else
	initstring();

	inithash();
	inittree();
#endif
	initnl();

	/*
	 * Process the input
	 */
	yyparse();
#ifdef DEBUG
	dumpnl(0);
#endif
	send(RFINISH);
	pexit(eflg ? ERRS : AOK);
}

#ifdef PI0
clear(cp, i)
	register char *cp;
	register int i;
{
	do
		*cp++ = 0;
	while (--i);
}
#endif
