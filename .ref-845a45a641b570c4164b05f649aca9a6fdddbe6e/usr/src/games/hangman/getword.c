/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getword.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "hangman.h"
#include <stdlib.h>

/*
 * getword:
 *	Get a valid word out of the dictionary file
 */
getword()
{
	register FILE		*inf;
	register char		*wp, *gp;
	register long		 pos;

	inf = Dict;
	for (;;) {
		pos = (double)rand() / (RAND_MAX + 1.0) * (double)Dict_size;
		fseek(inf, pos, 0);
		if (fgets(Word, BUFSIZ, inf) == NULL)
			continue;
		if (fgets(Word, BUFSIZ, inf) == NULL)
			continue;
		Word[strlen(Word) - 1] = '\0';
		if (strlen(Word) < MINLEN)
			continue;
		for (wp = Word; *wp; wp++)
			if (!islower(*wp))
				goto cont;
		break;
cont:		;
	}
	gp = Known;
	wp = Word;
	while (*wp) {
		*gp++ = '-';
		wp++;
	}
	*gp = '\0';
}
