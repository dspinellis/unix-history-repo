/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)getword.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"hangman.h"

# if pdp11
#	define	RN	(((off_t) rand() << 16) | (off_t) rand())
# else
#	define	RN	rand()
# endif

/*
 * getword:
 *	Get a valid word out of the dictionary file
 */
getword()
{
	register FILE		*inf;
	register char		*wp, *gp;

	inf = Dict;
	for (;;) {
		fseek(inf, abs(RN % Dict_size), 0);
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

/*
 * abs:
 *	Return the absolute value of an integer
 */
off_t
abs(i)
off_t	i;
{
	if (i < 0)
		return -(off_t) i;
	else
		return (off_t) i;
}
