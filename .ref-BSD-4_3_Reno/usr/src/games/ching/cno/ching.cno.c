/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Guy Harris.
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
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ching.cno.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * cno - Read a question, cast a change, and output the line values to the
 * standard output for processing by "phx".
 */
#include <stdio.h>
#include "ching.h"

long	now;		/* current time */

unsigned seed;		/* seed for random number generator */
unsigned getrand();

char	*change();
char	string[6+1];	/* where the actual change string is put */

int	table[2][2][2] = {
	{ { OYIN,  YYANG,}, { YYANG, YYIN,} },
	{ { YYANG, YYIN,},  { YYIN,  OYANG,} },
};

main()
{
	FILE *logf;

	time(&now);
	seed = (int)now + getquest() + getgid() + getuid() + getpid();	/* randomize */
	printf("%s\n", change());
}

/*
 * Hash the question by adding all the characters together.
 */
int
getquest()
{
	int result;
	register int c;

	result = 0;
	while ((c = getchar()) != EOF)
		result += c;
	return(result);
}

/*
 * Get a set of six lines making up a change.
 */
char *
change()
{
	register int i;

	for (i = 0; i < 6; i++)
		string[i] = table[getrnum()&01][getrnum()&01][getrnum()&01] + '0';
	string[i] = '\0';
	return(string);
}

/*
 * Get a number more random than what getrand() gives.
 */
getrnum()
{
	return((getrand())>>(getrand()%17));
}

/*
 * Get a random number.
 */
unsigned
getrand()
{
	return(seed = (seed*13077) + 6925);
}
