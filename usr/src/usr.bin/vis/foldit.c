/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)foldit.c	1.1 (Berkeley) %G%";
#endif /* not lint */

foldit(chunk, col, max)
	char *chunk;
{
	register char *cp;

	/*
	 * Keep track of column position. Insert hidden newline
	 * if this chunk puts us over the limit.
	 */
again:
	cp = chunk;
	while (*cp) {
		switch(*cp) {
		case '\n':
		case '\r':
			col = 0;
			break;
		case '\t':
			col = col + 8 &~ 07;
			break;
		case '\b':
			col = col ? col - 1 : 0;
			break;
		default:
			col++;
		}
		if (col > (max - 2)) {
			printf("\\\n");
			col = 0;
			goto again;
		} 
		cp++;
	}
	return (col);
}
