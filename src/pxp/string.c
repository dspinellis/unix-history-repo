/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"

/*
 * STRING SPACE DECLARATIONS
 *
 * Strng is the base of the current
 * string space and strngp the
 * base of the free area therein.
 * No array of descriptors is needed
 * as string space is never freed.
 */
STATIC	char strings[STRINC];
STATIC	char *strng strings;
STATIC	char *strngp strings;

/*
initstring()
{

}
 */
/*
 * Copy a string into the string area.
 */
savestr(cp)
	register char *cp;
{
	register int i;

	i = strlen(cp) + 1;
	if (strngp + i >= strng + STRINC) {
		strngp = alloc(STRINC);
		if (strngp == -1) {
			yerror("Ran out of memory (string)");
			pexit(DIED);
		}
		strng = strngp;
	}
	strcpy(strngp, cp);
	cp = strngp;
	strngp = cp + i;
	return (cp);
}

esavestr(cp)
	char *cp;
{

	strngp = (strngp + 1) &~ 1;
	return (savestr(cp));
}
