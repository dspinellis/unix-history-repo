/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)string.c	5.4 (Berkeley) 12/4/87";
#endif not lint

#include "whoami.h"
#include "align.h"
#include "0.h"
#ifndef PI01
#ifndef PXP
#include "send.h"
#endif
#endif

/*
 * STRING SPACE DECLARATIONS
 *
 * Strng is the base of the current
 * string space and strngp the
 * base of the free area therein.
 * Strp is the array of descriptors.
 */
#ifndef PI0
STATIC	char strings[STRINC];
STATIC	char *strng = strings;
STATIC	char *strngp = strings;
#else
char	*strng, *strngp;
#endif
#ifndef PI01
#ifndef PXP
STATIC	char *strp[20];
STATIC	char **stract strp;
int	strmax;
#endif
#endif

#ifndef PI01
#ifndef PXP
#ifndef PI0
initstring()
#else
initstring(strings)
	char *strings;
#endif
{

	*stract++ = strings;
#ifdef PI0
	strng = strngp = strings;
#endif
	strmax = STRINC * 2;
}
#endif
#endif

/*
 * Copy a string into the string area.
 */
char *
savestr(cp)
	register char *cp;
{
	register int i;

	i = strlen(cp) + 1;
	if (strngp + i >= strng + STRINC) {
		strngp = malloc(STRINC);
		if (strngp == 0) {
			yerror("Ran out of memory (string)");
			pexit(DIED);
		}
#ifndef PI01
#ifndef PXP
		*stract++ = strngp;
		strmax += STRINC;
#endif
#endif
		strng = strngp;
	}
	(void) pstrcpy(strngp, cp);
	cp = strngp;
	strngp = cp + i;
#ifdef PI0
	send(RSTRING, cp);
#endif
	return (cp);
}

#ifndef PI1
#ifndef PXP
char *
esavestr(cp)
	char *cp;
{

#ifdef PI0
	send(REVENIT);
#endif
	strngp = ( (char *) roundup( strngp, A_LONG ) );
	return (savestr(cp));
}
#endif
#endif

#ifndef PI01
#ifndef PXP
soffset(cp)
	register char *cp;
{
	register char **sp;
	register int i;

	if (cp == NIL || cp == OCT || cp == HEX)
		return (-cp);
	for (i = STRINC, sp = strp; sp < stract; sp++) {
		if (cp >= *sp && cp < (*sp + STRINC))
			return (i + (cp - *sp));
		i += STRINC;
	}
	i = nlfund(cp);
	if (i != 0)
		return (i);
	panic("soffset");
}
#ifdef PI1
sreloc(i)
	register int i;
{

	if (i == 0 || i == -OCT || i == -HEX)
		return (-i);
	if (i < STRINC) {
		if (i >= INL)
			panic("sreloc INL");
		i = nl[i].symbol;
		if (i == 0)
			panic("sreloc nl[i]");
		return (i);
	}
	if (i > strmax || i < 0)
		panic("sreloc");
	return (strp[(i / STRINC) - 1] + (i % STRINC));
}
#endif
#endif
#endif
