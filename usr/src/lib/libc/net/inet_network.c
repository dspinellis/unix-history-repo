/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)inet_network.c	5.1 (Berkeley) %G%";
#endif not lint

#include <sys/types.h>
#include <ctype.h>

/*
 * Internet network address interpretation routine.
 * The library routines call this routine to interpret
 * network numbers.
 */
u_long
inet_network(cp)
	register char *cp;
{
	register u_long val, base, n;
	register char c;
	u_long parts[4], *pp = parts;
	register int i;

again:
	val = 0; base = 10;
	if (*cp == '0')
		base = 8, cp++;
	if (*cp == 'x' || *cp == 'X')
		base = 16, cp++;
	while (c = *cp) {
		if (isdigit(c)) {
			val = (val * base) + (c - '0');
			cp++;
			continue;
		}
		if (base == 16 && isxdigit(c)) {
			val = (val << 4) + (c + 10 - (islower(c) ? 'a' : 'A'));
			cp++;
			continue;
		}
		break;
	}
	if (*cp == '.') {
		if (pp >= parts + 4)
			return (-1);
		*pp++ = val, cp++;
		goto again;
	}
	if (*cp && !isspace(*cp))
		return (-1);
	*pp++ = val;
	n = pp - parts;
	if (n > 4)
		return (-1);
	for (val = 0, i = 0; i < n; i++) {
		val <<= 8;
		val |= parts[i] & 0xff;
	}
	return (val);
}
