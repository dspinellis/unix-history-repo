/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strtol.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <ctype.h>

long
strtol(ascii, endp, base)
	register char *ascii;
	char **endp;
	register int base;
{
	register long val;
	register int c;
	int negative;

	for (; isascii(*ascii) && isspace(*ascii); ++ascii);

	negative = 0;
	if (*ascii == '+')
		++ascii;
	else if (*ascii == '-') {
		negative = 1;
		++ascii;
	}

	/*
	 * ``If base is positive, but not greater than 36, it is used as
	 * the base for the conversion.''
	 *	-- The UNIX System User's Manual, 1986
	 */
	if ((unsigned int)base > 36)
		base = 10;
	else if (base == 0)
		if (*ascii == '0') {
			++ascii;
			if (*ascii == 'X' || *ascii == 'x') {
				++ascii;
				base = 16;
			}
			else
				base = 8;
		}
		else
			base = 10;
	else if (base == 16 && *ascii == '0' &&
	    (*++ascii == 'X' || *ascii == 'x'))
			++ascii;

	for (val = 0; isascii(c = *ascii) && isalnum(c); ++ascii) {
		if (isdigit(c))
			c -= '0';
		else {
			if (isupper(c))
				c = tolower(c);
			c = c - 'a' + 10;
		}
		if (c >= base)
			break;
		val = val * base + c;
	}
	if (endp)
		*endp = ascii;
	return(negative ? -val : val);
}
