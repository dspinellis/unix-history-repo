/* uleq.c - "unsigned" lexical compare */
#ifndef	lint
static char ident[] = "@(#)$Id: uleq.c,v 1.7 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#define TO_LOWER 040
#define NO_MASK  000
#include <ctype.h>

uleq (c1, c2)
register char  *c1,
               *c2;
{
    register int    c,
		    mask;

    if (!c1)
	c1 = "";
    if (!c2)
	c2 = "";

    while (c = *c1++)
    {
#ifdef LOCALE
	c &= 0xff;
	mask = *c2 & 0xff;
	c = (isalpha(c) && isupper(c)) ? tolower(c) : c;
	mask = (isalpha(mask) && isupper(mask)) ? tolower(mask) : mask;
	if (c != mask)
#else
	mask = (isalpha(c) && isalpha(*c2)) ?  TO_LOWER : NO_MASK;
	if ((c | mask) != (*c2 | mask))
#endif
	    return 0;
	else
	    c2++;
    }
    return (*c2 == 0);
}
