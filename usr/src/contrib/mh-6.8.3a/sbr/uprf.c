/* uprf.c - "unsigned" lexical prefix  */
#ifndef	lint
static char ident[] = "@(#)$Id: uprf.c,v 1.8 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#define TO_LOWER 040
#define NO_MASK  000
#include <ctype.h>

uprf (c1, c2)
register char  *c1,
               *c2;
{
    register int    c,
		    mask;

    if (c1 == 0 || c2 == 0)
	return(0);         /* XXX */

    while (c = *c2++)
    {
#ifdef LOCALE
	c &= 0xff;
	mask = *c1 & 0xff;
	c = (isalpha(c) && isupper(c)) ? tolower(c) : c;
	mask = (isalpha(mask) && isupper(mask)) ? tolower(mask) : mask;
	if (c != mask)
#else
	mask = (isalpha(c) && isalpha(*c1)) ?  TO_LOWER : NO_MASK;
	if ((c | mask) != (*c1 | mask))
#endif
	    return 0;
	else
	    c1++;
    }
    return 1;
}
