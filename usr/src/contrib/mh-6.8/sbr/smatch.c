/* smatch.c - match a switch */
#ifndef	lint
static char ident[] = "@(#)$Id: smatch.c,v 1.5 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"

#ifndef abs
#define abs(i) (i < 0 ? -i : i)
#endif

smatch(string, swp)
register char *string;
register struct swit *swp;
{
    register char  *sp,
                   *tcp;
    struct swit *tp;
    int     firstone,
            stringlen;

    firstone = UNKWNSW;

    if (string == 0)
	return firstone;

    for (stringlen = strlen (string), tp = swp; tcp = tp -> sw; tp++) {
	if (stringlen < abs (tp -> minchars))
	    continue;		/* no match */
	for (sp = string; *sp == *tcp++;) {
	    if (*sp++ == 0)
		return (tp - swp);/* exact match */
	}
	if (*sp != 0) {
	    if (*sp != ' ')
		continue;	/* no match */
	    if (*--tcp == 0)
		return (tp - swp);/* exact match */
	}
	if (firstone == UNKWNSW)
	    firstone = tp - swp;
	else
	    firstone = AMBIGSW;
    }

    return (firstone);
}
