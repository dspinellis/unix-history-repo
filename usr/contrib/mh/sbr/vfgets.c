/* vfgets.c - virtual fgets */

#include "../h/mh.h"
#include <stdio.h>


#define	QUOTE	'\\'


int	vfgets (in, bp)
register FILE *in;
register char  **bp;
{
    register int    toggle;
    register char  *cp,
                   *dp,
                   *ep,
		   *fp;
    static int  len = 0;
    static char *pp = NULL;

    if (pp == NULL)
	if ((pp = malloc ((unsigned) (len = BUFSIZ))) == NULL)
	    adios (NULLCP, "unable to allocate string storage");

    for (ep = (cp = pp) + len - 1;;) {
	if (fgets (cp, ep - cp + 1, in) == NULL) {
	    if (cp != pp) {
		*bp = pp;
		return OK;
	    }
	    return (ferror (in) ? NOTOK : DONE);
	}

	if ((dp = cp + strlen (cp) - 2) < cp || *dp != QUOTE) {
wrong_guess: ;
	    if (cp > ++dp)
		adios (NULLCP, "vfgets() botch -- you lose big");
	    if (*dp == '\n') {
		*bp = pp;
		return OK;
	    }
	    else
		cp = ++dp;
	}
	else {
	    for (fp = dp - 1, toggle = 0; fp >= cp; fp--)
		if (*fp != QUOTE)
		    break;
		else
		    toggle = !toggle;
	    if (toggle)
		goto wrong_guess;
	    if (*++dp == '\n')
		*--dp = NULL, cp = dp;
	    else
		cp = ++dp;
	}

	if (cp >= ep) {
	    register int curlen = cp - pp;

	    if ((dp = realloc (pp, (unsigned) (len += BUFSIZ))) == NULL)
		adios (NULLCP, "unable to allocate string storage");
	    else
		cp = dp + curlen, ep = (pp = dp) + len - 1;
	}
    }
}
