/* ambigsw.c - report an ambiguous switch */

#include "../h/mh.h"
#include <stdio.h>


void ambigsw (arg, swp)
register char   *arg;
register struct swit *swp;
{
    advise (NULLCP, "-%s ambiguous.  It matches", arg);
    printsw (arg, swp, "-");
}
