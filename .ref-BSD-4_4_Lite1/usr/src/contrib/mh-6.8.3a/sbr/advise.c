/* advise.c - print out error message */

#include "../h/mh.h"


/* VARARGS2 */

void advise (what, fmt, a, b, c, d, e, f)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
{
    advertise (what, NULLCP, fmt, a, b, c, d, e, f);
}
