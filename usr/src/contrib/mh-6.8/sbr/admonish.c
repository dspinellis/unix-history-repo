/* admonish.c - admonish the user */

#include "../h/mh.h"


/* VARARGS2 */

void admonish (what, fmt, a, b, c, d, e, f)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
{
    advertise (what, "continuing...", fmt, a, b, c, d, e, f);
}
