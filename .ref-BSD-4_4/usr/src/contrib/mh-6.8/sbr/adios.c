/* adios.c - print out error message and exit */

#include "../h/mh.h"


/* VARARGS2 */

void adios (what, fmt, a, b, c, d, e, f)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d,
       *e,
       *f;
{
    advise (what, fmt, a, b, c, d, e, f);
    done (1);
}
