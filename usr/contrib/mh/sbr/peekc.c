/* peekc.c - peek at the next character in a stream */

#include "../h/mh.h"
#include <stdio.h>


int	peekc(ib)
register FILE *ib;
{
    register int    c;

    c = getc (ib);
    (void) ungetc (c, ib);

    return c;
}
