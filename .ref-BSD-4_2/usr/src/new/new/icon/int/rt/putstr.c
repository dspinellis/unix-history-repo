#include <stdio.h>

/*
 * put a string on output stream.
 */

putstr(f, s, l)
register FILE *f;
register char *s;
register int l;
   {
   while (l--)
      putc(*s++, f);
   }
