#include <stdio.h>

/*
 * Print l characters starting at s on file f.
 */

putstr(f, s, l)
register FILE *f;
register char *s;
register int l;
   {
   while (l--)
      putc(*s++, f);
   }
