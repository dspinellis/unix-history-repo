#include <stdio.h>
/* This is used by the GNU `assert' macro.  */
void
__eprintf (string, expression, line, filename)
     char *string;
     char *expression;
     int line;
     char *filename;
{
  fprintf (stderr, string, expression, line, filename);
  fflush (stderr);
  abort ();
}
