#include <stdio.h>
#include <varargs.h>
#undef vprintf
int
vprintf (format, ap)
     char *format;
     va_list ap;
{
  return vfprintf (stdout, format, ap);
}
