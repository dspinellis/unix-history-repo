#include <stdio.h>
#include <varargs.h>
#undef vfprintf

int
vfprintf (file, format, ap)
     FILE *file;
     char *format;
     va_list ap;
{
   return _doprnt (format, ap, file);
}
