#include <stdio.h>

/*
 * getstr - read a line into buf from file fd.  At most max characters
 *  are read.  getstr returns the length of the line, not counting
 *  the newline.  Note that if a file doesn't end in a newline, the
 *  characters between the last newline and the end of the file
 *  are never seen.
 */

getstr(buf, max, fd)
register char *buf;
int max;
FILE *fd;
   {
   register int c, l;

   l = 0;
   while ((c = getc(fd)) != '\n') {
      if (c == EOF)
         return -1;
      *buf++ = c;
      if (++l >= max)
         break;
      }
   return l;
   }
