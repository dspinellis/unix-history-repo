#include <stdio.h>

/*
 * getstr(buf, max, fd) - read a line into buf from file fd.
 * Reads at most max characters.
 * Returns length of line, excluding newline.
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
