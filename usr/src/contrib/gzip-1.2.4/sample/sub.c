/* sub.c   not copyrighted (n) 1993 by Mark Adler */
/* version 1.1   11 Jun 1993 */

/* sub is a simple filter to preprocess a data file before compression.
   It can increase compression for data whose points tend to be close to
   the last point.  The output is the difference of successive bytes of
   the input.  The add filter is used to undo what sub does.  This could
   be used on 8-bit sound or graphics data.

   sub can also take an argument to apply this to interleaved sets of
   bytes.  For example, if the data are 16-bit sound samples, then you
   can use "sub 2" to take differences on the low-byte stream and the
   high-byte stream.  (This gives nearly the same effect as subtracting
   the 16-bit values, but avoids the complexities of endianess of the
   data.)  The concept extends to RGB image data (sub 3), 16-bit stereo
   data (sub 4), floating point data (sub 4 or sub 8), etc.

   add takes no options, since the number of interleaved byte streams
   is put in the first two bytes of the output stream for add to use
   (in little-endian format).

   Examples:

      sub < graph.vga | gzip -9 > graph.vga.sgz
      sub < phone.snd | gzip -9 > phone.snd.sgz
      sub 2 < audio.snd | gzip -9 > audio.snd.sgz
      sub 3 < picture.rgb | gzip -9 > picture.rgb.sgz
      sub 4 < stereo.snd | gzip -9 > stereo.snd.sgz
      sub 8 < double.data | gzip -9 > double.data.sgz

   To expand, use the reverse operation, as in:

      gunzip < double.data.sgz | add > double.data
*/

#include <stdio.h>

#define MAGIC1    'S' /* sub data */
#define MAGIC2    26  /* ^Z */
#define MAX_DIST  16384

char a[MAX_DIST];	/* last byte buffer for up to MAX_DIST differences */

int main(argc, argv)
  int argc;
  char **argv;
{
  int n = 1;		/* number of differences */
  int i;		/* difference counter */
  int c;		/* byte from input */
  int atoi();		/* (avoid including stdlib for portability) */

  /* process arguments */
  if (argc > 2)
  {
    fputs("sub: only one argument needed--# of differences\n", stderr);
    exit(1);
  }
  if (argc > 1)
    n = atoi(argv[1]);

  if (n < 0) n = -n;	/* tolerate "sub -2" */
  if (n == 0 || n > MAX_DIST) {
    fputs("sub: incorrect distance\n", stderr);
    exit(1);
  }

  /* initialize last byte */
  i = n;
  do {
    a[--i] = 0;
  } while (i);

  /* write differenced data */
  putchar(MAGIC1);  putchar(MAGIC2);	/* magic word for add */
  putchar(n & 0xff);			/* so add knows what to do */
  putchar((n>>8) & 0xff);

  while ((c = getchar()) != EOF)
  {
    putchar((c - a[i]) & 0xff);		/* write difference */
    a[i++] = c;				/* save last byte */
    if (i == n)				/* cycle on n differences */
      i = 0;
  }
  exit(0);
  return 0; /* avoid warning */
}
