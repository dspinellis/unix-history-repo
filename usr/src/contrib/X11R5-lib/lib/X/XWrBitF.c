/* $XConsortium: XWrBitF.c,v 1.9 91/02/01 16:34:58 gildea Exp $ */
/* Copyright, 1987, Massachusetts Institute of Technology */

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#include "Xlibint.h"
#include <X11/Xos.h>
#include "Xutil.h"
#include <stdio.h>

#define ERR_RETURN 0

static char *Format_Image(image, resultsize)
XImage *image;
int *resultsize;
{
  register int x, c, b;
  register char *ptr;
  int y;
  char *data;
  int width, height;
  int bytes_per_line;

  width = image->width;
  height = image->height;

  bytes_per_line = (width+7)/8;
  *resultsize = bytes_per_line * height;           /* Calculate size of data */

  data = (char *) Xmalloc( *resultsize );           /* Get space for data */
  if (!data)
    return(ERR_RETURN);

  /*
   * The slow but robust brute force method of converting the image:
   */
  ptr = data;
  c = 0; b=1;
  for (y=0; y<height; y++) {
    for (x=0; x<width;) {
      if (XGetPixel(image, x, y))
	c |= b;
      b <<= 1;
      if (!(++x & 7)) {
	*(ptr++)=c;
	c=0; b=1;
      }
    }
    if (x & 7) {
      *(ptr++)=c;
      c=0; b=1;
    }
  }

  return(data);
}
   
#define BYTES_PER_OUTPUT_LINE 12

#if NeedFunctionPrototypes
int XWriteBitmapFile(
     Display *display,
     _Xconst char *filename,
     Pixmap bitmap,
     unsigned int width,
     unsigned int height,
     int x_hot,
     int y_hot)
#else
int XWriteBitmapFile(display, filename, bitmap, width, height, x_hot, y_hot)
     Display *display;
     char *filename;
     Pixmap bitmap;
     unsigned int width, height;
     int x_hot, y_hot;
#endif
{
  char *data, *ptr;
  int size, byte;
  int c;
  XImage *image;
  FILE *stream;
  char *name;

  if (!(name = rindex(filename, '/')))
    name = (char *)filename;
  else
    name++;

  if (!(stream = fopen(filename, "w")))
    return(BitmapOpenFailed);

  /* Convert bitmap to an image */
  image = XGetImage(display, bitmap, 0,0,width, height, 1L, XYPixmap);

  /* Get standard format for data */
  data = Format_Image(image, &size);
  XDestroyImage(image);
  if (!data) {
    fclose(stream);
    return(BitmapNoMemory);
  }

  /* Write out standard header */
  fprintf(stream, "#define %s_width %d\n", name, width);
  fprintf(stream, "#define %s_height %d\n", name, height);
  if (x_hot != -1) {
    fprintf(stream, "#define %s_x_hot %d\n", name, x_hot);
    fprintf(stream, "#define %s_y_hot %d\n", name, y_hot);
  }

  /* Print out the data itself */
  fprintf(stream, "static char %s_bits[] = {", name);
  for (byte=0, ptr=data; byte<size; byte++, ptr++) {
    if (!byte)
      fprintf(stream, "\n   ");
    else if (!(byte % BYTES_PER_OUTPUT_LINE))
      fprintf(stream, ",\n   ");
    else
      fprintf(stream, ", ");
    c = *ptr;
    if (c<0)
      c += 256;
    fprintf(stream, "0x%02x", c);
  }
  fprintf(stream, "};\n");

  Xfree(data);
  fclose(stream);
  return(BitmapSuccess);
}
