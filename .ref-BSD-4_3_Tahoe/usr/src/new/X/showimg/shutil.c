/* split off some utility functions */

#include <stdio.h>
#include <X/Xlib.h>

#include "quad_arrow.cursor"
#include "quad_arrow_mask.cursor"
#include "pan.cursor"
#include "pan_mask.cursor"
#include "print.cursor"
#include "print_mask.cursor"
#include "zoom2.cursor"
#include "zoom2_mask.cursor"
#include "zoom4.cursor"
#include "zoom4_mask.cursor"
#include "pal.cursor"
#include "pal_mask.cursor"

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

#define SYNCINT 20

ipow( x, n)
register int x, n;
{
	register int p;
	for (p = 1; n > 0; --n)
		p *= x;
	return(p);
}

make_cursors(imcursor,pancursor,printcursor,zoom2cursor,zoom4cursor,
	     palcursor)
      Cursor *imcursor, *pancursor, *printcursor;
      Cursor *zoom2cursor, *zoom4cursor, *palcursor;
{
      Cursor XCreateCursor();

     *imcursor = XCreateCursor(quad_arrow_width, quad_arrow_height,
			      quad_arrow_bits, 
			      quad_arrow_mask_bits,
			      quad_arrow_x_hot, quad_arrow_y_hot,
			      BlackPixel, WhitePixel, GXcopy);
     *pancursor = XCreateCursor(pan_width, pan_height,
			      pan_bits, 
			      pan_mask_bits,
			      pan_x_hot, pan_y_hot,
			      BlackPixel, WhitePixel, GXcopy);
     *printcursor = XCreateCursor(print_width, print_height,
			      print_bits, 
			      print_mask_bits,
			      print_x_hot, print_y_hot,
			      BlackPixel, WhitePixel, GXcopy);
     *zoom2cursor = XCreateCursor(zoom2_width, zoom2_height,
			      zoom2_bits, 
			      zoom2_mask_bits,
			      zoom2_x_hot, zoom2_y_hot,
			      BlackPixel, WhitePixel, GXcopy);
     *zoom4cursor = XCreateCursor(zoom4_width, zoom4_height,
			      zoom4_bits, 
			      zoom4_mask_bits,
			      zoom4_x_hot, zoom4_y_hot,
			      BlackPixel, WhitePixel, GXcopy);
     *palcursor = XCreateCursor(pal_width, pal_height,
			      pal_bits, 
			      pal_mask_bits,
			      pal_x_hot, pal_y_hot,
			      BlackPixel, WhitePixel, GXcopy);
     return;
}

/* write image pixels to the screen according to location, 
 * zoom factor
 */
writepix(wind, image, ncols, nrows, zoomfactor, x, y, width, height,
	 xzero, yzero, mask,func,planes)
     Window wind;
     unsigned char *image;
     short ncols, nrows, zoomfactor, x, y, width, height, xzero, yzero;
     int mask, func, planes;
{
     unsigned char linebuf[8192];
     int j, k, l, minw, ldebug = 0;

     register unsigned char *byteimage, *line = linebuf;
     register int ncols_reg = ncols;
     register minwidth, maxy;
     register int i;

     minwidth = MIN((ncols_reg-x)<<zoomfactor, width);
     maxy = MIN(nrows<<zoomfactor, y+height);

     byteimage = image + ((yzero + y) * ncols_reg) + xzero + x;

     XSync(0);
     if(zoomfactor == 0) {
         for(i=y; i<maxy; i++) {
	    XPixmapBitsPutZ(wind, x, i,
	    	minwidth, 1, byteimage, mask, func, planes);
	    if((i % SYNCINT) == 0) XSync(0); 
	    byteimage += ncols_reg;
         }
         return;
     } else {

       l = 1<<zoomfactor;

       /* adjust for odd pixels */
       width += x & (l-1);
       x -= x & (l-1);
       height += y & (l-1);
       y -= y & (l-1);
       if(width & (l-1))
	 width = (width & ~(l-1)) + l;
       if(height & (l-1))
	 height = (height & ~(l-1))+ l;

       minwidth = MIN((ncols_reg-(x>>zoomfactor))<<zoomfactor, width);
       maxy = MIN(nrows<<zoomfactor, y+height);
       minw = minwidth>>zoomfactor;

       byteimage = image + ((yzero + (y>>zoomfactor)) * ncols_reg)
                      + xzero + (x>>zoomfactor);

       for( j=y; j<maxy; j+=l) {
	   for(k=0; k<minw; k++ ) 
	       for(i=0; i<l; i++)
	           *line++ = byteimage[k];
           line = linebuf;
	   for(i=0; i<l; i++) {
	       if(j+i >= maxy) return;
               XPixmapBitsPutZ(wind, x, j+i, minwidth, 1, line,
			     mask, func, planes);
	       if(((i+j) % SYNCINT) == 0) XSync(0); 
	   }
	   byteimage += ncols_reg;
       }
     }
     return;
}


