#include <stdio.h>
#include <X/Xlib.h>

#include "wzoom.cursor"
#include "wzoom_mask.cursor"

#define ZOOMFACT 4   /* zoom factor relative to parent window */

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

static u_char *zimage = NULL;          /* pointer to storage */
static unsigned long zsize = 0;        /* current storage size */
static Bitmap wzmask = NULL;
static int xcur, ycur;
static int xnstatic = 0, ynstatic = 0;
static int xzoom = 0, yzoom = 0, f = 0;

updatezoom(wzoom, xzoomsize, yzoomsize,
	   x, y, xsize, ysize, image, wzfactor)

    Window wzoom;              /* zoom window ID */
    int xzoomsize, yzoomsize;  /* zoom window size */
    register int x, y;         /* center coord for zoom */
    int xsize, ysize;          /* size of original image */
    unsigned char *image;      /* 8-bit image */
    int wzfactor;              /* parent window zoom factor */

{
    unsigned char *malloc(), *free();
    int i;
    register int j, xn = xnstatic, yn = ynstatic;

    if(x<0 || x>=xsize || y<0 || y>= ysize) return;

/* recalculate the storage needed for nearest larger zoomed pixel */
    if((f != ZOOMFACT * wzfactor) || (xzoom != xzoomsize) || 
       (yzoom != yzoomsize)) {
	   f = ZOOMFACT * wzfactor;
	   xn = xnstatic = xzoomsize/f + 1;
	   yn = ynstatic = yzoomsize/f + 1;
	   xzoom = xn*f; 
	   yzoom = yn*f;
	   xcur = (xzoom>>1) - wzoom_x_hot + ((f/ZOOMFACT)<<1);
	   ycur = (yzoom>>1) - wzoom_y_hot + ((f/ZOOMFACT)<<1);
       }

/* check available storage space */
    if( (xzoom*yzoom) > zsize) {
	if(zimage != NULL) free(zimage);
	zsize = xzoom*yzoom;
	if((zimage = malloc(zsize)) == NULL) {
	    fprintf(stderr,"Can't allocate zoom image buffer!\n");
	    exit(1);
	}
    }

    replicate(x-xn/2,y-yn/2,xsize,ysize,image,f,
	    0,0,xzoom,yzoom,zimage);

    if( (y-yn/2) < 0) 
      bzero(zimage,xzoom*f*(yn/2-y));
    if( (y-yn/2+yn) > ysize)
      bzero(zimage+xzoom*f*(ysize-y+yn/2),
	    xzoom*f*(y-yn/2+yn-ysize));
    if( (x-xn/2) < 0)
      for(j=0;j<yzoom;j++) bzero(zimage+j*xzoom,f*(xn/2-x));
    if( (x-xn/2+xn) > xsize)
      for(j=0;j<yzoom;j++) bzero(zimage+j*xzoom+f*(xsize-x+xn/2),
			     f*(x-xn/2+xn-xsize));

    XPixmapBitsPutZ(wzoom,0,0,xzoom,yzoom,zimage,0,GXcopy,AllPlanes);

    if(wzmask == NULL) {
	wzmask = XStoreBitmap(wzoom_mask_width, wzoom_mask_height,
			      wzoom_mask_bits);
    }
    XBitmapBitsPut(wzoom, xcur, ycur,
		   wzoom_width, wzoom_height, wzoom_bits,
		   WhitePixel, BlackPixel, wzmask, GXcopy, AllPlanes);
}

replicate(x0,y0,w0,h0,from,f,x,y,w,h,to)
/* Fill array "to" with a piece of "from", each pixel mapping to fxf pixels */
     unsigned char *from, *to;	/* Source array, destination array */
     int x0,y0,w0,h0;		/* Source origin, dimensions */
     int x,y,w,h;		/* Destination origin, dimensions */
     int f;			/* Replication factor */
{
    register unsigned char *s, *d;
    register int k, n, i;
    int i0,i1,j0,j1,j;

    j0 = MAX(0,y0);
    j1 = MIN(h0,y0+(h-y)/f);
    i0 = MAX(0,x0);
    i1 = MIN(w0,x0+(w-x)/f);
    for(j=j0;j<j1;j++) {
	s = from + w0*j + i0;
	d = to + w*(f*(j-y0)+y) + x + f*(i0-x0);
	i = i1 - i0;
	n = f;
	while(i--) {
	    k = n;
	    while(k--) *d++ = *s;
	    s++;
	}
	i = f-1;
	n = f*(i1-i0);
	s = d - f*(i1-i0);
	d = s + w;
	while(i--) {
	    bcopy(s,d,n);
	    d += w;
	}
    }
}

