/* Calculates gray scale based on mouse offset from window center */
#include <X/Xlib.h>

#ifdef MC68000
#include <sys/types.h>
#endif

#include "shimg.h"
   
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

CalcMap(winfo,cmap,x,y,ncolors,flags,key)
	WindowInfo *winfo;
	Color *cmap;
	int x,y, key;
	int ncolors;
	u_short flags;
{
	register int intercept,slope,intensity,i;

	if(x<0 || y<=0 || x>=winfo->width-2 || y>winfo->height) return;

	intercept = (((x<<1) - winfo->width)<<16) / winfo->width;
	slope = ((winfo->height - y)<<16) / y;

	for(i=0; i<ncolors; i++) {
		intensity = (slope * i)/(ncolors-1) + intercept;
		intensity = MIN(65535,MAX(intensity,0));
		if(flags & VOP_Inverse)  intensity = 65535 - intensity;
		if(flags & VOP_RGB) 
		    switch((int)key) {
			case LeftButton:
				cmap[i].red = intensity; break;
			case MiddleButton:
				cmap[i].green = intensity; break;
			case RightButton:
				cmap[i].blue = intensity; break;
		    }
		else {
		    cmap[i].red = cmap[i].blue = cmap[i].green = intensity;
		}
	}
	return;
}

initcmap(ncolors,cmap,pixel,nplanes,flags)
     int ncolors, nplanes, pixel;
     Color *cmap;
     unsigned short flags;
{
     int i;

/* Initialize color map to slope 1, intercept 0 */
     for (i = 0; i < ncolors; i++) {
	cmap[i].pixel = pixel + i;
	cmap[i].red = cmap[i].green = cmap[i].blue =
	    ( (flags & VOP_Inverse) ? (65535 - (i << (16 - nplanes))) :
	      (i << (16 - nplanes)));
     }
     return;
}
