/* fill.c	Perform a simple raster operation a section of the screen
 *
 *	PixFill		Do a function on the screen
 *	StippleFill	Fill rectangle with a stipple pattern
 *
 *	Modification History
 *
 *	Carver 8510.25 Removed error checking in calls to copyrmsk
 *		       and copybmsk.  No errors are ever return.
 *
 */

#include "ddxqvss.h"
#include "qvss.h"
#include "vstagbl.h"

extern BITMAP pbm;

/*ARGSUSED*/
PixFill (srcpix, xymask, dstx, dsty, width, height, clips, clipcount,
	 func, zmask)
	int srcpix, dstx, dsty, width, height, clipcount, zmask;
	register BITMAP *xymask;
	register int func;
	CLIP *clips;
{
	int constant = srcpix & 1;

	if (!(zmask & 1)) {
	    return;
	}
	if (xymask == 0)
	  {
	  copyrmsk (VSTA$K_SRC_CONST, constant, 0, 0, 0, 0,
			  width, height, (short *) pbm.data,
			  pbm.width, pbm.height, dstx, dsty, func,
			  clipcount, clips);
	  return;
	  }

	copybmsk (VSTA$K_SRC_CONST, constant, 0, 0, 0, 0,
			(short *) xymask->data, xymask->width, 
			xymask->height, 0, 0, width, height, 
			(short *) pbm.data,
			pbm.width, pbm.height, dstx, dsty, func,
			clipcount, clips);
	return;
}

extern int errno;
#include <errno.h>

int StippleFill (srcpix, xoff, yoff, stipmask, dstx, dsty, width, height,
	clips, clipcount, func, zmask)
	int srcpix;		/* source pixel */
	int xoff, yoff;		/* stipple origin */
	BITMAP *stipmask;	/* stipple mask */
	int dstx, dsty;		/* destination */
	int width, height;
	CLIP *clips;		/* clipping rectangles */
	int clipcount;
	int func;		/* GX display function */
	int zmask;		/* plane mask */
{
    	static char funcmap[16][2] = {
		{GXandInverted,	GXandInverted},	/* GXclear */
		{GXandInverted, GXnoop},	/* GXand */
		{GXandInverted,	GXxor},		/* GXandReverse */
		{GXandInverted, GXor},		/* GXcopy */
		{GXnoop,	GXandInverted},	/* GXandInverted */
		{GXnoop,	GXnoop},	/* GXnoop */
		{GXnoop,	GXxor},		/* GXxor */
		{GXnoop,	GXor},		/* GXor */
		{GXxor,		GXandInverted},	/* GXnor */
		{GXxor,		GXnoop},	/* GXequiv */
		{GXxor,		GXxor},		/* GXinvert */
		{GXxor,		GXor},		/* GXorReverse */
		{GXor,		GXandInverted},	/* GXcopyInverted */
		{GXor,		GXnoop},	/* GXorInverted */
		{GXor,		GXxor},		/* GXnand */
		{GXor,		GXor}		/* GXset */
	};
	int newfunc = funcmap [func][srcpix & 1];
		
	if (!(zmask & 1)) {
	    return (1);
	}
	if ((stipmask->width != 16) || (stipmask->height != 16)) {
		errno = EINVAL;
		return (0);
	}
	copyrmsk (VSTA$K_SRC_HT_BITMAP, (short *)stipmask->data, 
		stipmask->width, stipmask->height, xoff, yoff,
		width, height,
		(short *)pbm.data, pbm.width, pbm.height,
		dstx, dsty, newfunc, clipcount, clips);
	return (1);
}
