/* copy.c	Copy one section of the framebuffer to another
 *
 *	CopyArea	Copies a section of the framebuffer
 *
 *	Modification History
 *
 *	Carver 8510.25 Removed error checking in calls to copyrmsk
 *		       and copybmsk.  No errors are ever return.
 */

#include "ddxqvss.h"
#include "qvss.h"
#include "vstagbl.h"

extern BITMAP pbm;

CopyArea (srcx, srcy, width, height, dstx, dsty, clips, clipcount, func, zmask)
	int srcx, srcy, width, height, dstx, dsty, clipcount, zmask;
	int func;
	CLIP *clips;
{
	register short *saddr = (short *) pbm.data;
	register int w = pbm.width;
	register int h = pbm.height;

	if (!(zmask & 1)) return;

	copyrmsk(VSTA$K_SRC_BITMAP, saddr, w, h,
		srcx, srcy, width, height,
		saddr, w, h,
		dstx, dsty, func, clipcount, clips);
}
