/*
 *	$Source: /u1/X/libis/RCS/copy.c,v $
 *	$Header: copy.c,v 1.1 86/11/17 14:33:18 swick Rel $
 */

#ifndef lint
static char *rcsid_copy_c = "$Header: copy.c,v 1.1 86/11/17 14:33:18 swick Rel $";
#endif	lint

#include "is-copyright.h"

/*	copy.c		Copy one section of the framebuffer to another
 *
 *	CopyArea	Copies a section of the framebuffer
 *
 *	Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include "Xis.h"

CopyArea(srcx, srcy, width, height, dstx, dsty, clips, clipcount, func, zmask)
register int	srcx, srcy;
int		width, height;
register int	dstx, dsty;
register CLIP	*clips;
register int	clipcount;
int		func;
int		zmask;
{
    CLIP dbounds;

#ifdef DEBUG
if (debug & D_CopyArea)
    printf("CopyArea(srcx=%d, srcy=%d, width=%d, height=%d, dstx=%d, dsty=%d, clips=0x%x, clipcount=%d, func=%d, zmask=0x%04x)\n",
	srcx, srcy, width, height, dstx, dsty, clips, clipcount, func, zmask);
#endif DEBUG

    dbounds.top = dsty;
    dbounds.left = dstx;
    dbounds.width = width;
    dbounds.height = height;

    for ( ; clipcount; clipcount--, ++clips) {
	if (Overlap(clips[0], dbounds)) {
	    CLIP i, sbounds;
	    i = Intersection(clips[0], dbounds);
	    CheckCursor(i);
	    sbounds.left = (i.left - dstx) + srcx;
	    sbounds.top = (i.top - dsty) + srcy;
	    sbounds.width = i.width;
	    sbounds.height = i.height;
	    CheckCursor(sbounds);
	    GIP_RasterOp((unsigned char)func,
		&ScreenPixmap, sbounds.left, sbounds.top,
		&ScreenPixmap, i.left, i.top,
		(BITMAP *)NULL, 0, 0,
		i.width, i.height, zmask);
	}

    }
    RestoreCursor();
}
