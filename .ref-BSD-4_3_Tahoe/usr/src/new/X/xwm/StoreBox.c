#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 *	StoreBox - This subroutine is used by the X Window Manager (xwm)
 *	to store the vertices for the resize / movement box in a vertex list.
 *
 *	File:		StoreBox.c
 */

#ifndef lint
static char *rcsid_StoreBox_c = "$Header: StoreBox.c,v 10.3 86/02/01 16:10:07 tony Rel $";
#endif

#include "xwm.h"

/*
 * Store the vertices for the resize movement box in a vertex list.
 */
int StoreBox(box, ulx, uly, lrx, lry)
    register Vertex box[];
    register int ulx;		/* Upper left X coordinate. */
    register int uly;		/* Upper left Y coordinate. */
    register int lrx;		/* Lower right X coordinate. */
    register int lry;		/* Lower right Y coordinate. */
{
    /*
     * Xor in.
     */
    box[0].x = ulx;  box[0].y = uly;  box[0].flags = VertexDontDraw;
    box[1].x = lrx;  box[1].y = uly;
    box[2].x = lrx;  box[2].y = lry;
    box[3].x = ulx;  box[3].y = lry;
    box[4].x = ulx;  box[4].y = uly;

    /*
     * Xor out.
     */
    box[5].x = lrx;  box[5].y = uly;
    box[6].x = lrx;  box[6].y = lry;
    box[7].x = ulx;  box[7].y = lry;
    box[8].x = ulx;  box[8].y = uly;

    /*
     * Total number of vertices is 9.
     */
    return(9);
}

