#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 *	StoreZap - This subroutine is used by the X Window Manager (xwm)
 *	to store the vertices for the iconify, uniconify zap.
 *
 *	File:		StoreZap.c
 */

#ifndef lint
static char *rcsid_StoreZap_c = "$Header: StoreZap.c,v 10.3 86/02/01 16:10:21 tony Rel $";
#endif

#include "xwm.h"

/*
 * Store the vertices for the zap effect.
 */
int StoreZap(zap, ulx_1, uly_1, lrx_1, lry_1, ulx_2, uly_2, lrx_2, lry_2)
    register Vertex zap[];
    int ulx_1;		/* From window, upper left X coordinate. */
    int uly_1;		/* From window, upper left Y coordinate. */
    int lrx_1;		/* From window, lower right X coordinate. */
    int lry_1;		/* From window, lower right Y coordinate. */
    int ulx_2;		/* To window, upper left X coordinate. */
    int uly_2;		/* To window, upper left Y coordinate. */
    int lrx_2;		/* To window, lower right X coordinate. */
    int lry_2;		/* To window, lower right Y coordinate. */
{

    /*
     * Xor in.
     */
    zap[0].x = ulx_1;	zap[0].y = uly_1;	zap[0].flags = VertexDontDraw;
    zap[1].x = ulx_2;	zap[1].y = uly_2;
    zap[2].x = lrx_1;	zap[2].y = uly_1;	zap[2].flags = VertexDontDraw;
    zap[3].x = lrx_2;	zap[3].y = uly_2;
    zap[4].x = lrx_1;	zap[4].y = lry_1;	zap[4].flags = VertexDontDraw;
    zap[5].x = lrx_2;	zap[5].y = lry_2;
    zap[6].x = ulx_1;	zap[6].y = lry_1;	zap[6].flags = VertexDontDraw;
    zap[7].x = ulx_2;	zap[7].y = lry_2;

    /*
     * Xor out.
     */
    zap[8].x = ulx_1;	zap[8].y = uly_1;	zap[8].flags = VertexDontDraw;
    zap[9].x = ulx_2;	zap[9].y = uly_2;
    zap[10].x = lrx_1;	zap[10].y = uly_1;	zap[10].flags = VertexDontDraw;
    zap[11].x = lrx_2;	zap[11].y = uly_2;
    zap[12].x = lrx_1;	zap[12].y = lry_1;	zap[12].flags = VertexDontDraw;
    zap[13].x = lrx_2;	zap[13].y = lry_2;
    zap[14].x = ulx_1;	zap[14].y = lry_1;	zap[14].flags = VertexDontDraw;
    zap[15].x = ulx_2;	zap[15].y = lry_2;

    /*
     * Total number of vertices is 16.
     */
    return(16);
}
