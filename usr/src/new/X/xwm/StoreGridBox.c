#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 *	StoreGridBox - This subroutine is used by the X Window Manager (xwm)
 *	to store the vertices for the movement resize grid and box in a
 *	vertex list.
 *
 *	File:		StoreGridBox.c
 */

#ifndef lint
static char *rcsid_StoreGridBox_c = "$Header: StoreGridBox.c,v 10.3 86/02/01 16:10:17 tony Rel $";
#endif

#include "xwm.h"

/*
 * Store the vertices for the movement resize grid and box in a vertex list.
 */
int StoreGridBox(box, ulx, uly, lrx, lry)
    register Vertex box[];
    register int ulx;		/* Upper left X coordinate. */
    register int uly;		/* Upper left Y coordinate. */
    register int lrx;		/* Lower right X coordinate. */
    register int lry;		/* Lower right Y coordinate. */
{
    register int value;
    int third;

    /*
     * Xor box in.
     */
    box[0].x = ulx;  box[0].y = uly;  box[0].flags = VertexDontDraw;
    box[1].x = lrx;  box[1].y = uly;
    box[2].x = lrx;  box[2].y = lry;
    box[3].x = ulx;  box[3].y = lry;
    box[4].x = ulx;  box[4].y = uly;

    /*
     * Xor grid in.
     */
    box[5].x = ulx;     box[5].flags = VertexDontDraw;
    box[6].x = lrx;
    box[7].x = ulx;     box[7].flags = VertexDontDraw;
    box[8].x = lrx;
    box[9].y = uly;     box[9].flags = VertexDontDraw;
    box[10].y = lry;
    box[11].y = uly;    box[11].flags = VertexDontDraw;
    box[12].y = lry;

    /*
     * Xor out.
     */
    box[13].x = ulx;  box[13].y = uly;  box[13].flags = VertexDontDraw; 
    box[14].x = lrx;  box[14].y = uly;
    box[15].x = lrx;  box[15].y = lry;
    box[16].x = ulx;  box[16].y = lry;
    box[17].x = ulx;  box[17].y = uly;

    /*
     * Xor grid out.
     */
    box[18].x = ulx;    box[18].flags = VertexDontDraw;
    box[19].x = lrx;
    box[20].x = ulx;    box[20].flags = VertexDontDraw; 
    box[21].x = lrx;
    box[22].y = uly;    box[22].flags = VertexDontDraw; 
    box[23].y = lry;
    box[24].y = uly;    box[24].flags = VertexDontDraw;
    box[25].y = lry;

    /*
     * These are the X and Y calculations for the parts of the grid that
     * are dependent the division by 3 calculations.
     */

    /*
     * Y dimension third.
     */
    third = (lry - uly) / 3;

    value = uly + third;
   
    box[5].y = value; 
    box[6].y = value; 
    box[18].y = value; 
    box[19].y = value;

    value += third;
    
    box[7].y = value; 
    box[8].y = value; 
    box[20].y = value; 
    box[21].y = value;

    /*
     * X dimension third.
     */
    third = (lrx - ulx) / 3;

    value = ulx + third;
    
    box[9].x = value; 
    box[10].x = value; 
    box[22].x = value; 
    box[23].x = value;

    value += third;
    
    box[11].x = value; 
    box[12].x = value; 
    box[24].x = value; 
    box[25].x = value;

    /*
     * Total number of vertices is 26.
     */
    return(26);
}

