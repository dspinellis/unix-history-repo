#ifndef lint
static char *rcsid_StoreGridBox_c = "$Header: StoreGridBox.c,v 10.4 86/11/19 16:24:52 jg Rel $";
#endif	lint

/*
 *			COPYRIGHT 1985, 1986
 *		   DIGITAL EQUIPMENT CORPORATION
 *		       MAYNARD, MASSACHUSETTS
 *			ALL RIGHTS RESERVED.
 *
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE AND
 * SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL EQUIPMENT CORPORATION.
 * DIGITAL MAKES NO REPRESENTATIONS ABOUT THE SUITIBILITY OF THIS SOFTWARE FOR
 * ANY PURPOSE.  IT IS SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 *
 * IF THE SOFTWARE IS MODIFIED IN A MANNER CREATING DERIVATIVE COPYRIGHT RIGHTS,
 * APPROPRIATE LEGENDS MAY BE PLACED ON THE DERIVATIVE WORK IN ADDITION TO THAT
 * SET FORTH ABOVE.
 *
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting documentation,
 * and that the name of Digital Equipment Corporation not be used in advertising
 * or publicity pertaining to distribution of the software without specific, 
 * written prior permission.
 *
 */


/*
 * MODIFICATION HISTORY
 *
 * 000 -- M. Gancarz, DEC Ultrix Engineering Group
 */

#ifndef lint
static char *sccsid = "@(#)StoreGridBox.c	3.8	1/24/86";
#endif
/*
 *	StoreGridBox - This subroutine is used by the X Window Manager (xwm)
 *	to store the vertices for the movement resize grid and box in a
 *	vertex list.
 */

#include "uwm.h"

/*
 * Store the vertices for the movement resize grid and box in a vertex list.
 */
int StoreGridBox(box, ulx, uly, lrx, lry)
register Vertex box[];
int ulx;			/* Upper left X coordinate. */
int uly;			/* Upper left Y coordinate. */
int lrx;			/* Lower right X coordinate. */
int lry;			/* Lower right Y coordinate. */
{
    register int xthird, ythird;
    int x1third, y1third;
    int x2third, y2third;

    /*
     * Xor in.
     */
    box[0].x = box[3].x = box[4].x = box[5].x = box[7].x = ulx;
    box[1].x = box[2].x = box[6].x = box[8].x = lrx;

    box[0].y = box[1].y = box[4].y = box[9].y = box[11].y = uly;
    box[2].y = box[3].y = box[10].y = box[12].y = lry;

    box[0].flags = box[5].flags = box[7].flags = box[9].flags =
        box[11].flags = VertexDontDraw;

    /*
     * These are the X and Y calculations for the parts of the grid that
     * are dependent on the division by 3 calculations.
     */

    /*
     * Y dimension third.
     */
    ythird = (lry - uly) / 3;
    y1third = uly + ythird;
    y2third = y1third + ythird;
   
    /*
     * X dimension third.
     */
    xthird = (lrx - ulx) / 3;
    x1third = ulx + xthird;
    x2third = x1third + xthird;

    /*
     * Stash first set of vertices.
     */
    box[5].y = box[6].y = y1third;
    box[7].y = box[8].y = y2third;

    box[9].x = box[10].x = x1third;
    box[11].x = box[12].x = x2third; 

    /*
     * Do not erase if we're freezing the screen.
     */
    if (Freeze)
        return(13);

    /*
     * From here on we're retracing the vertices to clear the
     * grid using GXxor.
     */
    box[18].y = box[19].y = y1third;
    box[20].y = box[21].y = y2third;

    box[22].x = box[23].x = x1third;
    box[24].x = box[25].x = x2third;

    box[13].x = box[16].x = box[17].x = box[18].x = box[20].x = ulx;
    box[13].y = box[14].y = box[17].y = box[22].y = box[24].y = uly;

    box[14].x = box[15].x = box[19].x = box[21].x = lrx;
    box[15].y = box[16].y = box[23].y = box[25].y = lry;

    box[13].flags = box[18].flags = box[20].flags = box[22].flags =
        box[24].flags = VertexDontDraw;


    /*
     * Total number of vertices is 26.
     */
    return(26);
}
