#ifndef lint
static char *rcsid_StoreBox_c = "$Header: StoreBox.c,v 10.4 86/11/19 16:24:44 jg Rel $";
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
static char *sccsid = "@(#)StoreBox.c	3.8	1/24/86";
#endif
/*
 *	StoreBox - This subroutine is used by the X Window Manager (xwm)
 *	to store the vertices for the resize / movement box in a vertex list.
 */

#include "uwm.h"

/*
 * Store the vertices for the resize movement box in a vertex list.
 */
int StoreBox(box, ulx, uly, lrx, lry)
register Vertex box[];
int ulx;			/* Upper left X coordinate. */
int uly;			/* Upper left Y coordinate. */
int lrx;			/* Lower right X coordinate. */
int lry;			/* Lower right Y coordinate. */
{
    /*
     * Xor in.
     */
    box[0].x = box[3].x = box[4].x = ulx;
    box[0].y = box[1].y = box[4].y = uly;

    box[1].x = box[2].x = lrx;
    box[2].y = box[3].y = lry;

    box[0].flags = VertexDontDraw;

    /*
     * If we freeze the screen, don't bother to xor out.
     */
    if (Freeze)
        return(5);

    /*
     * Xor out.
     */
    box[7].x = box[8].x = ulx;
    box[5].y = box[8].y = uly;

    box[5].x = box[6].x = lrx;
    box[6].y = box[7].y = lry;

    /*
     * Total number of vertices is 9.
     */
    return(9);
}
