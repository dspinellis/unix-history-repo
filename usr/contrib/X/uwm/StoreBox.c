#ifndef lint
static char *rcsid_StoreBox_c = "$Header: StoreBox.c,v 10.3 86/02/01 16:23:53 tony Rel $";
#endif	lint

/************************************************************************
 *									*
 *			Copyright (c) 1986 by				*
 *		Digital Equipment Corporation, Maynard, MA		*
 *		         All Rights Reserved.				*
 *									*
 *	Permission to use, copy, modify, and distribute this software	*
 *	and its documentation is hereby granted only to licensees of 	*
 *	The Regents of the University of California pursuant to their	*
 *	license agreement for the Berkeley Software Distribution 	*
 *	provided that the following appears on all copies.		*
 *									*
 *            "LICENSED FROM DIGITAL EQUIPMENT CORPORATION		*
 *                      COPYRIGHT (C) 1986				*	
 *                 DIGITAL EQUIPMENT CORPORATION			*
 *                         MAYNARD, MA					*
 *                     ALL RIGHTS RESERVED.				*
 *									*
 *      THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT	* 
 *	NOTICE AND SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL	*
 *	EQUIPMENT CORPORATION.  DIGITAL MAKES NO REPRESENTATIONS	*
 *	ABOUT SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE. IT IS	*
 *	SUPPLIED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.		*
 *									*	
 * 	IF THE UNIVERSITY OF CALIFORNIA OR ITS LICENSEES MODIFY 	*	
 *	THE SOFTWARE IN A MANNER CREATING DERIVATIVE COPYRIGHT 		*	
 *	RIGHTS APPROPRIATE COPYRIGHT LEGENDS MAY BE PLACED ON THE	*
 *	DERIVATIVE WORK IN ADDITION TO THAT SET FORTH ABOVE."		*	
 *									*
 ************************************************************************/
 

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
