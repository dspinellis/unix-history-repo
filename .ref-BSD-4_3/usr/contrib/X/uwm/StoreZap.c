#ifndef lint
static char *rcsid_StoreZap_c = "$Header: StoreZap.c,v 10.3 86/02/01 16:24:04 tony Rel $";
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
static char *sccsid = "@(#)StoreZap.c	3.8	1/24/86";
#endif
/*
 *	StoreZap - This subroutine is used by the X Window Manager (xwm)
 *	to store the vertices for the iconify, uniconify zap.
 *
 */

#include "uwm.h"

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
