/************************************************************************/
/************************************************************************/
/*									*/
/*	 DDX DRAW CURVE INTERFACE					*/
/*									*/
/*	written by : MATT CORKUM  09-12-85				*/
/*      modified 09-23-85:  make it look like the ddx interface		*/
/*									*/
/*									*/
/*	 The ddx draw curve interface code				*/
/*									*/
/*	MODIFICATION HISTORY						*/
/*									*/
/*	Carver  8510.21 Fixed "bwidth, bwidth" to be "bwidth, bheight"  */
/*									*/
/*	Carver  8510.21 Put in single plane code.			*/
/*									*/
/*	Carver  8510.09 Fixed "if (mode = 1)" to be "if (mode == 1)"	*/
/*									*/
/*	Carver  8510.03 Bad idea... after a few infinite loops we don't */
/*   			ignore errors returned by the path list		*/
/*			converter anymore.				*/
/*									*/
/*	Carver	8509.25	Removed error handling code			*/
/*									*/
/*									*/
/************************************************************************/
/************************************************************************/

/* THERE IS A CONFLICT BETWEEN ddxqvss.h and qvss.h (wpitch) SO MAKE SURE
   TO INCLUDE ddxqvss.h FIRST */

#include "ddxqvss.h"
#include "qvss.h"
#include "extern.h"
#include "vstagbl.h"

extern BITMAP pbm;



DrawCurve( verts, vertcnt, xbase, ybase, srcpix, altpix, mode, bwidth, bheight,
	    pat, patlen, patmult, clips, clipcount, func, zmask )

Vertex *verts; 			/* vertexes and flags	   	*/
int	vertcnt;		/* vertex count 	   	*/
int     xbase;			/* destination offset in x      */
int     ybase;			/* destination offset in y      */
int	srcpix;			/* source pixel		   	*/
int     altpix;			/* alternate source pixel  	*/
int     mode;			/* 0:solid 1:dashed 2:patterned */
int	bwidth;			/* brush width			*/
int	bheight;		/* brush height			*/
int 	pat;			/* pattern			*/
int	patlen;			/* pattern length		*/
int 	patmult;		/* pattern repeat count		*/
CLIP    *clips;			/* clipping rectangles		*/
int 	clipcount;		/* count of clipping rectangles */
int 	func;			/* GX display function 		*/
int	zmask;			/* plane mask			*/

{
	long	error;				/* error value to be returned*/
    	char    s_pixel_flag;			/* single pixel flag         */
	short   *newvert;			/* new vertex list 	     */
	int     newvertcnt;			/* new vertex count	     */


	/* LIMIT THE DRAW OPERATION TO ONE PLANE */

	if ((zmask & 1) == 0)
		return;

	srcpix = srcpix & 1;

	altpix = altpix & 1;
	
	/* pre-process the specified path list ..... create another one 
	   containing only absolute straight line drawing */


	error = path_list_converter ( &verts, &vertcnt, xbase, ybase,
					&newvert, &newvertcnt );

	if ( error )
		{
		DeviceError ("DrawCurve failure\n");
		};

	/* are we in single pixel mode or not ? */

	if ( (bwidth == 1) && (bheight == 1)) s_pixel_flag = 1;
				else         s_pixel_flag = 0;

	if ((mode == 0) && (s_pixel_flag)  ) /* solid single pixel */

		/* call the solid single pixel draw command */


		error = draw_cons_solid_spix_line(srcpix, (short *)pbm.data, 
			pbm.width, pbm.height,  func, newvertcnt, 
			newvert, clipcount, clips);


	else if (mode == 0 ) /* solid mode */


		/* s_pixel_flag = 0 ( multiple pixel )     */
		/* call the solid multi-pixel draw command */


		error = draw_cons_solid_mpix_line(srcpix, bwidth, bheight, 
			  (short *)pbm.data, pbm.width, pbm.height, 
			  func, newvertcnt, newvert, clipcount, clips);

	else if ((mode == 1) && ( s_pixel_flag ) ) /* dashed mode */


		/* call dashed single pixel draw curve */

		error = draw_cons_pat_spix_line(srcpix, (short *)pbm.data, 
			  pbm.width, pbm.height, func, 
			  newvertcnt, newvert, patlen, pat,
    	    	    	  patmult, 0, 0, clipcount, clips);

	else if (mode == 1) /* dashed mode */

		/* s_pixel_flag = 0 ( multiple pixel )     */
		/* call multiple pixel dashed draw command */

		error = draw_cons_pat_mpix_line(srcpix, bwidth, bheight, 
			  (short *)pbm.data, pbm.width, pbm.height, 
			  func, newvertcnt, newvert, patlen, pat, 
			  patmult, 0, 0, clipcount, clips);

	else if ((mode == 2) && (s_pixel_flag) ) /* patterned mode */
		{

		error = draw_2_src_cons_spix_line(srcpix, altpix, 
			  (short *)pbm.data, pbm.width, pbm.height, 
			  func, newvertcnt, newvert, patlen, pat, 
			  patmult, 0, 0, clipcount, clips); 

		} /* end of patterned single pixel */


	else if (mode == 2)  /* patterned lines */
               {

			error = draw_2_src_mpix_line(srcpix, altpix, 
			  bwidth, bheight, (short *)pbm.data, pbm.width, 
			  pbm.height, func, newvertcnt, 
			  newvert, patlen, pat, patmult, 
			  0, 0, clipcount, clips);

		}
	
	}
