/************************************************************************/
/************************************************************************/
/*									*/
/*	 DDX FILL INTERFACE						*/
/*									*/
/*	written by : MATT CORKUM  09-25-85				*/
/*									*/
/*									*/
/*	 The ddx fill interface code					*/
/*									*/
/*									*/
/*	Modification History:						*/
/*									*/
/*	8511.04 (Carver) When filling with constant make sure to mask	*/
/*			 srcpix to one bit.				*/
/*									*/
/*	8509.30 (Corkum) Make the code work in a test environment.	*/
/*									*/
/*									*/
/************************************************************************/
/************************************************************************/

#include "ddxqvss.h"
#include "qvss.h"
#include "extern.h"
#include "vstagbl.h"

extern BITMAP pbm;



/*ARGSUSED*/
DrawFilled (verts, vertcount, xbase, ybase, srcpix, tile, xoff, yoff,
	    clips, clipcount, func, zmask)
	Vertex *verts;
	register PIXMAP *tile;
	int vertcount, xbase, ybase, srcpix, xoff, yoff, clipcount, zmask;
	register int func;
	CLIP *clips;
{

	long	error;				/* error value to be returned*/
	short   *newvert;			/* new vertex list 	     */
	int     newvertcnt;			/* new vertex count	     */
	int     src, src_type;			/* source information 	     */

	if (!(zmask & 0x0001)) return;


	/* pre-process the specified path list ..... create another one 
	   containing only absolute straight line drawing */

	/* for a fill operation the destination offsets are zero, however we
		call the pathlist converter routine to make an absolute,
		(pertaining to not relative), and straight line pathlist */

	error = path_list_converter( &verts, &vertcount, xbase, ybase, 
				     &newvert, &newvertcnt );

	if ( error )

		return (error);  

	if ( tile == 0 )   /* if no halftone pattern exists then use the
				constant source to perform the fill */

		{

		src_type = VSTA$K_SRC_CONST;

		error = fillhalf ( 
				src_type, (srcpix & 1), 0, 0,  0, 0,  
			        (short *)pbm.data, pbm.width, pbm.height, 
				0, 0, func, newvertcnt, newvert, clipcount, 
				clips);

		}		
		
	else if (PTYPE(tile) == BitmapPixmap) /* fill with a halftone */
	
		{

		register BITMAP *bm = (BITMAP *) tile->data;
		extern char SSMap[];

		src_type  = VSTA$K_SRC_HT_BITMAP;

        	func = SSMap[ func | (tile->kind & InvertFlag)];

		error = fillhalf ( src_type, (short *)bm->data, 
			bm->width, bm->height, xoff, yoff,
		        (short *)pbm.data, pbm.width, pbm.height, 
			0, 0, func, newvertcnt, 
			newvert, clipcount, clips);

		}

	else /* the pixmap is actually a constant source */

	        {
		
		src_type = VSTA$K_SRC_CONST;

		error = fillhalf ( 
				src_type, tile->data, 0, 0,  0, 0,  
			        (short *)pbm.data, pbm.width, pbm.height, 
				0, 0, func, newvertcnt, newvert, clipcount, 
				clips);

		}


}
