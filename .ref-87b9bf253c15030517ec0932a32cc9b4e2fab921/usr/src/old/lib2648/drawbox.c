/*	drawbox.c	4.1	83/03/09	*/
/*
 * Draw a box around a window.  The lower left corner of the box is at (r, c).
 * Color is 1 for drawing a box, 0 for erasing.
 * The box is nrow by ncol.
 */

#include "2648.h"

drawbox(r, c, color, nrow, ncol)
int r, c, color, nrow, ncol;
{
	if (color)
		setset();
	else
		setclear();
	move(c, r);
	draw(c+ncol-1, r);
	draw(c+ncol-1, r+nrow-1);
	draw(c, r+nrow-1);
	draw(c, r);
}
