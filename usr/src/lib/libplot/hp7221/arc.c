/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)arc.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "hp7221.h"

/* 
 * 7221 requires knowing the anlge of arc.  To do this, the triangle formula
 *	c^2 = a^2 + b^2 - 2*a*b*cos(angle)
 * is used where "a" and "b" are the radius of the circle and "c" is the
 * distance between the beginning point and the end point.
 *
 * This gives us "angle" or angle - 180.  To find out which, draw a line from
 * beg to center.  This splits the plane in half.  All points on one side of the
 * plane will have the same sign when plugged into the equation for the line.
 * Pick a point on the "right side" of the line (see program below).  If "end"
 * has the same sign as this point does, then they are both on the same side
 * of the line and so angle is < 180.  Otherwise, angle > 180.
 */
   
#define side(x,y)	(a*(x)+b*(y)+c > 0.0 ? 1 : -1)

arc(xcent,ycent,xbeg,ybeg,xend,yend)
int xcent,ycent,xbeg,ybeg,xend,yend;
{
	double radius2, c2;
	double a,b,c;
	int angle;

	/* Probably should check that this is really a circular arc.  */
	radius2 = (xcent-xbeg)*(xcent-xbeg) + (ycent-ybeg)*(ycent-ybeg);
	c2 = (xend-xbeg)*(xend-xbeg) + (yend-ybeg)*(yend-ybeg);
	angle = (int) ( 180.0/PI * acos(1.0 - c2/(2.0*radius2)) + 0.5 );

	a = (double) (ycent - ybeg);
	b = (double) (xcent - xbeg);
	c = (double) (ycent*xbeg - xcent*ybeg);
	if (side(xbeg + (ycent-ybeg), ybeg - (xcent-xbeg)) != side(xend,yend))
		angle += 180;
	
	move(xcent, ycent);
	/* Not quite implemented...
	printf("C(A%d c)[%d,%d]", angle, xbeg, ybeg);
	*/
}
