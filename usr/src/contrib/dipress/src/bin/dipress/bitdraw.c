
/******************************************************************************
 *  bitdraw - 	implements the bitmap drawing primitives necessary to turn
 *		ditroff draw commands into a pixel vector for interpress 
 *
 *
 *	John Mellor-Crummey (Xerox Corp)
 *
 * 	Copyright (c) 1985, 1986 Xerox Corp.
 *
 ******************************************************************************/


#include <math.h>
#include <stdio.h>
#include <ctype.h>

#include "defs.h" 
#include "externs.h" 


/*------------------------------------------------------------------------------
 *	bitmapDrawCircle	incrementally compute the points for one octant 
 *				of a circle and complete the figure by 
 *				reflecting the points into each of the octants.
 *
 *		based on an algorithm by J. Michener from
 *		Fundamentals of Interactive Computer Graphics,
 *		Foley & Van Dam, 1982, p. 445
 *----------------------------------------------------------------------------*/
bitmapDrawCircle(d)
int d;
{
	int x,y,xc,yc,delta;

	xc = hor_pos + d/2;
	yc = ver_pos;
	delta = 3 - d;
	x = 0;
	y = d/2;
	while ( x < y)
	{
		octPlot(x,y,xc,yc);
		if (delta < 0) 
			delta = delta + 4 * x + 6;
		else
		{
			delta = delta + 4 * (x - y) + 10;
			y--;
		}
		x++;
	}
	if (x == y) octPlot(x,y,xc,yc);
	hMov(xc + d/2);
	vMov(yc);
}


/*------------------------------------------------------------------------------
 *	octPlot		reflect the point  x,y into each of the eight
 *			octants centered about xc,yc and set the pixels
 *			in a bitmap
 *----------------------------------------------------------------------------*/
octPlot(x,y,xc,yc)
int x,y,xc,yc;
{
	vMov(yc + y); 
	hMov(xc + x); setpixel();
	hMov(xc - x); setpixel();
	vMov(yc - y); setpixel();
	hMov(xc + x); setpixel();
	vMov(yc + x); 
	hMov(xc + y); setpixel();
	hMov(xc - y); setpixel();
	vMov(yc - x); setpixel();
	hMov(xc + y); setpixel();
}

/*------------------------------------------------------------------------------
 *	bitmapDrawEllipse	incrementally compute the points for one 
 *				quadrant of an ellipse and complete the figure  
 *				by reflecting the points into each of the 
 *				quadrants.
 *----------------------------------------------------------------------------*/
bitmapDrawEllipse(xdiam,ydiam)
int xdiam,ydiam;
{
	int x,y,xc,yc,a,b;
	int fourAsq,fourAsqY;
	int sixBsq,twoBsq,fourBsq,fourBsqX;
	int d;
	int midpoint;

	a = xdiam / 2;
	b = ydiam / 2;
	xc = hor_pos + a;
	yc = ver_pos;

	x = 0;
	y = b;
	fourAsq = a * a * 4;
	twoBsq = b * b * 2;
	fourBsq = twoBsq + twoBsq;
	sixBsq = fourBsq + twoBsq;
	fourAsqY = fourAsq * y;
	fourBsqX = 0;
	midpoint = a * a * sqrt((double) 1.0 / (a * a + b * b));
	d = twoBsq +  a * a * (2 * b  + 1);

	while(x < midpoint)
	{
		quadPlot(x,y,xc,yc);
		if (d > 0) /* case 2 -> y-- */
		{
			d += fourAsq - fourAsqY;
			fourAsqY -= fourAsq;
			y--;
		}
		d += fourBsqX + sixBsq;
		fourBsqX += fourBsq;
		x++;
	}
	d -= twoBsq * x - b * b;
	while(y >= 0)
	{
		quadPlot(x,y,xc,yc);
		if (d > 0) /* case 3 -> x++ */
		{
			d += fourBsqX + sixBsq;
			fourBsqX += fourBsq;
			x++;
		}
		d += fourAsq - fourAsqY;
		fourAsqY -= fourAsq;
		y--;
	}
	hMov(xc + a);
	vMov(yc);
}


/*------------------------------------------------------------------------------
 *	quadPlot	reflect the point  x,y into each of the four
 *			quadrants centered about xc,yc and set the pixels
 *			in a bitmap
 *----------------------------------------------------------------------------*/
quadPlot(x,y,xc,yc)
int x,y,xc,yc;
{
	vMov(yc + y); 
	hMov(xc + x); setpixel();
	hMov(xc - x); setpixel();
	vMov(yc - y); setpixel();
	hMov(xc + x); setpixel();
}


/*------------------------------------------------------------------------------
 *	bitmapDrawCircle	incrementally draw a circular arc in a c
 *				counterclockwise direction. the arguments are 
 *				relative coordinates for the center point
 *				from the current point, and the termination 
 *				point from the center point.
 *
 *		based on an algorithm by J. Bresenham 
 *		A Linear Algorithm for Incremental Digital
 *		Display of Circular Arcs, Communications of the ACM, 
 *		Feb. 1977, pp. 103-104.
 *----------------------------------------------------------------------------*/
bitmapDrawArc(relxc,relyc,relxt,relyt)
int relxc,relyc,relxt,relyt;
{
	int xc,yc;
	int Xsprime,Ysprime,Xtprime,Ytprime;
	int Xshat,Yshat,Xthat,Ythat,Xs,Ys,Xt,Yt,Xi,Yi;
	int delta,deltai,deltaprime;
	int M1x,M1y,M2x,M2y,M3x,M3y;
	int q,qs,qt,qstar;
	int move;
	int xsave;
	int radius;
	double angle;
	int xplot,yplot;

	xc = hor_pos + relxc;
	yc = ver_pos + relyc;
	Xtprime =  hor_pos;
	Ytprime = ver_pos;
	Xsprime = relxt + xc;
	Ysprime = relyt + yc;

	/* get the radius from the start point */
	radius = hypot((double) relxc,(double) relyc);

	/* readjust start point to be sure it is on proper grid point */
	angle = atan2((double) (Ysprime - yc),(double) (Xsprime - xc));
	xplot = Xsprime = radius * cos(angle) + xc + .5;
	yplot = Ysprime = radius * sin(angle) + yc + .5;

	/* readjust termination point to be sure it is on proper grid point */
	angle = atan2((double) (Ytprime - yc),(double) (Xtprime - xc));
	Xtprime = radius * cos(angle) + xc + .5;
	Ytprime = radius * sin(angle) + yc + .5;

	/* compute start and end points of the arc as relative coordinates */
	Xshat = Xsprime - xc; 
	Yshat = Ysprime - yc;
	Xthat = Xtprime - xc; 
	Ythat = Ytprime - yc;

	/* implement the quadrant transforms to normalize to first quadrant 
	 * for both start and end points
	 */

	if (Xshat < 0)
	{
		if (Yshat < 0)
		{
			Xs =  abs(Yshat);
			Ys =  abs(Xshat);
			qs = 3;
			M1x = 0; M1y = -1;
			M2x = 1; M2y = -1;
			M3x = 1; M3y = 0;
		}
		else
		{
			Xs =  abs(Xshat);
			Ys =  abs(Yshat);
			qs = 2;
			M1x = -1; M1y =  0;
			M2x = -1; M2y = -1;
			M3x =  0; M3y = -1;
		}
	}
	else
	{
		if (Yshat < 0)
		{
			Xs =  abs(Xshat);
			Ys =  abs(Yshat);
			qs = 0;
			M1x = 1; M1y = 0;
			M2x = 1; M2y = 1;
			M3x = 0; M3y = 1;
		}
		else
		{
			Xs =  abs(Yshat);
			Ys =  abs(Xshat);
			qs = 1;
			M1x =  0; M1y =  1;
			M2x = -1; M2y = 1;
			M3x = -1; M3y = 0;
		}
	}

	if (Xthat < 0)
	{
		if (Ythat < 0)
		{
			Xt =  abs(Ythat);
			Yt =  abs(Xthat);
			qt = 3;
		}
		else
		{
			Xt =  abs(Xthat);
			Yt =  abs(Ythat);
			qt = 2;
		}
	}
	else
	{
		if (Ythat < 0)
		{
			Xt =  abs(Xthat);
			Yt =  abs(Ythat);
			qt = 0;
		}
		else
		{
			Xt =  abs(Ythat);
			Yt =  abs(Xthat);
			qt = 1;
		}
	}

	/* calculate number of quadrants */
	qstar = (4 + qt - qs) % 4;
	if ((qstar == 0) && (Xt <= Xs) && (Yt >= Ys))
		q = 3;
	else	q = qstar - 1;

	/* initialize for iteration */
	deltai = 2 * (Xs - Ys + 1);
	Xi = Xs;
	Yi = Ys;

	while(TRUE)
	{
		if ((q < 0) && (Xt <= Xi) && (Yt >= Yi))
			break;

		hMov(xplot);
		vMov(yplot);
		setpixel();

		if (Yi < 1)
		{
			xsave = Xi;
			Xi = - Yi;
			Yi = xsave;
			deltai = deltai - 4 * xsave;

			q = q - 1;
			M1x = M3x;
			M1y = M3y;

			xsave = M2x;
			M2x = - M2y;
			M2y = xsave;

			xsave = M3x;
			M3x = - M3y;
			M3y = xsave;

			continue;
		}
		if (deltai <= 0)
		{
			delta = 2 * (deltai + Yi) - 1;
			if (delta > 0) 
				move = M2;
			else 	move = M1;
		}
		else
		{
			deltaprime = 2 * (deltai - Xi) - 1;
			if (deltaprime > 0) 
				move = M3;
			else	move = M2;
		}

		switch(move)
		{
		case M1:
			Xi++;
			deltai = deltai + 2* Xi + 1;
			xplot += M1x;
			yplot += M1y;
			break;
		case M2:
			Xi++;
			Yi--;
			deltai = deltai + 2* (Xi - Yi) + 2;
			xplot += M2x;
			yplot += M2y;
			break;
		case M3:
			Yi--;
			deltai = deltai - 2 * Yi + 1;
			xplot += M3x;
			yplot += M3y;
			break;
		}
	}
}


/*------------------------------------------------------------------------------
 *	bitmapDrawWigglyLine	interpolate a curve between the sets of 
 *				relative points. the interpolation is done
 *				using a spline like method to produce a curve
 *				compatible with other output of ipic 
 *				(if the wiggly line should have arrowheads,
 *				ipic assumes that the wiggly line will pass
 *				close to the 2nd to last point in the curve
 *				when it computes the tilt of the arrowheads.
 *				For a smoother curve such as a 
 *				B-spline, the arrowheads will not be tilted 
 *				correctly as the spline is not guaranteed to 
 *				pass through the 2nd to last point)
 *----------------------------------------------------------------------------*/
bitmapDrawWigglyLine(s)
char *s;
{
	int x[maxPointsInSpline],y[maxPointsInSpline]; 
	int xi,yi,i,j,numPoints;
	float temp1,temp2,temp3,t,dis;
	float euclidDist();

	/* skip all leading white space */
	while(white(*s)) s++;		
	if(!isdigit(*s)) return; 

	/* read in the x y pairs of points for the spline */
	for(numPoints = 2; ((numPoints< maxPointsInSpline) && 
		(readNumber(&s,&x[numPoints]) != NULL) &&
		(readNumber(&s,&y[numPoints]) != NULL)); numPoints++);
	numPoints++;

	/* first point of curve is current point */
	x[1] = hor_pos;
	y[1] = ver_pos;

	/* turn relative points into absolute points */
	for (i = 2; i < numPoints; i++) 
	{
		x[i] += x[i-1];
		y[i] += y[i-1];
	}

	/* if the wiggle's ends meet, insure the curve meets */
	if ((x[1] == x[numPoints-1]) && (y[1] == y[numPoints-1]))
	{
		x[0] = x[numPoints-2];
		y[0] = y[numPoints-2];
		x[numPoints] = x[2];
		y[numPoints] = y[2];
	}
	else
	{
		x[0] = x[1];
		y[0] = y[1];
		x[numPoints] = x[numPoints-1];
		y[numPoints] = y[numPoints-1];
	}
	numPoints++;

	/* position next pointers to the start of spline */
	hMov((x[0] + x[1]) / 2);
	vMov((y[0] + y[1]) / 2);

	for (i = 0; i < numPoints - 2; i++) 
	{	
		dis = (euclidDist(x[i],y[i], x[i+1],y[i+1]) + 
		       euclidDist(x[i+1],y[i+1], x[i+2],y[i+2])) / 2;
		for(j=1;j<dis;j++)
		{	
			t = (float) j/dis;
			temp1 = 0.5 * t * t;
			temp2 = -temp1 - temp1 + t + 0.5;
			temp3 = temp1 - t + 0.5;

			xi = temp1 * x[i+2] + temp2 * x[i+1] + temp3 * x[i] + 0.5;
			yi = temp1 * y[i+2] + temp2 * y[i+1] + temp3 * y[i] + 0.5;
			if (xi != hor_pos || yi != ver_pos) 
			{
				hMov(xi);
				vMov(yi);
				setpixel();
			}
		}
	}
}


/*-----------------------------------------------------------------------------
 *	readNumber	read an integer from the string pointed to by *ptr,
 *			returning the integer in val, and updating *ptr for
 *			the caller
 *---------------------------------------------------------------------------*/
readNumber(ptr,val)
char **ptr;
int *val;
{
	int sign = 1;

	*val = 0;

	if (**ptr == '-')
	{
		sign = -1;
		++*ptr;
	}
	while(isdigit(**ptr))
	{
		*val = *val * 10 + **ptr - '0';
		++*ptr;
	}
	*val = *val * sign;

	/* skip all trailing white space */
	while(white(**ptr) || **ptr == '\n') ++*ptr; 

	/* return next char -- if at end of string this is NULL */
	return(**ptr);
}


/*-----------------------------------------------------------------------------
 *	euclidDist	compute euclidean distance between the two cartesian
 *			coordinates
 *---------------------------------------------------------------------------*/
float euclidDist(x, y, x1, y1)	
int x1,y1,x,y;
{
	double deltax, deltay;

	deltax = x - x1;
	deltay = y - y1;
	return(sqrt(deltax*deltax + deltay*deltay) + 0.5);
}
