/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gxdither.c */
#include "gx.h"
#include "gxfixed.h"
#include "gxlum.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gzdevice.h"
#include "gzcolor.h"
#include "gzht.h"

/* 
 *	Improved dithering for Ghostscript.  The underlying device imaging 
 *	model supports dithering between two colors to generate intermediate
 *	shades.  
 *	
 *	The strategy is to first see if the color is either pure white or
 *	pure black.  In this case the problem is trivial.
 *
 *	Next, if the device has high quality colors (at least 256 values
 *	per axis), we ask it to map the color directly.
 *
 *	Next, if the device is black and white, or the color happens
 *	to be achromatic, we perform simple B/W dithering.
 *	
 *	Otherwise, things are a bit more complicated.  If the device 
 * 	supports N shades of each R, G and B independently, there are a total 
 *	of N*N*N colors.  These colors form a 3-D grid in a cubical color 
 *	space.  The following dithering technique works by locating the 
 *	color we want in this 3-D color grid and finding the eight colors 
 * 	that surround it.  In the case of dithering into 8 colors with 1 
 *	bit for each red, green and blue, these eight colors will always 
 *	be the same.
 *
 *	Now we consider all possible diagonal paths between the eight colors
 *	and chose the path that runs closest to our desired color in 3-D
 *	color space.  There are 28 such paths.  Then we find the position
 *	on the path that is closest to our color.
 *
 *	The search is made faster by always reflecting our color into
 *	the bottom octant of the cube and comparing it to 7 paths.
 *	After the best path and the best position on that path are found,
 *	the results are reflected back into the original color space.
 *
 *	NOTE: This code has been tested for B/W and Color imaging with
 *	1, 2, 3 and 8 bits per component.
 *
 *	--- original code by Paul Haeberli @ Silicon Graphics - 1990
 *	--- extensively revised by L. Peter Deutsch, Aladdin Enterprises
 */

void gx_color_load(P2(gx_device_color *, gs_state *));

#define	WEIGHT1		(unsigned long)(100)	/* 1.0 			*/
#define	WEIGHT2		(unsigned long)(71)	/* 1/sqrt(2.0) 		*/
#define	WEIGHT3		(unsigned long)(62)	/* 1/sqrt(3.0)+tad 	*/

#define	DIAG_R		(0x1)
#define	DIAG_G		(0x2)
#define	DIAG_B		(0x4)
#define	DIAG_RG		(0x3)
#define	DIAG_GB		(0x6)
#define	DIAG_BR		(0x5)
#define	DIAG_RGB	(0x7)

static unsigned short lum[8] = {
    (0*lum_blue_weight+0*lum_green_weight+0*lum_red_weight),
    (0*lum_blue_weight+0*lum_green_weight+1*lum_red_weight),
    (0*lum_blue_weight+1*lum_green_weight+0*lum_red_weight),
    (0*lum_blue_weight+1*lum_green_weight+1*lum_red_weight),
    (1*lum_blue_weight+0*lum_green_weight+0*lum_red_weight),
    (1*lum_blue_weight+0*lum_green_weight+1*lum_red_weight),
    (1*lum_blue_weight+1*lum_green_weight+0*lum_red_weight),
    (1*lum_blue_weight+1*lum_green_weight+1*lum_red_weight),
};

/* Compute a fractional color, the correctly rounded quotient of */
/* f * max_color_param / maxv. */
#define _fc(f, maxv)\
  (gx_color_value)(((f) * (max_color_param_long * 2) + maxv) / (maxv * 2))
/* We have to split up the following because of a bug in the IBM AIX 3.2 */
/* C compiler. */
private gx_color_value
  q0[] = { 0 };
private gx_color_value
  q1[] = { 0, 0xffff };
private gx_color_value
  q2[] = { 0, _fc(1,2), 0xffff };
private gx_color_value
  q3[] = { 0, _fc(1,3), _fc(2,3), 0xffff };
private gx_color_value
  q4[] = { 0, _fc(1,4), _fc(2,4), _fc(3,4), 0xffff };
private gx_color_value
  q5[] = { 0, _fc(1,5), _fc(2,5), _fc(3,5), _fc(4,5), 0xffff };
private gx_color_value
  q6[] = { 0, _fc(1,6), _fc(2,6), _fc(3,6), _fc(4,6), _fc(5,6), 0xffff };
private gx_color_value
  q7[] = { 0, _fc(1,7), _fc(2,7), _fc(3,7), _fc(4,7), _fc(5,7), _fc(6,7), 0xffff };
private gx_color_value _ds *color_quo[8] = { q0, q1, q2, q3, q4, q5, q6, q7 };
#define fractional_color(f, maxv)\
  ((maxv) <= 7 ? color_quo[maxv][f] : _fc(f, maxv))

/* Note that this routine assumes that the incoming color */
/* has already been mapped through the transfer functions. */
void
gx_color_render(gs_color *pcolor, gx_device_color *pdevc, gs_state *pgs)
{	device *pdev = pgs->device;
	uint max_value;
	unsigned long hsize;
	gx_device *dev;
	gx_color_index (*map_rgb_color)(P4(gx_device *, gx_color_value,
					gx_color_value, gx_color_value));

/* Make a special check for black and white. */
	if ( pcolor->is_gray )
	   {	if ( pcolor->luminance == 0 )
		   {	pdevc->color2 = pdevc->color1 = pdev->black;
			pdevc->halftone_level = 0; /* pure color */
			return;
		   }
		else if ( pcolor->luminance == max_color_param )
		   {	pdevc->color2 = pdevc->color1 = pdev->white;
			pdevc->halftone_level = 0; /* pure color */
			return;
		   }
	   }

/* get a few handy values */
	dev = pdev->info;
	map_rgb_color = dev->procs->map_rgb_color;
	hsize = (unsigned)pgs->halftone->order_size;

/* See if we should do it in black and white. */
	if ( !gx_device_has_color(dev) || pcolor->is_gray )
	   {	color_param lum = color_luminance(pcolor);
		if ( dev->color_info.max_gray >= 31 )
		   {	/* Give the device a chance first. */
			gx_color_index cx =
				(*map_rgb_color)(dev, lum, lum, lum);
			if ( cx != gx_no_color_index )
			   {	pdevc->color2 = pdevc->color1 = cx;
				pdevc->halftone_level = 0;
				return;
			   }
		   }
		/* No luck, must dither. */
		max_value = dev->color_info.dither_gray - 1;
		   {	unsigned long nshades = hsize * max_value + 1;
			unsigned long lx =
				(nshades * lum) / (max_color_param_long+1);
			color_param l = lx / hsize;
			pdevc->halftone_level = lx % hsize;
			lum = fractional_color(l, max_value);
			pdevc->color1 = (*map_rgb_color)(dev, lum, lum, lum);
			if ( pdevc->halftone_level == 0 )
			   {	/* Close enough to a pure color, */
				/* no dithering needed. */
				pdevc->color2 = pdevc->color1;
			   }
			else
			   {	lum = fractional_color(l+1, max_value);
				pdevc->color2 =
					(*map_rgb_color)(dev, lum, lum, lum);
			   }
			gx_color_load(pdevc, pgs);
		   }
		return;
	   }

/* If we are on a high quality RGB display, try not dithering first. */
	if ( dev->color_info.max_rgb >= 31 )
	   {	gx_color_index cx = (*map_rgb_color)(dev, pcolor->red,
						     pcolor->green,
						     pcolor->blue);
		if ( cx != gx_no_color_index )
		   {	pdevc->color2 = pdevc->color1 = cx;
			pdevc->halftone_level = 0;	/* pure color */
			return;
		   }
	   }
	max_value = dev->color_info.dither_rgb - 1;

/* must do real color dithering */
   {	color_param r, g, b;
	color_param rem_r = pcolor->red;
	color_param rem_g = pcolor->green;
	color_param rem_b = pcolor->blue;
	int adjust_r, adjust_b, adjust_g;
	unsigned short amax;
	unsigned long dmax;
	int axisc, diagc;
	unsigned short lum_invert;
	unsigned long dot1, dot2, dot3;
	int level;

	/* Compute the quotient and remainder of each color component */
	/* with the actual number of available colors.  Avoid */
	/* multiplies and divides for the common values. */
	/* Note that max_color_param is all 1s, so when only a short */
	/* result is needed, subtracting x*max_color_param is equivalent to */
	/* just adding x.  rem_{r,g,b} are short, so we can get away */
	/* with short arithmetic. */
	switch ( max_value )
	   {
	case 1:			/* 8 colors */
		if ( rem_r == max_color_param ) rem_r = 0, r = 1;
		else r = 0;
		if ( rem_g == max_color_param ) rem_g = 0, g = 1;
		else g = 0;
		if ( rem_b == max_color_param ) rem_b = 0, b = 1;
		else b = 0;
		break;
		/* When max_color_param % max_value = 0, */
		/* we can do some short arithmetic. */
		/* Don't bother if ints are 32 bits. */
#if arch_ints_are_short
	case 3:			/* 64 colors */
	case 15:		/* 4096 colors */
	   {	const color_param q = max_color_param / max_value;
		r = rem_r / q;
		rem_r = rem_r * max_value + r;
		g = rem_g / q;
		rem_g = rem_g * max_value + g;
		b = rem_b / q;
		rem_b = rem_b * max_value + b;
	   }	break;
#endif
	default:
	   {	unsigned long want_r = (ulong)max_value * rem_r;
		unsigned long want_g = (ulong)max_value * rem_g;
		unsigned long want_b = (ulong)max_value * rem_b;
		r = want_r / max_color_param_long;
		g = want_g / max_color_param_long;
		b = want_b / max_color_param_long;
		rem_r = (color_param)want_r + r;
		rem_g = (color_param)want_g + g;
		rem_b = (color_param)want_b + b;
	   }
	   }

	/* Check for no dithering required */
	if ( !(rem_r | rem_g | rem_b) )
	   {	pdevc->color2 = pdevc->color1 =
			(*map_rgb_color)(dev, fractional_color(r, max_value),
					 fractional_color(g, max_value),
					 fractional_color(b, max_value));
		pdevc->halftone_level = 0;
		return;
	   }
#ifdef DEBUG
if ( gs_debug['c'] )
   {	dprintf3("[c]rgb=%x,%x,%x -->\n",
		 (unsigned)pcolor->red, (unsigned)pcolor->green,
		 (unsigned)pcolor->blue);
	dprintf6("   %x+%x,%x+%x,%x+%x -->\n",
		(unsigned)r, (unsigned)rem_r, (unsigned)g, (unsigned)rem_g,
		(unsigned)b, (unsigned)rem_b);
   }
#endif

/* flip the remainder color into the 0, 0, 0 octant */
	lum_invert = 0;
#define half ((color_param)(max_color_param_long>>1))
	if ( rem_r > half )
		rem_r = max_color_param - rem_r,
		  adjust_r = -1, r++, lum_invert += lum_red_weight;
	else
		adjust_r = 1;
	if ( rem_g > half)
		rem_g = max_color_param - rem_g,
		  adjust_g = -1, g++, lum_invert += lum_green_weight;
	else
		adjust_g = 1;
	if ( rem_b > half )
		rem_b = max_color_param - rem_b,
		  adjust_b = -1, b++, lum_invert += lum_blue_weight;
	else
		adjust_b = 1;
	pdevc->color1 = (*map_rgb_color)(dev, fractional_color(r, max_value),
					 fractional_color(g, max_value),
					 fractional_color(b, max_value));
/* 
 * Dot the color with each axis to find the best one of 7;
 * find the color at the end of the axis chosen.
 */
	if ( rem_g > rem_r )
	   {	if ( rem_b > rem_g )
			amax = rem_b, axisc = DIAG_B;
		else
			amax = rem_g, axisc = DIAG_G;
		if ( rem_b > rem_r )
			dmax = (unsigned long)rem_g+rem_b, diagc = DIAG_GB;
		else
			dmax = (unsigned long)rem_r+rem_g, diagc = DIAG_RG;
	   }
	else
	   {	if ( rem_b > rem_r )
			amax = rem_b, axisc = DIAG_B;
		else
			amax = rem_r, axisc = DIAG_R;
		if ( rem_b > rem_g )
			dmax = (unsigned long)rem_b+rem_r, diagc = DIAG_BR;
		else
			dmax = (unsigned long)rem_r+rem_g, diagc = DIAG_RG;
	   }

	dot1 = amax*WEIGHT1;
	dot2 = dmax*WEIGHT2;
	dot3 = (ulong)rem_r+rem_g+rem_b;	/* rgb axis */
	if ( dot1 > dot2 )
	   {	if ( dot3*WEIGHT3 > dot1 )
			diagc = DIAG_RGB,
			  level = (hsize * dot3) / (3 * max_color_param_long);
		else
			diagc = axisc,
			  level = (hsize * amax) / max_color_param_long;
	   }
	else
	   {	if ( dot3*WEIGHT3 > dot2 )
			diagc = DIAG_RGB,
			  level = (hsize * dot3) / (3 * max_color_param_long);
		else
			level = (hsize * dmax) / (2 * max_color_param_long);
	   };
#ifdef DEBUG
if ( gs_debug['c'] )
   {	dprintf6("   %x+%x,%x+%x,%x+%x;",
		(unsigned)r, (unsigned)rem_r, (unsigned)g, (unsigned)rem_g,
		(unsigned)b, (unsigned)rem_b);
	dprintf3(" adjust=%d,%d,%d;\n",
		adjust_r, adjust_g, adjust_b);
   }
#endif

	if ( (pdevc->halftone_level = level) == 0 )
		pdevc->color2 = pdevc->color1;
	else
	   {	gx_color_index color2;
/* construct the second color, inverting back to original space if needed */
		if (diagc & DIAG_R) r += adjust_r;
		if (diagc & DIAG_G) g += adjust_g;
		if (diagc & DIAG_B) b += adjust_b;
/* get the second device color, sorting by luminance */
		color2 = (*map_rgb_color)(dev, fractional_color(r, max_value),
					  fractional_color(g, max_value),
					  fractional_color(b, max_value));
/****** THIS IS A BAD IDEA ******/
#if 0
		if ( lum[diagc] < lum_invert )
		   {	pdevc->color2 = pdevc->color1;
			pdevc->color1 = color2;
			pdevc->halftone_level = level = hsize - level;
		   }
		else
#endif
			pdevc->color2 = color2;
		gx_color_load(pdevc, pgs);
	   }

#ifdef DEBUG
if ( gs_debug['c'] )
   {	dprintf5("[c]diagc=%d; color1=%lx, color2=%lx, level=%d/%d\n",
		 diagc, (ulong)pdevc->color1, (ulong)pdevc->color2,
		 level, (unsigned)hsize);
   }
#endif

   }
}
