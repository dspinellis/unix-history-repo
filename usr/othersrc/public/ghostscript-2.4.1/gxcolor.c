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

/* gxcolor.c */
/* Private color procedures for Ghostscript library */
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"			/* for gxmatrix.h */
#include "gxlum.h"
#include "gxmatrix.h"
#include "gxdevice.h"			/* for gx_color_index */
#include "gzcolor.h"
#include "gzht.h"
#include "gzstate.h"

/* Imported procedures */
void gx_color_render(P3(gs_color *, gx_device_color *, gs_state *));

/* Forward declarations */
void gx_color_from_rgb(P1(gs_color *));

/* ------ Color setup routines ------ */

/* Set up black for writing into the character cache. */
void
gx_set_black(gs_state *pgs)
{	register gs_color *pcolor = pgs->color;
	pcolor->red = pcolor->green = pcolor->blue = pcolor->luminance = 0;
	pcolor->luminance_set = 1;
	pcolor->is_gray = 1;
	pcolor->space = (byte)gs_color_space_DeviceGray;
	gx_color_render(pcolor, pgs->dev_color, pgs);
}

/* Set up a gray color.  This is an internal routine */
/* for use by initialization and gx_remap_color. */
void
gx_set_gray_only(register gs_color *pcolor, color_param gray)
{	pcolor->red = pcolor->green = pcolor->blue =
	  pcolor->luminance = gray;
	pcolor->luminance_set = 1;
	pcolor->is_gray = 1;
	pcolor->space = (byte)gs_color_space_DeviceGray;
}

/* Set up an RGB color.  This is an internal routine */
/* for use by gx_remap_color. */
void
gx_set_rgb_only(register gs_color *pcolor,
  color_param r, color_param g, color_param b)
{	pcolor->red = r;
	pcolor->green = g;
	pcolor->blue = b;
	gx_color_from_rgb(pcolor);
}

/* Set up an RGB color. */
void
gx_color_from_rgb(register gs_color *pcolor)
{	if ( pcolor->red == pcolor->green && pcolor->green == pcolor->blue )
	   {	pcolor->luminance = pcolor->red;	/* pick any one */
		pcolor->is_gray = pcolor->luminance_set = 1;
	   }
	else
	   {	/* Compute luminance later */
		pcolor->is_gray = pcolor->luminance_set = 0;
	   }
	pcolor->space = (byte)gs_color_space_DeviceRGB;
}

/* Force a parameter into the range [0..1], */
/* and convert to a color_param. */
color_param
gx_color_unit_param(floatp fval)
{	if ( fval <= 0.0 )
		return 0;
	else if ( fval >= 1.0 )
		return max_color_param;
	else
		return (color_param)(fval * max_color_param_float);
}

/* Map a color_param through a transfer map. */
color_param
gx_color_param_map(color_param cv, color_param *values)
{
#define cp_frac_bits (color_param_bits - log2_transfer_map_size)
	int cmi = cv >> cp_frac_bits;
	color_param mv = values[cmi];
	int rem, mdv;
	/* Interpolate between two adjacent values if needed. */
	rem = (cv & ((1 << cp_frac_bits) - 1)) - (cv >> (color_param_bits - cp_frac_bits));
	if ( rem == 0 ) return mv;
	else if ( rem > 0 ) mdv = values[cmi + 1] - mv;
	else mdv = mv - values[cmi - 1];
#if arch_ints_are_short
	/* Only use long multiplication if necessary. */
	if ( mdv > 1 << (16 - cp_frac_bits) )
		return mv + (uint)(((ulong)rem * mdv) >> cp_frac_bits);
#endif
	return mv + ((rem * mdv) >> cp_frac_bits);
#undef cp_frac_bits
}

/* Remap the color in the graphics state. */
void gx_render_color(P2(gs_color *, gs_state *));
int
gx_remap_color(gs_state *pgs)
{	gs_color *pcolor = pgs->color;
	gs_color mcolor;		/* color mapped by transfer procs */
#define mapcp(p,c) gx_map_color_param(pgs, pcolor->c, p)
	switch ( (gs_color_space)pcolor->space )
	   {
	case gs_color_space_DeviceGray:
		gx_set_gray_only(&mcolor, mapcp(gray, red));	/* any one */
		break;
	case gs_color_space_DeviceRGB:
		gx_set_rgb_only(&mcolor, mapcp(red, red),
				mapcp(green, green), mapcp(blue, blue));
		break;
	default:
		return_error(gs_error_undefined);
	   }
#undef mapcp
	gx_render_color(&mcolor, pgs);
	return 0;
}
void
gx_render_color(gs_color *pmcolor, gs_state *pgs)
{	gx_color_render(pmcolor, pgs->dev_color, pgs);
}

/* ------ Color conversion routines ------ */

/* Note: the color model conversion algorithms are taken from */
/* Rogers, Procedural Elements for Computer Graphics, pp. 401-403. */

/* Compute (if necessary) and return the luminance of a color. */
color_param
gx_color_luminance(register gs_color *pcolor)
{	if ( !pcolor->luminance_set )
	   {	pcolor->luminance =
			(pcolor->red * (unsigned long)lum_red_weight +
			pcolor->green * (unsigned long)lum_green_weight +
			pcolor->blue * (unsigned long)lum_blue_weight +
			(lum_all_weights / 2))
		    / lum_all_weights;
		pcolor->luminance_set = 1;
	   }
	return pcolor->luminance;
}

/* Convert RGB to HSB */
void
gx_color_to_hsb(register gs_color *pcolor, color_param hsb[3])
{
#define rhue hsb[0]
#define rsat hsb[1]
#define rbri hsb[2]
	if ( pcolor->is_gray )
	   {	rhue = 0;	/* arbitrary */
		rsat = 0;
		rbri = pcolor->red;	/* pick any one */
	   }
	else
	   {	/* Convert rgb to hsb */
		gs_color c;
		color_param V, Temp;
		long diff, H;
		c.red = pcolor->red;
		c.green = pcolor->green;
		c.blue = pcolor->blue;
		V = (c.red > c.green ? c.red : c.green);
		if ( c.blue > V ) V = c.blue;
		Temp = (c.red > c.green ? c.green : c.red);
		if ( c.blue < Temp ) Temp = c.blue;
		diff = V - Temp;
		if ( V == c.red )
			H = (c.green - c.blue) * max_color_param_long / diff;
		else if ( V == c.green )
			H = (c.blue - c.red) * max_color_param_long / diff + 2 * max_color_param_long;
		else /* V == c.blue */
			H = (c.red - c.green) * max_color_param_long / diff + 4 * max_color_param_long;
		if ( H < 0 ) H += 6 * max_color_param_long;
		rhue = H / 6;
		rsat = diff * max_color_param_long / V;
		rbri = V;
	   }
#undef rhue
#undef rsat
#undef rbri
}

/* Complete color specified by hsb */
void
gx_color_from_hsb(register gs_color *pcolor,
  color_param hue, color_param saturation, color_param brightness)
{	if ( saturation == 0 )
	   {	pcolor->red = pcolor->green = pcolor->blue = brightness;
	   }
	else
	   {	/* Convert hsb to rgb. */
		/* We rely on the fact that the product of two */
		/* color_params fits into an unsigned long. */
		ulong V = brightness;	/* force arithmetic to long */
		color_param S = saturation;
#define mcp max_color_param
#define mcpl max_color_param_long
#define mcp6 (mcp / 6 + 1)
		ulong F = (hue % mcp6) * 6;	/* ditto */
		int I = hue / mcp6;
		color_param M = V * (mcpl - S) / mcpl;
		color_param N = V * (mcpl - S * F / mcpl) / mcpl;
		color_param K = V * (mcpl - S * (mcpl - F) / mcpl) / mcpl;
#undef mcp6
#undef mcpl
#undef mcp
		color_param R, G, B;
		switch ( I )
		   {
		default: R = V; G = K; B = M; break;
		case 1: R = N; G = V; B = M; break;
		case 2: R = M; G = V; B = K; break;
		case 3: R = M; G = N; B = V; break;
		case 4: R = K; G = M; B = V; break;
		case 5: R = V; G = M; B = N; break;
		   }
		pcolor->red = R;
		pcolor->green = G;
		pcolor->blue = B;
	   }
	gx_color_from_rgb(pcolor);	/* compute luminance */
}

/* ------ Internal routines ------ */

/* Heapsort (algorithm 5.2.3H, Knuth vol. 2, p. 146), */
/* modified for 0-origin indexing. */
void
gx_sort_ht_order(ht_bit *recs, uint N)
{	uint l = N >> 1;
	uint r = N - 1;
	uint j;
	ht_bit R;
	if ( N <= 1 ) return;
#define key(m) recs[m].mask
#define K R.mask
	while ( 1 )
	   {	if ( l > 0 )
			R = recs[--l];
		else
		   {	R = recs[r];
			recs[r] = recs[0];
			if ( --r == 0 )
			   {	recs[0] = R;
				break;
			   }
		   }
		j = l;
		while ( 1 )
		   {	uint i = j;
			j = j + j + 1;
			if ( j < r )
				if ( key(j) < key(j + 1) ) j++;
			if ( j > r || K >= key(j) )
			   {	recs[i] = R;
				break;	/* to outer loop */
			   }
			recs[i] = recs[j];
		   }
	   }
}
