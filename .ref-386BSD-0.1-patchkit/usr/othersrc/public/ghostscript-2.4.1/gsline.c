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

/* gsline.c */
/* Line parameter operators for GhostScript library */
#include "math_.h"
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"			/* ditto */
#include "gxmatrix.h"			/* for gzstate */
#include "gzstate.h"
#include "gzline.h"

/* setlinewidth */
int
gs_setlinewidth(gs_state *pgs, floatp width)
{	pgs->line_params->width = width / 2;
	return 0;
}

/* currentlinewidth */
float
gs_currentlinewidth(const gs_state *pgs)
{	return (float)(pgs->line_params->width * 2);
}

/* setlinecap */
int
gs_setlinecap(gs_state *pgs, gs_line_cap cap)
{	pgs->line_params->cap = cap;
	return 0;
}

/* currentlinecap */
gs_line_cap
gs_currentlinecap(const gs_state *pgs)
{	return pgs->line_params->cap;
}

/* setlinejoin */
int
gs_setlinejoin(gs_state *pgs, gs_line_join join)
{	pgs->line_params->join = join;
	return 0;
}

/* currentlinejoin */
gs_line_join
gs_currentlinejoin(const gs_state *pgs)
{	return pgs->line_params->join;
}

/* setmiterlimit */
int
gs_setmiterlimit(gs_state *pgs, floatp limit)
{	if ( limit < 1.0 ) return_error(gs_error_rangecheck);
	pgs->line_params->miter_limit = limit;
	/* The supplied miter limit is an upper bound on */
	/* 1/sin(phi/2).  We convert this to a lower bound on */
	/* tan(phi).  Note that if phi > pi/2, this is negative. */
	/* We use the half-angle and angle-sum formulas here */
	/* to avoid the trig functions.... */
	   {	double limit_sq = limit * limit;
	/* We need a special check for phi/2 close to pi/4. */
	/* Some C compilers can't handle the following as a */
	/* conditional expression.... */
		if ( limit_sq < 2.0001 && limit_sq > 1.9999 )
			pgs->line_params->miter_check = 1.0e6;
		else
			pgs->line_params->miter_check =
				sqrt(limit_sq - 1) * 2 / (limit_sq - 2);
	   }
	return 0;
}

/* currentmiterlimit */
float
gs_currentmiterlimit(const gs_state *pgs)
{	return pgs->line_params->miter_limit;
}

/* setdash */
int
gs_setdash(gs_state *pgs, const float *pattern, uint length, floatp offset)
{	uint n = length;
	const float *dfrom = pattern;
	char ink = 1;
	int index = 0;
	float pattern_length = 0.0;
	float dist_left;
	dash_params *dash = &pgs->line_params->dash;
	float *ppat;
	/* Check the dash pattern */
	while ( n-- )
	   {	float elt = *dfrom++;
		if ( elt < 0 ) return_error(gs_error_rangecheck);
		pattern_length += elt;
	   }
	if ( length == 0 )		/* empty pattern */
	   {	dist_left = 0.0;
		ppat = 0;
	   }
	else
	   {	if ( pattern_length == 0 )
			return_error(gs_error_rangecheck);
		/* Compute the initial index, ink_on, and distance left */
		/* in the pattern, according to the offset. */
#define f_mod(a, b) ((a) - floor((a) / (b)) * (b))
		dist_left = f_mod(offset, pattern_length);
		while ( (dist_left -= pattern[index]) >= 0 )
			ink = !ink, index++;
		ppat = (float *)gs_malloc(length, sizeof(float),
					  "dash pattern");
		if ( ppat == 0 ) return_error(gs_error_VMerror);
		memcpy(ppat, pattern, length * sizeof(float));
	   }
	dash->pattern = ppat;
	dash->pattern_size = length;
	dash->offset = offset;
	dash->init_ink_on = ink;
	dash->init_index = index;
	dash->init_dist_left = -dist_left;
	return 0;
}

/* currentdash */
uint
gs_currentdash_length(const gs_state *pgs)
{	return pgs->line_params->dash.pattern_size;
}
int
gs_currentdash_pattern(const gs_state *pgs, float *pattern)
{	memcpy(pattern, pgs->line_params->dash.pattern, pgs->line_params->dash.pattern_size * sizeof(float));
	return 0;
}
float
gs_currentdash_offset(const gs_state *pgs)
{	return pgs->line_params->dash.offset;
}
