/* Copyright (C) 1989, 1990 Aladdin Enterprises.  All rights reserved.
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

/* gspaint.c */
/* Painting procedures for GhostScript library */
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"			/* for gs_state */
#include "gspaint.h"
#include "gzpath.h"
#include "gzstate.h"
#include "gzdevice.h"
#include "gzcolor.h"
#include "gxcpath.h"
#include "gxdevmem.h"
#include "gximage.h"

/* Erase the page */
int
gs_erasepage(gs_state *pgs)
{	device *pdev = pgs->device;
	gx_device *dev = pdev->info;
	(*dev->procs->fill_rectangle)(dev, 0, 0, dev->width, dev->height, pdev->white);
	return 0;
}

/* Fill using the winding number rule */
int
gs_fill(gs_state *pgs)
{	return gs_fill_adjust(pgs, (fixed)0);
}
/* This is a hack, see gx_fill_path and gs_type1_interpret. */
int
gs_fill_adjust(gs_state *pgs, fixed adjust)
{	int code;
	/* If we're inside a charpath, just merge the current path */
	/* into the parent's path. */
	if ( pgs->in_charpath )
		code = gx_path_add_path(pgs->saved->path, pgs->path);
	else
	   {	gx_color_load(pgs->dev_color, pgs);
		code = gx_fill_path(pgs->path, pgs->dev_color, pgs,
				    gx_rule_winding_number, adjust);
		if ( !code ) gs_newpath(pgs);
	   }
	return code;
}

/* Fill using the even/odd rule */
int
gs_eofill(gs_state *pgs)
{	int code;
	/* If we're inside a charpath, just merge the current path */
	/* into the parent's path. */
	if ( pgs->in_charpath )
		code = gx_path_add_path(pgs->saved->path, pgs->path);
	else
	   {	gx_color_load(pgs->dev_color, pgs);
		code = gx_fill_path(pgs->path, pgs->dev_color, pgs,
				    gx_rule_even_odd, (fixed)0);
		if ( !code ) gs_newpath(pgs);
	   }
	return code;
}

/* Stroke the current path */
int
gs_stroke(gs_state *pgs)
{	int code;
	/* If we're inside a charpath, just merge the current path */
	/* into the parent's path. */
	if ( pgs->in_charpath )
		code = gx_path_add_path(pgs->saved->path, pgs->path);
	else
	   {	gx_color_load(pgs->dev_color, pgs);
		code = gx_stroke_fill(pgs->path, pgs);
		if ( !code ) gs_newpath(pgs);
	   }
	return code;
}

/* Compute the stroked outline of the current path */
int
gs_strokepath(gs_state *pgs)
{	gx_path spath;
	int code;
	gx_path_init(&spath, &pgs->memory_procs);
	code = gx_stroke_add(pgs->path, &spath, pgs);
	if ( code < 0 ) return code;
	gx_path_release(pgs->path);
	*pgs->path = spath;
	return 0;
}

/* Render a sampled image */
int
gs_colorimage(gs_state *pgs, int width, int height, int bps, int spp,
  gs_matrix *pmat, byte *data)
{	gs_image_enum *penum =
	  (gs_image_enum *)gs_malloc(1, gs_image_enum_sizeof, "gs_[color]image");
	int code;
	if ( penum == 0 ) return_error(gs_error_VMerror);
	if ( (code = gs_image_init(penum, pgs, width, height, bps, spp, pmat)) < 0 )
	   {	gs_free((char *)penum, 1, gs_image_enum_sizeof, "gs_[color]image");
		return code;
	   }
	if ( spp > 0 )
	   {	uint size =
			(((uint)width * bps * spp + 7) >> 3) * (uint)height;
		code = gs_image_next(penum, data, size);
	   }
	else
	   {	/* Deliver the colors separately */
		uint plane_size =
			(((uint)width * bps + 7) >> 3) * (uint)height;
		byte *plane_data = data;
		int count = spp;
		do
		   {	code = gs_image_next(penum, plane_data, plane_size);
			if ( code < 0 ) return code;
			plane_data += plane_size;
		   }
		while ( ++count );
	   }
	gs_free((char *)penum, 1, gs_image_enum_sizeof, "gs_[color]image");
	return (code < 0 ? code : 0);
}
int
gs_image(gs_state *pgs, int width, int height, int bps,
  gs_matrix *pmat, byte *data)
{	return gs_colorimage(pgs, width, height, bps, 1, pmat, data);
}

/* Render a mask */
int
gs_imagemask(gs_state *pgs, int width, int height, int invert,
  gs_matrix *pmat, byte *data, int adjust)
{	gs_image_enum *penum =
	  (gs_image_enum *)gs_malloc(1, gs_image_enum_sizeof, "gs_imagemask");
	int code;
	uint size = (((uint)width + 7) >> 3) * (uint)height;
	if ( penum == 0 ) return_error(gs_error_VMerror);
	if ( (code = gs_imagemask_init(penum, pgs, width, height, invert, pmat, adjust)) < 0 )
	   {	gs_free((char *)penum, 1, gs_image_enum_sizeof, "gs_imagemask");
		return code;
	   }
	code = gs_image_next(penum, data, size);
	gs_free((char *)penum, 1, gs_image_enum_sizeof, "gs_imagemask");
	return (code < 0 ? code : 0);
}
