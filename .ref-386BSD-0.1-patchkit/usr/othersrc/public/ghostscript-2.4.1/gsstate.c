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

/* gsstate.c */
/* Miscellaneous graphics state operators for Ghostscript library */
#include "gx.h"
#include "memory_.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"			/* for gzstate */
#include "gzstate.h"
#include "gzdevice.h"
#include "gzcolor.h"			/* requires gxdevice.h */
#include "gzht.h"
#include "gzline.h"
#include "gzpath.h"

/* Forward references */
private gs_state *alloc_gstate(P4(proc_alloc_t, proc_free_t, const gs_state *, const char *));
private int alloc_gstate_contents(P1(gs_state *));
private void free_gstate_contents(P1(gs_state *));
private void copy_gstate_contents(P2(gs_state *pto, const gs_state *pfrom));

/* Allocate and free a structure */
#define alloc_struct(pgs,typ,cname)\
  (typ *)(*(pgs)->memory_procs.alloc)(1, sizeof(typ), cname)
#define free_struct(pgs,ptr,cname)\
  (*(pgs)->memory_procs.free)((char *)(ptr), 1, sizeof(*(ptr)), cname)

/* ------ Operations on the entire graphics state ------ */

/* Allocate and initialize a graphics state. */
private float
null_transfer(gs_state *pgs, floatp gray)
{	return gray;
}
gs_state *
gs_state_alloc(proc_alloc_t palloc, proc_free_t pfree)
{	register gs_state *pgs = alloc_gstate(palloc, pfree, (gs_state *)0, "gs_state_alloc");
	if ( pgs == 0 ) return 0;
	pgs->saved = 0;
	/* Initialize things not covered by initgraphics */
	gx_path_init(pgs->path, &pgs->memory_procs);
	gx_path_init(&pgs->clip_path->path, &pgs->memory_procs);
	gx_clip_list_init(&pgs->clip_path->list);
	pgs->halftone->width = pgs->halftone->height =
		pgs->halftone->order_size = 0;
	gs_sethalftonephase(pgs, 0, 0);
	/* Initialize the color so that gx_remap_color won't crash. */
	gx_set_gray_only(pgs->color, (color_param)0);
	gs_settransfer(pgs, null_transfer);
	gs_nulldevice(pgs);
	gs_setflat(pgs, 1.0);
	gs_setstrokeadjust(pgs, 1);
	/****** What about the font ? ******/
	pgs->in_cachedevice = pgs->in_charpath = 0;
	pgs->level = 0;
	if ( gs_initgraphics(pgs) < 0 )
	   {	/* Something went very wrong */
		return 0;
	   }
	return pgs;
}

/* Free a graphics state */
int
gs_state_free(gs_state *pgs)
{	free_gstate_contents(pgs);
	free_struct(pgs, pgs, "gs_state_free");
	return 0;
}

/* Save the graphics state */
int
gs_gsave(gs_state *pgs)
{	gs_state *pnew = alloc_struct(pgs, gs_state, "gs_gsave");
	if ( pnew == 0 )
		return_error(gs_error_VMerror);
	*pnew = *pgs;
	if ( alloc_gstate_contents(pgs) < 0 )
	   {	*pgs = *pnew;		/* undo partial alloc */
		free_struct(pgs, pnew, "gs_gsave");
		return_error(gs_error_VMerror);
	   }
	copy_gstate_contents(pgs, pnew);
	pgs->saved = pnew;
	pgs->level++;
	return 0;
}

/* Restore the graphics state. */
int
gs_grestore(gs_state *pgs)
{	gs_state *saved = pgs->saved;
	if ( !saved ) return gs_gsave(pgs);	/* shouldn't happen */
	free_gstate_contents(pgs);
	*pgs = *saved;
	free_struct(pgs, saved, "gs_grestore");
	return (pgs->saved == 0 ? gs_gsave(pgs) : 0);
}

/* Restore to the bottommost graphics state. */
int
gs_grestoreall(gs_state *pgs)
{	if ( !pgs->saved ) return gs_gsave(pgs);	/* shouldn't happen */
	while ( pgs->saved->saved ) gs_grestore(pgs);
	return gs_grestore(pgs);
}

/* Allocate and return a new graphics state. */
gs_state *
gs_gstate(gs_state *pgs)
{	gs_state *pnew = alloc_gstate(pgs->memory_procs.alloc, pgs->memory_procs.free, pgs, "gs_gstate");
	if ( pnew == 0 ) return 0;
	copy_gstate_contents(pnew, pgs);
	pnew->saved = 0;
	return pgs;
}

/* Copy the current graphics state to a previously allocated one. */
int
gs_currentgstate(gs_state *pto, const gs_state *pgs)
{	/* We have to copy both the scalar and composite parts */
	/* of the state. */
	gs_state sgs;
	sgs = *pto;
	*pto = *pgs;
	/* Put back the composite part pointers. */
#define gcopy(element)\
    pto->element = sgs.element
	gcopy(path);
	gcopy(clip_path);
	gcopy(line_params);
	gcopy(halftone);
	gcopy(color);
	gcopy(dev_color);
	gcopy(transfer);
	gcopy(device);
#undef gcopy
	copy_gstate_contents(pto, pgs);
	return 0;
}

/* Restore the current graphics state from a previously allocated one. */
int
gs_setgstate(gs_state *pgs, const gs_state *pfrom)
{	/* The implementation is the same as currentgstate, */
	/* except we must preserve the saved pointer and the level. */
	gs_state *saved = pgs->saved;
	int level = pgs->level;
	int code = gs_currentgstate(pgs, pfrom);
	if ( code < 0 ) return code;
	pgs->saved = saved;
	pgs->level = level;
	return 0;
}

/* Swap the saved pointer of the graphics state. */
/* This is provided only for save/restore. */
gs_state *
gs_state_swap_saved(gs_state *pgs, gs_state *new_saved)
{	gs_state *saved = pgs->saved;
	pgs->saved = new_saved;
	return saved;
}

/* Swap the contents of two graphics states, except for the saved pointer. */
/* This is provided only for save/restore. */
void
gs_state_swap(gs_state *p1, gs_state *p2)
{	gs_state temp;
	temp = *p1, *p1 = *p2, *p2 = temp;
	/* Restore the saved pointers. */
	p2->saved = p1->saved;
	p1->saved = temp.saved;
}

/* ------ Operations on components ------ */

/* Reset most of the graphics state */
int
gs_initgraphics(register gs_state *pgs)
{	int code;
	gs_initmatrix(pgs);
	if (	(code = gs_newpath(pgs)) < 0 ||
		(code = gs_initclip(pgs)) < 0 ||
		(code = gs_setlinewidth(pgs, 1.0)) < 0 ||
		(code = gs_setlinecap(pgs, gs_cap_butt)) < 0 ||
		(code = gs_setlinejoin(pgs, gs_join_miter)) < 0 ||
		(code = gs_setdash(pgs, (float *)0, 0, 0.0)) < 0 ||
		(code = gs_setgray(pgs, 0.0)) < 0 ||
		(code = gs_setmiterlimit(pgs, 10.0)) < 0
	   ) return code;
	return 0;
}

/* setflat */
int
gs_setflat(gs_state *pgs, floatp flat)
{	if ( flat <= 0.2 ) flat = 0.2;
	else if ( flat > 100 ) flat = 100;
	pgs->flatness = flat;
	return 0;
}

/* currentflat */
float
gs_currentflat(const gs_state *pgs)
{	return pgs->flatness;
}

/* setstrokeadjust */
int
gs_setstrokeadjust(gs_state *pgs, int stroke_adjust)
{	pgs->stroke_adjust = stroke_adjust;
	return 0;
}

/* currentstrokeadjust */
int
gs_currentstrokeadjust(const gs_state *pgs)
{	return pgs->stroke_adjust;
}

/* ------ Internal routines ------ */

/* Allocate a graphics state object and its contents, */
/* optionally initializing it from an existing object. */
/* Return 0 if the allocation fails. */
private gs_state *
alloc_gstate(proc_alloc_t palloc, proc_free_t pfree, const gs_state *pold, const char *cname)
{	gs_state *pgs = (gs_state *)(*palloc)(1, sizeof(gs_state), cname);
	if ( pgs == 0 ) return 0;
	if ( pold != 0 )
		*pgs = *pold;
	else
		pgs->transfer = 0;
	pgs->memory_procs.alloc = palloc;
	pgs->memory_procs.free = pfree;
	if ( alloc_gstate_contents(pgs) < 0 )
	   {	free_struct(pgs, pgs, cname);
		return 0;
	   }
	return pgs;
}

/* Allocate the contents of a graphics state object. */
/* Return -1 if the allocation fails. */
/* Note that the contents have been smashed in this case. */
private int
alloc_gstate_contents(register gs_state *pgs)
{	proc_alloc_t palloc = pgs->memory_procs.alloc;
	static const char cname[] = "alloc_gstate_contents";
#define galloc(element,type,fail)\
    if ( (pgs->element = (type *)(*palloc)(1, sizeof(type), cname)) == 0 ) goto fail
	galloc(path, gx_path, up);
	galloc(clip_path, gx_clip_path, ucp);
	galloc(line_params, line_params, ulp);
	galloc(halftone, halftone_params, uht);
	galloc(color, gs_color, uc);
	galloc(dev_color, gx_device_color, udc);
	if ( pgs->transfer != 0 )
		pgs->transfer->ref_count++;
	else
	   {	galloc(transfer, gx_transfer, ut);
		pgs->transfer->ref_count = 1;
	   }
	galloc(device, device, ud);
#undef galloc
	pgs->device_is_shared = 0;
	return 0;
	/* Undo partial allocations if an allocation failed. */
#define gunalloc(element) free_struct(pgs, pgs->element, cname)
ud:	gunalloc(transfer);
ut:	gunalloc(dev_color);
udc:	gunalloc(color);
uc:	gunalloc(halftone);
uht:	gunalloc(line_params);
ulp:	gunalloc(clip_path);
ucp:	gunalloc(path);
up:	return -1;
#undef gunalloc
}

/* Free the contents of a graphics state, but not the state itself. */
private void
free_gstate_contents(gs_state *pgs)
{	proc_free_t pfree = pgs->memory_procs.free;
	static const char cname[] = "free_gstate_contents";
	gx_cpath_release(pgs->clip_path);
	gx_path_release(pgs->path);
#define gfree(element)\
    (*pfree)((char *)pgs->element, 1, sizeof(*pgs->element), cname)
	if ( !pgs->device_is_shared )
		gfree(device);
	if ( !--(pgs->transfer->ref_count) )
		gfree(transfer);
	gfree(dev_color);
	gfree(color);
	gfree(halftone);
	gfree(line_params);
	gfree(clip_path);
	gfree(path);
#undef gfree
}

/* Copy the composite parts of a graphics state. */
private void
copy_gstate_contents(gs_state *pto, const gs_state *pfrom)
{
#define gcopy(element)\
    *pto->element = *pfrom->element
	gcopy(path);
	gcopy(clip_path);
	gcopy(line_params);
	gcopy(halftone);
	gcopy(color);
	gcopy(dev_color);
	if ( pto->transfer != pfrom->transfer )
	   {	if ( !--(pto->transfer->ref_count) )
		   {	/* We could just copy the contents, but */
			/* we'd rather free the storage and hope that */
			/* we won't have to reallocate later. */
			free_struct(pto, pto->transfer, "copy gstate");
		   }
		(pto->transfer = pfrom->transfer)->ref_count++;
	   }
	gcopy(device);
#undef gcopy
	gx_path_share(pto->path);
	gx_cpath_share(pto->clip_path);
}
