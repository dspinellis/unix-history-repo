#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*	Routines dealing with output:
 *
 *	Do_PixFill, Do_TileFill, Do_StippleFill, Do_PixmapPut,
 *	Do_PixmapBitsPut, Do_BitmapBitsPut,
 *	Do_CopyArea,
 *	Do_Text, Do_TextMask,
 *	Do_Line, Do_Draw, Do_DrawFilled,
 *	Do_PixmapSave, Do_PixmapGet,
 *	Do_background, Do_refill, Do_border
 */
#ifndef lint
static char *rcsid_screen_c = "$Header: screen.c,v 10.7 86/03/25 11:34:11 jg Rel $";
#endif

#include "Xint.h"

extern DEVICE device;
extern u_char Xstatus;
extern WINDOW *rootwindow;
extern RECTANGLE *free_rectangles;
#ifdef DUALTCP
extern int swapped[];
#endif DUALTCP

RECTANGLE *Rec_intersection(), *Alloc_rectangle();
char *AllocateSpace();
PIXMAP *PixmapSave();

/* Do a PixFill in a window */

Do_PixFill (w, req, mask)
	register WINDOW *w;
	register XReq *req;
	BITMAP *mask;
{
#define src (int) req->param.u[4]
#define dst ((REGION *) req->param.s)
	register WINDOW *ow;
	register int i;
	CLIP *cliplist;

	if (req->func >= funclim) {
	    Xstatus = BadValue;
	    return;
	}
	if FALSE(w->mapped) return;
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;

	dst->left += w->full.left;
	dst->top += w->full.top;
	if TRUE(ow->unobscured) {
	    cliplist = &w->clip;
	    i = 1;
	} else {
	    dst->width += dst->left;
	    dst->height += dst->top;
	    if ((i = Get_clips (dst, w, ow, 1, &cliplist)) == 0)
		return;
	    dst->width -= dst->left;
	    dst->height -= dst->top;
	}
	PixFill (src, mask, dst->left, dst->top, dst->width, dst->height,
		 cliplist, i, (int) req->func, (int) req->mask);
#undef src
#undef dst
}

/* Do a TileFill in a window */

Do_TileFill (w, req, tile, mask)
	register WINDOW *w;
	register XReq *req;
	PIXMAP *tile;
	BITMAP *mask;
{
#define dst ((REGION *) req->param.s)
	register WINDOW *ow;
	register int i;
	CLIP *cliplist;

	if (req->func >= funclim) {
	    Xstatus = BadValue;
	    return;
	} else if FALSE(tile->tile) {
	    Xstatus = BadTile;
	    return;
	} else if (mask && (mask->height != dst->height ||
			    mask->width != dst->width)) {
	    Xstatus = BadMatch;
	    return;
	}
	if FALSE(w->mapped) return;
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;

	dst->left += w->full.left;
	dst->top += w->full.top;
	if TRUE(ow->unobscured) {
	    cliplist = &w->clip;
	    i = 1;
	} else {
	    dst->width += dst->left;
	    dst->height += dst->top;
	    if ((i = Get_clips (dst, w, ow, 1, &cliplist)) == 0)
		return;
	    dst->width -= dst->left;
	    dst->height -= dst->top;
	}
	while TRUE(w->tilemode) w = w->parent;
	TileFill (tile, w->full.left, w->full.top, mask, dst->left, dst->top,
		  dst->width, dst->height, cliplist, i,
		  (int) req->func, (int) req->mask);
#undef dst
}

/* Do a StippleFill in a window */
 
Do_StippleFill (w, req, mask)
	register WINDOW *w;
	register XReq *req;
	BITMAP *mask;
{
#define src (int) req->param.u[4]
#define dst ((REGION *) req->param.s)
	register WINDOW *ow;
	register int i;
	CLIP *cliplist;
 
	if (req->func >= funclim) {
	    Xstatus = BadValue;
	    return;
	}
	if FALSE(w->mapped) return;
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;
 
	dst->left += w->full.left;
	dst->top += w->full.top;
	if TRUE(ow->unobscured) {
	    cliplist = &w->clip;
	    i = 1;
	} else {
	    dst->width += dst->left;
	    dst->height += dst->top;
	    if ((i = Get_clips (dst, w, ow, 1, &cliplist)) == 0)
		return;
	    dst->width -= dst->left;
	    dst->height -= dst->top;
	}
	while TRUE(w->tilemode) w = w->parent;
	if (StippleFill (src, w->full.left, w->full.top, mask,
		dst->left, dst->top, dst->width, dst->height,
		cliplist, i, (int) req->func, (int) req->mask)
		== NULL) {
	    Xstatus = BadTile;
	    return;
	}
#undef src
#undef dst
}
/* Do a PixmapPut in a window */

Do_PixmapPut (w, req, pix)
	register WINDOW *w;
	register XReq *req;
	PIXMAP *pix;
{
#define src ((REGION *) req->param.s)
#define dst ((RASTER *) &req->param.s[4])
	register WINDOW *ow;
	register int i;
	CLIP *cliplist;

	if (req->func >= funclim) {
	    Xstatus = BadValue;
	    return;
	}
	if FALSE(w->mapped) return;
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;

	dst->left += w->full.left;
	dst->top += w->full.top;
	if TRUE(ow->unobscured) {
	    cliplist = &w->clip;
	    i = 1;
	} else {
	    dst->right = dst->left + src->width;
	    dst->bottom = dst->top + src->height;
	    if ((i = Get_clips (dst, w, ow, 1, &cliplist)) == 0)
		return;
	}
	PixmapPut (pix, src->left, src->top, src->width, src->height,
		   dst->left, dst->top, cliplist, i,
		   (int) req->func, (int) req->mask);
#undef src
#undef dst
}

/* Do a PixmapBitsPut in a window */

Do_PixmapBitsPut (w, req, data, mask)
	register WINDOW *w;
	register XReq *req;
	char *data;
	BITMAP *mask;
{
#define dst ((REGION *) req->param.s)
#define format req->param.s[4]
	register WINDOW *ow;
	register int i;
	CLIP *cliplist;

	if (req->func >= funclim) {
	    Xstatus = BadValue;
	    return;
	} else if (mask && (mask->height != dst->height ||
			    mask->width != dst->width)) {
	    Xstatus = BadMatch;
	    return;
	}
	if FALSE(w->mapped) return;
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;

	dst->left += w->full.left;
	dst->top += w->full.top;
	if TRUE(ow->unobscured) {
	    cliplist = &w->clip;
	    i = 1;
	} else {
	    dst->width += dst->left;
	    dst->height += dst->top;
	    if ((i = Get_clips (dst, w, ow, 1, &cliplist)) == 0)
		return;
	    dst->width -= dst->left;
	    dst->height -= dst->top;
	}
	PixmapBitsPut (dst->width, dst->height, format, data, mask,
		       dst->left, dst->top, cliplist, i,
		       (int) req->func, (int) req->mask);
#undef dst
#undef format
}

/* Do a BitmapBitsPut in a window */

Do_BitmapBitsPut (w, req, data, mask)
	register WINDOW *w;
	register XReq *req;
	char *data;
	BITMAP *mask;
{
#define dst ((REGION *) req->param.s)
#define fore (int) req->param.u[4]
#define back (int) req->param.u[5]
	register WINDOW *ow;
	register int i;
	CLIP *cliplist;

	if (req->func >= funclim) {
	    Xstatus = BadValue;
	    return;
	} else if (mask && (mask->height != dst->height ||
			    mask->width != dst->width)) {
	    Xstatus = BadMatch;
	    return;
	}
	if FALSE(w->mapped) return;
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;

	dst->left += w->full.left;
	dst->top += w->full.top;
	if TRUE(ow->unobscured) {
	    cliplist = &w->clip;
	    i = 1;
	} else {
	    dst->width += dst->left;
	    dst->height += dst->top;
	    if ((i = Get_clips (dst, w, ow, 1, &cliplist)) == 0)
		return;
	    dst->width -= dst->left;
	    dst->height -= dst->top;
	}
	BitmapBitsPut (dst->width, dst->height, data, fore, back, mask,
		       dst->left, dst->top, cliplist, i,
		       (int) req->func, (int) req->mask);
#undef dst
#undef fore
#undef back
}

/* Does a CopyArea in a window */

Do_CopyArea (w, req)
	register WINDOW *w;
	register XReq *req;
{
#define src ((REGION *) req->param.s)
#define srs ((RASTER *) req->param.s)
#define dstx req->param.s[6]
#define dsty req->param.s[7]
	register RECTANGLE *v1, *v2;
	register WINDOW *ow;
	RECTANGLE rec, *vlist, *vis, *new, *end;
	RASTER temp;
	int i;
	CLIP *cliplist, *clipptr;
	PIXMAP *tile;

	if (req->func >= funclim) {
	    Xstatus = BadValue;
	    return;
	}
	if FALSE(w->mapped) {
	    Stash_misses (w, (RECTANGLE *) NULL);
	    return;
	}
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;

	dstx -= src->left;
	dsty -= src->top;
	src->left += w->full.left;
	src->top += w->full.top;
	if TRUE(ow->unobscured) {
	    /* no hairy stuff */
	    CopyArea (src->left, src->top, src->width, src->height,
		      src->left + dstx, src->top + dsty,
		      &w->clip, 1, (int) req->func, (int) req->mask);
	    srs->right += srs->left;
	    srs->bottom += srs->top;
	    /* unless part of the source is invisible */
	    if (srs->left >= w->vs.left && srs->top >= w->vs.top &&
		srs->right <= w->vs.right && srs->bottom <= w->vs.bottom) {
		Stash_misses (w, (RECTANGLE *) NULL);
		return;
	    }
	    vlist = ow->visible;
	    /* get destination */
	    rec.left = srs->left + dstx;
	    rec.top = srs->top + dsty;
	    rec.right = srs->right + dstx;
	    rec.bottom = srs->bottom + dsty;
	    /* get visible source */
	    Clip_raster (srs, &w->vs);
	    srs->left += dstx;
	    srs->top += dsty;
	    srs->right += dstx;
	    srs->bottom += dsty;
	    /* get updated destination */
	    Clip_raster (srs, &w->vs);
	    RASTRECT(v1, *srs, contents_rec);
	    rec.next = v1;
	    rec.next->next = NULL;
	} else {
	    if TRUE(w->clipmode)
		vlist = ow->cmvisible;
	    else
		vlist = ow->visible;
	    /* get source */
	    *(REGION *) &rec = *src;
	    rec.right += src->left;
	    rec.bottom += src->top;
	    rec.next = NULL;
	    i = 0;
	    for (v1 = vlist; v1; v1 = v1->next) {
		/* get visible source */
		if (v1->type != contents_rec ||
		    (vis = Rec_intersection (v1, &rec)) == NULL ||
		    (w != ow && TRUE(Clip_rectangle (vis, &w->vs))))
		    continue;
		/* get destination */
		vis->left += dstx;
		vis->top += dsty;
		vis->right += dstx;
		vis->bottom += dsty;
		for (v2 = vlist; v2; v2 = v2->next) {
		    /* get visible destination */
		    if (v2->type != contents_rec ||
			(new = Rec_intersection (v2, vis)) == NULL ||
			(w != ow && TRUE(Clip_rectangle (new, &w->vs))))
			continue;
		    /* try to merge it in */
		    for (end = rec.next; end; end = end->next) {
			if (end->left != new->left || end->right != new->right)
			    continue;
			if (end->top == new->bottom) {
			    end->top = new->top;
			} else if (end->bottom == new->top) {
			    end->bottom = new->bottom;
			} else
			    continue;
			FREERECT(new);
			new = NULL;
			break;
		    }
		    if (new == NULL)
			continue;
		    new->next = rec.next;
		    rec.next = new;
		    i++;
		}
		FREERECT(vis);
	    }
	    /* reorder by direction of motion */
	    for (v1 = rec.next; v1; v1 = v1->next) {
		for (v2 = v1->next; v2; v2 = v2->next) {
		    if ((((dsty <= 0 && v1->top == v2->top) ||
			  (dsty > 0 && v1->bottom == v2->bottom)) &&
			    ((dstx < 0 && v1->left > v2->left) ||
			     (dstx > 0 && v1->right < v2->right))) ||
			(dsty < 0 && v1->top > v2->top) ||
			(dsty > 0 && v1->bottom < v2->bottom)) {
			temp = *(RASTER *) v1;
			*(RASTER *) v1 = *(RASTER *) v2;
			*(RASTER *) v2 = temp;
		    }
		}
	    }
	    if (i) {
		clipptr = (CLIP *) AllocateSpace (i * sizeof (CLIP));
		if (cliplist = clipptr) {
		    for (v1 = rec.next; v1; v1 = v1->next) {
			clipptr->left = v1->left;
			clipptr->top = v1->top;
			clipptr->height = v1->bottom - v1->top;
			clipptr->width = v1->right - v1->left;
			clipptr++;
		    }
		    CopyArea (src->left, src->top, src->width, src->height,
			      src->left + dstx, src->top + dsty,
			      cliplist, i, (int) req->func, (int) req->mask);
		} else
		    DeviceError ("Cliplist too large");
	    }
	    /* get destination */
	    rec.left += dstx;
	    rec.top += dsty;
	    rec.right += dstx;
	    rec.bottom += dsty;
	}
	vis = NULL;
	for (v1 = vlist; v1; v1 = v1->next) {
	    /* get updated destination */
	    if (v1->type != contents_rec ||
		(v2 = Rec_intersection (v1, &rec)) == NULL ||
		(w != ow && TRUE(Clip_rectangle (v2, &w->vs))))
		continue;
	    v2->next = vis;
	    vis = v2;
	}
	/* get missed but visible destination */
	for (v1 = rec.next; v1; v1 = v2) {
	    Calc_overlaps ((RASTER *)v1, &vis);
	    v2 = v1->next;
	    FREERECT(v1);
	}
	i = 0;
	for (v1 = vis; v1; v1 = v1->next)
	    i++;
	if (i) {
	    clipptr = (CLIP *) AllocateSpace (i * sizeof (CLIP));
	    if (cliplist = clipptr) {
		for (v1 = vis; v1; v1 = v1->next) {
		    clipptr->left = v1->left;
		    clipptr->top = v1->top;
		    clipptr->height = v1->bottom - v1->top;
		    clipptr->width = v1->right - v1->left;
		    clipptr++;
		}
	    }
	    tile = ow->tile;
	    ow = w;
	    while TRUE(ow->tilemode) ow = ow->parent;
	    if (clipptr == NULL)
		DeviceError ("Cliplist too large");
	    else
		TileFill (tile, ow->full.left, ow->full.top, (BITMAP *) NULL,
			  rec.left, rec.top,
			  rec.right - rec.left, rec.bottom - rec.top,
			  cliplist, i, (int) req->func, (int) req->mask);
	}
	Stash_misses (w, vis);
#undef src
#undef srs
#undef dstx
#undef dsty
}

/* Writes text with a source font in a window */

Do_Text (w, req, text, font)
	register WINDOW *w;
	register XReq *req;
	char *text;
	FONT *font;
{
#define dstx req->param.s[0]
#define dsty req->param.s[1]
#define fore (int) req->param.u[4]
#define back (int) req->param.u[5]
#define ccount req->param.s[6]
#define cpad req->param.b[14]
#define spad req->param.b[15]
	register WINDOW *ow;
	RASTER bound;
	register int i;
	CLIP *cliplist;

	if (req->func >= funclim) {
	    Xstatus = BadValue;
	    return;
	}
	if (FALSE(w->mapped) || ccount == 0) return;
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;

	bound.left = dstx + w->full.left;
	bound.top = dsty + w->full.top;
	if TRUE(ow->unobscured) {
	    cliplist = &w->clip;
	    i = 1;
	} else {
	    bound.bottom = bound.top + font->height;
	    bound.right = bound.left + ccount * cpad +
			  TextWidth (text, ccount, spad, font);
	    if ((i = Get_clips (&bound, w, ow, 1, &cliplist)) == 0)
		return;
	}
	PrintText (text, ccount, font, fore, back, cpad, spad,
		   bound.left, bound.top, cliplist, i,
		   (int) req->func, (int) req->mask);
#undef dstx
#undef dsty
#undef fore
#undef back
#undef ccount
#undef cpad
#undef spad
}

/* Writes text with a mask font in a window */

Do_TextMask (w, req, text, font)
	register WINDOW *w;
	register XReq *req;
	char *text;
	FONT *font;
{
#define dstx req->param.s[0]
#define dsty req->param.s[1]
#define src (int) req->param.u[4]
#define ccount req->param.s[6]
#define cpad req->param.b[14]
#define spad req->param.b[15]
	register WINDOW *ow;
	RASTER bound;
	register int i;
	CLIP *cliplist;

	if (req->func >= funclim) {
	    Xstatus = BadValue;
	    return;
	}
	if (FALSE(w->mapped) || ccount == 0) return;
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;

	bound.left = dstx + w->full.left;
	bound.top = dsty + w->full.top;
	if TRUE(ow->unobscured) {
	    cliplist = &w->clip;
	    i = 1;
	} else {
	    bound.bottom = bound.top + font->height;
	    bound.right = bound.left + ccount * cpad +
			  TextWidth (text, ccount, spad, font);
	    if ((i = Get_clips (&bound, w, ow, 1, &cliplist)) == 0)
		return;
	}
	PrintTextMask (text, ccount, font, src, cpad, spad,
		       bound.left, bound.top, cliplist, i,
		       (int) req->func, (int) req->mask);
#undef dstx
#undef dsty
#undef ccount
#undef src
#undef cpad
#undef spad
}

/* Draws a line in a window */

Do_Line (w, req)
	register WINDOW *w;
	register XReq *req;
{
#define x1 req->param.s[0]
#define y1 req->param.s[1]
#define x2 req->param.s[2]
#define y2 req->param.s[3]
#define src (int) req->param.u[4]
#define brushy req->param.b[10]
#define brushx req->param.b[11]
	register WINDOW *ow;
	RASTER bound;
	register int i;
	CLIP *cliplist;
	Vertex vlist[2];

	if (req->func >= funclim || brushx <= 0 || brushy <= 0) {
	    Xstatus = BadValue;
	    return;
	}
	if FALSE(w->mapped) return;
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;

	if TRUE(ow->unobscured) {
	    cliplist = &w->clip;
	    i = 1;
	} else {
	    /* we can only approximate a minimal clipping region */
	    if (x1 <= x2) {
		bound.left = x1;
		bound.right = x2;
	    } else {
		bound.left = x2;
		bound.right = x1;
	    }
	    bound.left += w->full.left - brushx;
	    bound.right += w->full.left + brushx;
	    if (y1 <= y2) {
		bound.top = y1;
		bound.bottom = y2;
	    } else {
		bound.top = y2;
		bound.bottom = y1;
	    }
	    bound.top += w->full.top - brushy;
	    bound.bottom += w->full.top + brushy;
	    if ((i = Get_clips (&bound, w, ow, 1, &cliplist)) == 0)
		return;
	}
	vlist[0].x = x1;
	vlist[0].y = y1;
	vlist[0].flags = VertexDontDraw;
	vlist[1].x = x2;
	vlist[1].y = y2;
	vlist[1].flags = VertexDrawLastPoint;
	DrawCurve (vlist, 2, w->full.left, w->full.top, src, 0, DrawSolidLine,
		   brushx, brushy, 0, 0, 0, cliplist, i, (int) req->func,
		   (int) req->mask);
#undef x1
#undef y1
#undef x2
#undef y2
#undef src
#undef brushx
#undef brushy
}

/* Draws a polygon/curve in a window */

Do_Draw (w, req, vlist)
	register WINDOW *w;
	register XReq *req;
	Vertex *vlist;
{
#define vcount req->param.s[0]
#define src (int) req->param.u[1]
#define brushy req->param.b[4]
#define brushx req->param.b[5]
#define dmode req->param.s[3]
#define altsrc (int) req->param.u[4]
#define patstr req->param.s[5]
#define patlen req->param.s[6]
#define patmul req->param.s[7]
	register WINDOW *ow;
	register int i;
	CLIP *cliplist;

	if (req->func >= funclim || brushx <= 0 || brushy <= 0 ||
	    dmode < 0 || dmode > 2 ||
	    (dmode > 0 && (patlen <= 0 || patlen > 16 || patmul <= 0))) {
	    Xstatus = BadValue;
	    return;
	}
	if (FALSE(w->mapped) || vcount == 0) return;
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;

	if TRUE(ow->unobscured) {
	    cliplist = &w->clip;
	    i = 1;
	/* it is generally impossible for us to compute a minimal clip */
	} else if ((i = Get_clips (&w->vs, w, ow, 1, &cliplist)) == 0)
	    return;

	vlist->flags = (vlist->flags | VertexDontDraw) & ~VertexRelative;
	DrawCurve (vlist, vcount, w->full.left, w->full.top, src, altsrc,
		   dmode, brushx, brushy, patstr, patlen, patmul, cliplist, i,
		   (int) req->func, (int) req->mask);
#undef vcount
#undef src
#undef brushy
#undef brushx
#undef dmode
#undef altsrc
#undef patstr
#undef patlen
#undef patmul
}

/* Draws a filled polygon/curve in a window */

Do_DrawFilled (w, req, vlist, tile)
	register WINDOW *w;
	register XReq *req;
	Vertex *vlist;
	PIXMAP *tile;
{
#define vcount req->param.s[0]
#define src (int) req->param.u[1]
	register WINDOW *ow;
	register int i;
	CLIP *cliplist;

	if (req->func >= funclim) {
	    Xstatus = BadValue;
	    return;
	} else if (tile && FALSE(tile->tile)) {
	    Xstatus = BadTile;
	    return;
	} else if (FALSE(w->mapped) || vcount == 0)
	    return;

	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;

	if TRUE(ow->unobscured) {
	    cliplist = &w->clip;
	    i = 1;
	/* it is generally impossible for us to compute a minimal clip */
	} else if ((i = Get_clips (&w->vs, w, ow, 1, &cliplist)) == 0)
	    return;
	ow = w;
	while TRUE(ow->tilemode) ow = ow->parent;

	vlist->flags = (vlist->flags | VertexDontDraw) & ~VertexRelative;
	DrawFilled (vlist, vcount, w->full.left, w->full.top, src, tile,
		    ow->full.left, ow->full.top, cliplist, i, (int) req->func,
		    (int) req->mask);
#undef vcount
#undef src
}

/* Save a region of a window */

PIXMAP *Do_PixmapSave (w, req)
	register WINDOW *w;
	register XReq *req;
{
#define src ((REGION *) req->param.s)
	PIXMAP *pix;

	if (src->height <= 0 || src->width <= 0) {
	    Xstatus = BadValue;
	    return (NULL);
	}

	src->left += w->full.left;
	src->top += w->full.top;

	if (FALSE(w->mapped) ||
	    src->left < w->vs.left || src->top < w->vs.top ||
	    src->left + src->width > w->vs.right ||
	    src->top + src->height > w->vs.bottom) {
	    Xstatus = BadValue;
	    return (NULL);
	}

	pix = PixmapSave (src->left, src->top, src->width, src->height);
	if (pix == NULL)
	    Xstatus = BadAlloc;
	return (pix);
#undef src
}

/* Read a region of a window */

Do_PixmapGet (w, req, client)
	register WINDOW *w;
	register XReq *req;
	int client;
{
#define src ((REGION *) req->param.s)
	XRep rep;
#ifdef DUALTCP
	register swaptype n;
#endif

	if ((rep.param.l[0] = Pix_size ((int) req->func,
					src->height, src->width)) == 0)
	    return;

	src->left += w->full.left;
	src->top += w->full.top;

	if (FALSE(w->mapped) ||
	    src->left < w->vs.left || src->top < w->vs.top ||
	    src->left + src->width > w->vs.right ||
	    src->top + src->height > w->vs.bottom) {
	    Xstatus = BadValue;
	    return;
	}

	rep.code = X_Reply;
#ifdef DUALTCP
	if (swapped[client]) {
	    pswapl(&rep, 0);
	}
#endif
	Write (client, (caddr_t) &rep, sizeof (XRep));
	
	PixmapGet (src->left, src->top, src->width, src->height,
		   client, (int) req->func,
#ifdef DUALTCP
		   swapped[client]
#else
		   0
#endif
		  );
#undef src
}

/* Fills a window with the background.
 * Fills all the window if not_just_new is set, else just the changes.
 */

Do_background (w, not_just_new)
	register WINDOW *w;
	int not_just_new;
{
	register WINDOW *ow;
	register int i;
	CLIP *cliplist;
	PIXMAP *tile;

	if FALSE(w->mapped) return;
	ow = w;
	while (ow->kind == IsTransparent) ow = ow->parent;
	if (ow->visible == NULL) return;

	if (TRUE(not_just_new) && TRUE(ow->unobscured)) {
	    cliplist = &w->clip;
	    i = 1;
	} else if ((i = Get_clips (&w->vs, (WINDOW *) NULL, ow, not_just_new,
				   &cliplist)) == 0)
	    return;
	tile = ow->tile;
	ow = w;
	while TRUE(ow->tilemode) ow = ow->parent;
	TileFill (tile, ow->full.left, ow->full.top, (BITMAP *) NULL,
		  w->vs.left, w->vs.top,
		  w->vs.right - w->vs.left, w->vs.bottom - w->vs.top,
		  cliplist, i, GXcopy, 0xffff);
}

/* Move the contents of a window on the screen and redisplay the border. */

Do_refill (w, dx, dy)
	register WINDOW *w;
	int dx, dy;
{
	register RECTANGLE *v1, *v2;
	register CLIP *clipptr;
	register int i;
	CLIP *cliplist;
	RASTER temp;

	/* Order rectangles by direction of motion */
	i = 0;
	for (v1 = w->cmvisible; v1; v1 = v1->next) {
	    i++;
	    for (v2 = v1->next; v2; v2 = v2->next) {
		if ((((dy <= 0 && v1->top == v2->top) ||
		      (dy > 0 && v1->bottom == v2->bottom)) &&
			((dx < 0 && v1->left > v2->left) ||
			 (dx > 0 && v1->right < v2->right))) ||
		    (dy < 0 && v1->top > v2->top) ||
		    (dy > 0 && v1->bottom < v2->bottom)) {
		    temp = *(RASTER *) v1;
		    *(RASTER *) v1 = *(RASTER *) v2;
		    *(RASTER *) v2 = temp;
		}
	    }
	}
	if (i) {
	    clipptr = (CLIP *) AllocateSpace (i * sizeof (CLIP));
	    if (cliplist = clipptr) {
		for (v1 = w->cmvisible; v1; v1 = v1->next) {
		    clipptr->left = v1->left;
		    clipptr->top = v1->top;
		    clipptr->height = v1->bottom - v1->top;
		    clipptr->width = v1->right - v1->left;
		    clipptr++;
		}
		CopyArea (w->vs.left - dx, w->vs.top - dy,
			  w->vs.right - w->vs.left, w->vs.bottom - w->vs.top,
			  w->vs.left, w->vs.top, cliplist, i, GXcopy, 0xffff);
	    } else
		DeviceError ("Cliplist too large");

	}
	Do_border (w);
}

/* Repaint the border of a window */

Do_border (w)
	register WINDOW *w;
{
	register RECTANGLE *r;
	register int i = 0;
	CLIP *cliplist;
	register CLIP *clipptr;

	for (r = w->visible; r; r = r->next) {
	    if (r->type == border_rec) i++;
	}

	if (i == 0) return;

	clipptr = (CLIP *) AllocateSpace (i * sizeof (CLIP));
	if (clipptr == NULL) {
	    DeviceError ("Cliplist too large");
	    return;
	}
	cliplist = clipptr;
	for (r = w->visible; r; r = r->next) {
	    if (r->type == border_rec) {
		clipptr->left = r->left;
		clipptr->top = r->top;
		clipptr->height = r->bottom - r->top;
		clipptr->width = r->right - r->left;
		clipptr++;
	    }
	}

	TileFill (w->border, w->full.left - w->bwidth, w->full.top - w->bwidth,
		  (BITMAP *) NULL,
		  w->full.left - w->bwidth, w->full.top - w->bwidth,
		  w->full.right - w->full.left + (w->bwidth << 1),
		  w->full.bottom - w->full.top + (w->bwidth << 1),
		  cliplist, i, GXcopy, 0xffff);
}

/* Compute a clip list for a destination, and return clip count.
 * w is the original window, ow is the actual output window.
 * If not_just_new, compute for all of the window, else just the changes.
 * We assume that it is better (cheaper/faster) for us to compute a minimal
 * clip list than to expect all devices to do the computation.
 */

Get_clips (dst, w, ow, not_just_new, cliplist)
	register RASTER *dst;
	WINDOW *w, *ow;
	register int not_just_new;
	CLIP **cliplist;
{
	register RECTANGLE *r;
	register RECTANGLE **ptr1, **ptr2;
	RECTANGLE *clips;
	register CLIP *clipptr;
	register int i;
	RASTER rast;

	if (w && TRUE(w->clipmode)) {
	    if (ow == rootwindow) {
		*cliplist = &w->clip;
		return (1);
	    }
	    ptr1 = &ow->cmvisible;
	} else
	    ptr1 = &ow->visible;
	if (w && w != ow) {
	    rast = *dst;
	    dst = &rast;
	    Clip_raster (dst, &w->vs);
	}
	ptr2 = &clips;
	i = 0;
	for (r = *ptr1; r; r = r->next) {
	    if (((r->type == contents_rec && not_just_new) ||
		 (r->type == new_rec)) &&
		max(dst->left, r->left) < min(dst->right, r->right) &&
		max(dst->top, r->top) < min(dst->bottom, r->bottom)) {
		*ptr2 = r;
		ptr2 = &r->next;
		i++;
	    } else {
		*ptr1 = r;
		ptr1 = &r->next;
	    }
	}
	*ptr2 = NULL;
	*ptr1 = clips;
	if (i == 0)
	    return (0);

	clipptr = (CLIP *) AllocateSpace (i * sizeof (CLIP));
	if (clipptr == NULL) {
	    DeviceError ("Cliplist too large");
	    return (0);
	}
	*cliplist = clipptr;
	for (r = clips; r; r = r->next) {
	    clipptr->left = max(r->left, dst->left);
	    clipptr->top = max(r->top, dst->top);
	    clipptr->height = min(r->bottom, dst->bottom) - clipptr->top;
	    clipptr->width = min(r->right, dst->right) - clipptr->left;
	    clipptr++;
	}
	return (i);
}
