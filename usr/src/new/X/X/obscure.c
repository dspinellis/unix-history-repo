#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*	Routines for calculating how windows obscure each other:
 *
 *	Obscure_top, Obscure_bottom, Remove_rectangles
 */
 
#ifndef lint
static char *rcsid_obscure_c = "$Header: obscure.c,v 10.7 86/02/01 15:16:51 tony Rel $";
#endif

#include "Xint.h"

extern RECTANGLE *free_rectangles;

RECTANGLE *Alloc_rectangle(), **Do_overlap();

/* Obscure_top allocates the rectangles for a top window, and updates the lists
 * of visible rectangles in other mapped windows to reflect obscuring.
 */

Obscure_top (w)
	register WINDOW *w;
{
	register WINDOW *w1;
	register RECTANGLE *pr, *v, *r;
	RECTANGLE crec, brec1, brec2, brec3, brec4;
	int done;

	/* Compute visible rectangles from what parent has to offer */

	*(RASTER *) &crec = w->vs;
	done = 0;
	for (pr = w->parent->cmvisible; pr; pr = pr->next) {
	    if (crec.left >= pr->right || pr->left >= crec.right ||
		crec.top >= pr->bottom || pr->top >= crec.bottom)
		continue;
	    NEWRECT(v, max(crec.left, pr->left), min(crec.right, pr->right),
		    max(crec.top, pr->top), min(crec.bottom, pr->bottom),
		    contents_rec);
	    if (v->left == crec.left && v->right == crec.right &&
		v->top == crec.top && v->bottom == crec.bottom)
		done = 1;
	    RASTRECT(r, *(RASTER *) v, contents_rec);
	    if (w->visible && TRUE(Merge_vertical (v, &w->visible, 1)))
		Merge_vertical (r, &w->cmvisible, 1);
	    else {
		v->next = w->visible;
		w->visible = v;
		r->next = w->cmvisible;
		w->cmvisible = r;
	    }
	    if TRUE(done) break;
	}
	w->unobscured = OB_NOT;
	Windex (w);

	if (w->bwidth) {
	    /* Compute the visible border */
	    brec1.left = w->full.left - w->bwidth;
	    brec1.right = w->full.left;
	    brec1.top = w->full.top;
	    brec1.bottom = w->full.bottom;
	    brec1.next = &brec2;
	    brec2.left = w->full.right;
	    brec2.right = w->full.right + w->bwidth;
	    brec2.top = w->full.top;
	    brec2.bottom = w->full.bottom;
	    brec2.next = &brec3;
	    brec3.left = w->full.left - w->bwidth;
	    brec3.right = w->full.right + w->bwidth;
	    brec3.top = w->full.top - w->bwidth;
	    brec3.bottom = w->full.top;
	    brec3.next = &brec4;
	    brec4.left = w->full.left - w->bwidth;
	    brec4.right = w->full.right + w->bwidth;
	    brec4.top = w->full.bottom;
	    brec4.bottom = w->full.bottom + w->bwidth;
	    brec4.type = border_rec;
	    brec4.next = NULL;

	    r = &brec1;
	    do {
		done = 0;
		/* again, use what parent has to offer */
		for (pr = w->parent->cmvisible; pr; pr = pr->next) {
		    if (r->left >= pr->right || pr->left >= r->right ||
			r->top >= pr->bottom || pr->top >= r->bottom)
			continue;
		    NEWRECT(v, max(r->left, pr->left), min(r->right, pr->right),
			    max(r->top, pr->top), min(r->bottom, pr->bottom),
			    border_rec);
		    if (v->left == r->left && v->right == r->right &&
			v->top == r->top && v->bottom == r->bottom)
			done = 1;
		    if (w->visible == NULL ||
			FALSE(Merge_vertical (v, &w->visible, 1))) {
			v->next = w->visible;
			w->visible = v;
		    }
		    if TRUE(done) break;
		}
	    } while (r = r->next);
	}

	if (w->visible == NULL)
	    return;

	/* Let the window obscure the rest of the list */

	w1 = w;
	while (1) {
	    w1 = w1->next;

	    if (w1->kind == IsTransparent ||
		w->ovs.left >= w1->ovs.right || w1->ovs.left >= w->ovs.right ||
		w->ovs.top >= w1->ovs.bottom || w1->ovs.top >= w->ovs.bottom) {
		if (w1 != w->parent)
		    continue;
		return;
	    }

	    if (TRUE(Calc_overlaps (&w->ovs, &w1->visible)) && w1->unobscured == OB_YES)
		w1->unobscured = OB_NOT;
	    if (w1 == w->parent)
		return;
	    Calc_overlaps (&w->ovs, &w1->cmvisible);
	}
}

/* Obscure_bottom allocates the rectangles for a bottom window, and updates the
 * lists of visible rectangles in the parent to reflect obscuring.
 */

Obscure_bottom (w)
	register WINDOW *w;
{
	register WINDOW *w1;
	register RECTANGLE **prev, *v, *pr, *r;
	RECTANGLE crec, brec1, brec2, brec3, brec4;
	int done;

	/* Compute visible rectangles from what parent has left */

	*(RASTER *) &crec = w->vs;
	done = 0;
	w1 = w->parent;
	prev = &w1->visible;
	while (pr = *prev) {
	    if (pr->type == border_rec ||
		crec.left >= pr->right || pr->left >= crec.right ||
		crec.top >= pr->bottom || pr->top >= crec.bottom) {
		prev = &pr->next;
		continue;
	    }
	    NEWRECT(v, max(crec.left, pr->left), min(crec.right, pr->right),
		    max(crec.top, pr->top), min(crec.bottom, pr->bottom),
		    contents_rec);
	    if (v->left == crec.left && v->right == crec.right &&
		v->top == crec.top && v->bottom == crec.bottom)
		done = 1;
	    /* compute what remains for parent */
	    prev = Do_overlap ((RASTER *) v, prev, &w1->visible);
	    if (w1->unobscured == OB_YES)
		w1->unobscured = OB_NOT;
	    RASTRECT(r, *(RASTER *) v, contents_rec);
	    if (w->visible && TRUE(Merge_vertical (v, &w->visible, 1)))
		Merge_vertical (r, &w->cmvisible, 1);
	    else {
		v->next = w->visible;
		w->visible = v;
		r->next = w->cmvisible;
		w->cmvisible = r;
	    }
	    if TRUE(done) break;
	}
	w->unobscured = OB_NOT;
	Windex (w);

	if (w->bwidth) {
	    /* Compute visible border from what parent has left */
	    brec1.left = w->full.left - w->bwidth;
	    brec1.right = w->full.left;
	    brec1.top = w->full.top;
	    brec1.bottom = w->full.bottom;
	    brec1.next = &brec2;
	    brec2.left = w->full.right;
	    brec2.right = w->full.right + w->bwidth;
	    brec2.top = w->full.top;
	    brec2.bottom = w->full.bottom;
	    brec2.next = &brec3;
	    brec3.left = w->full.left - w->bwidth;
	    brec3.right = w->full.right + w->bwidth;
	    brec3.top = w->full.top - w->bwidth;
	    brec3.bottom = w->full.top;
	    brec3.next = &brec4;
	    brec4.left = w->full.left - w->bwidth;
	    brec4.right = w->full.right + w->bwidth;
	    brec4.top = w->full.bottom;
	    brec4.bottom = w->full.bottom + w->bwidth;
	    brec4.type = border_rec;
	    brec4.next = NULL;

	    r = &brec1;
	    do {
		done = 0;
		prev = &w1->visible;
		while (pr = *prev) {
		    if (pr->type == border_rec ||
			r->left >= pr->right || pr->left >= r->right ||
			r->top >= pr->bottom || pr->top >= r->bottom) {
			prev = &pr->next;
			continue;
		    }
		    NEWRECT(v, max(r->left, pr->left), min(r->right, pr->right),
			    max(r->top, pr->top), min(r->bottom, pr->bottom),
			    border_rec);
		    if (v->left == r->left && v->right == r->right &&
			v->top == r->top && v->bottom == r->bottom)
			done = 1;
		    /* compute what remains in parent */
		    prev = Do_overlap ((RASTER *) v, prev, &w1->visible);
		    if (w1->unobscured == OB_YES)
			w1->unobscured = OB_NOT;
		    if (w->visible == NULL ||
			FALSE(Merge_vertical (v, &w->visible, 1))) {
			v->next = w->visible;
			w->visible = v;
		    }
		    if TRUE(done) break;
		}
	    } while (r = r->next);
	}
}

/* Remove_rectangles removes rectangles from remw and gives the space to
 * the various windows in the chain.  It does this by moving down the list,
 * giving space to each window in turn.  If cleanup = 1, then update and
 * display windows you give space to.  If cleanup < 0, then update the
 * windows without changing the bits on the screen.  If, in the course
 * of following the chain, remw is encountered, we stop.
 */

Remove_rectangles (remw, chain, cleanup)
	register WINDOW *remw;
	WINDOW *chain;
	int cleanup;
{
	register RECTANGLE *r, **oldr;
	register WINDOW *w, *ww;
	register RECTANGLE *addrec;
	RECTANGLE *new;

	for (w = chain;	remw->visible && w != remw; w = w->next) {

	    if (w->kind == IsTransparent ||
		w->ovs.left >= remw->ovs.right || remw->ovs.left >= w->ovs.right ||
		w->ovs.top >= remw->ovs.bottom || remw->ovs.top >= w->ovs.bottom)
		continue;

	    new = NULL;

	    oldr = &remw->visible;
	    while (r = *oldr) {
		if (w->ovs.left >= r->right || r->left >= w->ovs.right ||
		    w->ovs.top >= r->bottom || r->top >= w->ovs.bottom) {
		    oldr = &r->next;
		} else {
		    /* compute what remains */
		    oldr = Do_overlap (&w->ovs, oldr, &remw->visible);
		    remw->unobscured = OB_NOT;
		    /* get the piece it wants */
		    Clip_rectangle (r, &w->ovs);
		    /* remove it */
		    if (r->type != border_rec && remw->cmvisible)
			Calc_overlaps ((RASTER *)r, &remw->cmvisible);

		    /* give it to our elders too */
		    if (w->level > remw->level) {
			ww = w;
			while ((ww = ww->parent)->level >= remw->level) {
			    RASTRECT(addrec, *(RASTER *) r, new_rec);
			    addrec->next = ww->cmvisible;
			    ww->cmvisible = addrec;
			}
		    }

		    /* If the rectangle enters the border of w, split it up */

		    if (w->bwidth) {
			if (r->left < w->vs.left) {
			    if (r->right <= w->vs.left) {
				r->type = border_rec;
				r->next = new;
				new = r;
				continue;
			    }
			    NEWRECT(addrec, r->left, w->vs.left,
				    r->top, r->bottom, border_rec);
			    addrec->next = new;
			    new = addrec;
			    r->left = w->vs.left;
			}
			if (r->right > w->vs.right) {
			    if (r->left >= w->vs.right) {
				r->type = border_rec;
				r->next = new;
				new = r;
				continue;
			    }
			    NEWRECT(addrec, w->vs.right, r->right,
				    r->top, r->bottom, border_rec);
			    addrec->next = new;
			    new = addrec;
			    r->right = w->vs.right;
			}
			if (r->top < w->vs.top) {
			    if (r->bottom <= w->vs.top) {
				r->type = border_rec;
				r->next = new;
				new = r;
				continue;
			    }
			    NEWRECT(addrec, r->left, r->right,
				    r->top, w->vs.top, border_rec);
			    addrec->next = new;
			    new = addrec;
			    r->top = w->vs.top;
			}
			if (r->bottom > w->vs.bottom) {
			    if (r->top >= w->vs.bottom) {
				r->type = border_rec;
				r->next = new;
				new = r;
				continue;
			    }
			    NEWRECT(addrec, r->left, r->right,
				    w->vs.bottom, r->bottom, border_rec);
			    addrec->next = new;
			    new = addrec;
			    r->bottom = w->vs.bottom;
			}
		    }

		    /* give it to ourselves too */
		    if (w->level >= remw->level) {
			RASTRECT(addrec, *(RASTER *) r, new_rec);
			addrec->next = w->cmvisible;
			w->cmvisible = addrec;
		    }

		    if (cleanup >= 0)
			r->type = new_rec;
		    else
			r->type = contents_rec;
		    r->next = new;
		    new = r;
		}
	    }

	    if (new) {
		for (ww = w; ww->level >= remw->level; ww = ww->parent) {
		    /* find any new stuff and merge it */
		    addrec = NULL;
		    while ((r = ww->cmvisible) && r->type == new_rec) {
			r->type = contents_rec;
			ww->cmvisible = r->next;
			r->next = addrec;
			addrec = r;
		    }
		    if (addrec)
			Merge_rectangles (addrec, &ww->cmvisible);
		}
		Merge_rectangles (new, &w->visible);
		Windex (w);
		if (cleanup > 0)
		    Draw_window (w, 0, 0);
		else if (cleanup == 0)
		    w->unobscured = OB_TMP;
	    }
	}
}
