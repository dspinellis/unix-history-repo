#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*	Routines to calculate how rectangles overlap and abut:
 *
 *	Calc_overlaps, Do_overlap,
 *	Clip_raster, Clip_rectangle, Rec_intersection,
 *	Merge_vertical, Merge_rectangles
 */
 
#ifndef lint
static char *rcsid_overlap_c = "$Header: overlap.c,v 10.6 86/02/01 15:16:58 tony Rel $";
#endif

#include "Xint.h"

extern RECTANGLE *free_rectangles;

RECTANGLE **Do_overlap(), *Alloc_rectangle();

/* Calc_overlaps figures out how raster rast overlaps rectangles rects and
 * recreates in rects a list of the maximally wide visible rectangles.
 * It returns 0 if no overlap, 1 if overlap.
 */

int Calc_overlaps (rast, rects)
	register RASTER *rast;
	register RECTANGLE **rects;
{
	register RECTANGLE *rbot, **prev;
	int obscured;

	obscured = 0;

	prev = rects;

	/* Go through the bottom rectangles and see which are obscured */

	while (rbot = *prev) {
	    if (rast->left >= rbot->right || rbot->left >= rast->right ||
		rast->top >= rbot->bottom || rbot->top >= rast->bottom) {
		prev = &rbot->next;
	    } else {
		prev = Do_overlap (rast, prev, rects);
		FREERECT(rbot);
		obscured = 1;
	    }
	}
	return (obscured);
}

/* Do_overlap figures out how raster front overlaps rectangle *prev and
 * replaces *prev in recs with a list of the maximally wide visible
 * rectangles.  Returns an updated list pointer that points to *prev->next.
 */

RECTANGLE **Do_overlap (front, prev, recs)
	register RASTER *front;
	register RECTANGLE **prev;
	RECTANGLE **recs;
{
	register RECTANGLE *back, *r;
	int overlap;

	back = *prev;
	*prev = back->next;
	overlap = 0;
	if (front->right < back->right)		overlap += 8;
	if (front->left > back->left) 		overlap += 4;
	if (front->bottom < back->bottom) 	overlap += 2;
	if (front->top > back->top) 		overlap += 1;

	switch (overlap) {
	    case 0:		/* Top completely covers bottom */
		goto done;

	    /* In the next four cases the bottom shows in one rectangle. */
	    case 1:			/* Back peeks out on top */
		NEWRECT(r, back->left, back->right,
			back->top, front->top, back->type);
		break;
	    case 2:			/* ...on bottom */
		NEWRECT(r, back->left, back->right,
			front->bottom, back->bottom, back->type);
		break;
	    case 4:			/* ...on left */
		NEWRECT(r, back->left, front->left,
			back->top, back->bottom, back->type);
		*prev = back;
		if TRUE(Merge_vertical (r, recs, 1))
		    goto refind;
		break;
	    case 8:			/* ...on right */
		NEWRECT(r, front->right, back->right,
			back->top, back->bottom, back->type);
		*prev = back;
		if TRUE(Merge_vertical (r, recs, 1))
		    goto refind;
		break;

	    /* Now we have the cases where there are 2 rectangles */

	    case 3:			/* ...on top & bottom */
		NEWRECT(r, back->left, back->right,
			back->top, front->top, back->type);
		*prev = r;
		prev = &r->next;
		NEWRECT(r, back->left, back->right,
			front->bottom, back->bottom, back->type);
		break;
	    case 5:		    /* ...on top & left */
		NEWRECT(r, back->left, back->right,
			back->top, front->top, back->type);
		r->next = *prev;
		*prev = r;
		prev = &r->next;
		NEWRECT(r, back->left, front->left,
			front->top, back->bottom, back->type);
		if TRUE(Merge_vertical (r, recs, 0))
		    goto done;
		break;
	    case 6:		    /* ...on bottom & left */
		NEWRECT(r, back->left, back->right,
			front->bottom, back->bottom, back->type);
		r->next = *prev;
		*prev = r;
		prev = &r->next;
		NEWRECT(r, back->left, front->left,
			back->top, front->bottom, back->type);
		if TRUE(Merge_vertical (r, recs, 0))
		    goto done;
		break;
	    case 9:		    /* ...on top & right */
		NEWRECT(r, back->left, back->right,
			back->top, front->top, back->type);
		r->next = *prev;
		*prev = r;
		prev = &r->next;
		NEWRECT(r, front->right, back->right,
			front->top, back->bottom, back->type);
		if TRUE(Merge_vertical (r, recs, 0))
		    goto done;
		break;
	    case 10:		    /* ...on bottom & right */
		NEWRECT(r, back->left, back->right,
			front->bottom, back->bottom, back->type);
		r->next = *prev;
		*prev = r;
		prev = &r->next;
		NEWRECT(r, front->right, back->right,
			back->top, front->bottom, back->type);
		if TRUE(Merge_vertical (r, recs, 0))
		    goto done;
		break;
	    case 12:		    /* ...on left & right */
		NEWRECT(r, back->left, front->left,
			back->top, back->bottom, back->type);
		*prev = back;
		if TRUE(Merge_vertical (r, recs, 1)) {
		    /* refind insertion point */
		    prev = recs;
		    while ((r = *prev) != back)
			prev = &r->next;
		    NEWRECT(r, front->right, back->right,
			    back->top, back->bottom, back->type);
		    if TRUE(Merge_vertical (r, recs, 1))
			goto refind;
		} else {
		    r->next = back->next;
		    *prev = r;
		    prev = &r->next;
		    NEWRECT(r, front->right, back->right,
			    back->top, back->bottom, back->type);
		    if TRUE(Merge_vertical (r, recs, 1))
			goto done;
		}
		break;

	    /* Now the cases where there are 3 rectangles */

	    case 7:		    /* ...top, bottom, & left */
		NEWRECT(r, back->left, front->left,
			front->top, front->bottom, back->type);
		*prev = r;
		prev = &r->next;
		NEWRECT(r, back->left, back->right,
			back->top, front->top, back->type);
		*prev = r;
		prev = &r->next;
		NEWRECT(r, back->left, back->right,
			front->bottom, back->bottom, back->type);
		break;
	    case 11:		    /* ...top, bottom, & right */
		NEWRECT(r, front->right, back->right,
			front->top, front->bottom, back->type);
		*prev = r;
		prev = &r->next;
		NEWRECT(r, back->left, back->right,
			back->top, front->top, back->type);
		*prev = r;
		prev = &r->next;
		NEWRECT(r, back->left, back->right,
			front->bottom, back->bottom, back->type);
		break;
	    case 13:		/* ...top, left, & right */
		NEWRECT(r, back->left, back->right,
			back->top, front->top, back->type);
		r->next = *prev;
		*prev = r;
		prev = &r->next;
		NEWRECT(r, back->left, front->left,
			front->top, back->bottom, back->type);
		if FALSE(Merge_vertical (r, recs, 0)) {
		    r->next = *prev;
		    *prev = r;
		    prev = &r->next;
		}
		NEWRECT(r, front->right, back->right,
			front->top, back->bottom, back->type);
		if TRUE(Merge_vertical (r, recs, 0))
		    goto done;
		break;
	    case 14:		/* ...left, right, & bottom */
		NEWRECT(r, back->left, back->right,
			front->bottom, back->bottom, back->type);
		r->next = *prev;
		*prev = r;
		prev = &r->next;
		NEWRECT(r, back->left, front->left,
			back->top, front->bottom, back->type);
		if FALSE(Merge_vertical (r, recs, 0)) {
		    r->next = *prev;
		    *prev = r;
		    prev = &r->next;
		}
		NEWRECT(r, front->right, back->right,
			back->top, front->bottom, back->type);
		if TRUE(Merge_vertical (r, recs, 0))
		    goto done;
		break;

	    /* And the case where there are 4 rectangles */

	    case 15:		/* ...left, right, top, & bottom */
		NEWRECT(r, back->left, back->right,
			back->top, front->top, back->type);
		*prev = r;
		prev = &r->next;
		NEWRECT(r, back->left, front->left,
			front->top, front->bottom, back->type);
		*prev = r;
		prev = &r->next;
		NEWRECT(r, front->right, back->right,
			front->top, front->bottom, back->type);
		*prev = r;
		prev = &r->next;
		NEWRECT(r, back->left, back->right,
			front->bottom, back->bottom, back->type);
		break;
	}
	r->next = back->next;
	*prev = r;
	return (&r->next);
refind:	/* refind insertion point */
	prev = recs;
	while ((r = *prev) != back)
	    prev = &r->next;
	*prev = back->next;
done:	return (prev);
}

/* Clip_raster modifies raster1 so that it all fits in raster2.
 */

Clip_raster (rast1, rast2)
	register RASTER *rast1, *rast2;
{
	if (rast1->right > rast2->right)
	    rast1->right = rast2->right;
	if (rast1->left < rast2->left)
	    rast1->left = rast2->left;
	if (rast1->bottom > rast2->bottom)
	    rast1->bottom = rast2->bottom;
	if (rast1->top < rast2->top)
	    rast1->top = rast2->top;
	if (rast1->left >= rast1->right ||
	    rast1->top >= rast1->bottom) {
	    rast1->right = rast1->left;
	    rast1->bottom = rast1->top;
	}
}

/* Rec_intersection returns a rectangle which is the intersection of its
 * 2 arguments.  If they don't interesect, it returns NULL.
 */

RECTANGLE *Rec_intersection (rec1, rec2)
	register RECTANGLE *rec1, *rec2;
{
	register int l, r, t, b;

	l = max(rec1->left, rec2->left);
	r = min(rec1->right, rec2->right);
	t = max(rec1->top, rec2->top);
	b = min(rec1->bottom, rec2->bottom);
	if (l >= r || t >= b)
	    return (NULL);
	NEWRECT(rec1, l, r, t, b, new_rec);
	return (rec1);
}

/* Clips a rectangle to fit in a raster.  Returns 0 if part of the rectangle
 * is inside, else frees the rectangle and returns 1.
 */

int Clip_rectangle (rect, rast)
	register RECTANGLE *rect;
	register RASTER *rast;
{
	if (rect->left < rast->left)
	    rect->left = rast->left;
	if (rect->right > rast->right)
	    rect->right = rast->right;
	if (rect->top < rast->top)
	    rect->top = rast->top;
	if (rect->bottom > rast->bottom)
	    rect->bottom = rast->bottom;

	if (rect->left < rect->right &&
	    rect->top < rect->bottom)
	    return (0);
	FREERECT(rect);
	return (1);
}

/* Merge_vertical tries to find a rectangle in the list that abuts above or
 * below with rec, and merges rec into it.  If bothsides is non-zero, the
 * merge might allow a further merge with an existing rectangle in the list,
 * and if so, the earlier rectangle is merged into the later rectangle in
 * the list.  Returns 1 if a merge was made, else 0.
 */

Merge_vertical (rec, list, bothsides)
	register RECTANGLE *rec, **list;
	int bothsides;
{
	register RECTANGLE *r;

	r = *list;
	while (1) {
	    if (r->type == rec->type &&
		r->left == rec->left && r->right == rec->right) {
		if (r->top == rec->bottom) {
		    r->top = rec->top;
		    FREERECT(rec);
		    if FALSE(bothsides)
			return (1);
		    break;
		} else if (r->bottom == rec->top) {
		    r->bottom = rec->bottom;
		    FREERECT(rec);
		    if FALSE(bothsides)
			return (1);
		    break;
		}
	    }
	    if ((r = r->next) == NULL)
		return (0);
	}
	rec = r;
	while (r = r->next) {
	    if (r->type == rec->type &&
		r->left == rec->left && r->right == rec->right) {
		if (r->top == rec->bottom)
		    r->top = rec->top;
		else if (r->bottom == rec->top)
		    r->bottom = rec->bottom;
		else
		    continue;
		while ((r = *list) != rec)
		    list = &r->next;
		*list = rec->next;
		FREERECT(rec);
		return (1);
	    }
	}
	return (1);
}

/* Merge_rectangles merges one list of rectangles into another.  It may also
 * resize rectangles to get wide rather than tall rectangles.  It makes
 * multiple passes since doing some merges may enable others to be made.
 * Since the lists might be long, you would think sorting would help, but
 * sorting actually seems to run slower.
 */

Merge_rectangles (r1, recs)
	register RECTANGLE *r1, **recs;
{
	register RECTANGLE *r2, **prev;

	while (1) {
	    prev = recs;
	    while (r2 = *prev) {
		if (r1->type != r2->type) {
		    prev = &r2->next;
		    continue;
		}
		if (r1->top == r2->top) {
		    if (r1->right == r2->left) {
			if (r1->bottom == r2->bottom) {
			    r1->right = r2->right;
			    goto free;
			} else if (r1->bottom < r2->bottom) {
			    r1->right = r2->right;
			    r2->top = r1->bottom;
			    goto remerge;
			} else {
			    r1->top = r2->bottom;
			    r2->left = r1->left;
			    goto remerge;
			}
		    } else if (r1->left == r2->right) {
			if (r1->bottom == r2->bottom) {
			    r1->left = r2->left;
			    goto free;
			} else if (r1->bottom < r2->bottom) {
			    r1->left = r2->left;
			    r2->top = r1->bottom;
			    goto remerge;
			} else {
			    r1->top = r2->bottom;
			    r2->right = r1->right;
			    goto remerge;
			}
		    }
		} else if (r1->bottom == r2->bottom) {
		    if (r1->right == r2->left) {
			if (r1->top < r2->top) {
			    r2->left = r1->left;
			    r1->bottom = r2->top;
			    goto remerge;
			} else {
			    r1->right = r2->right;
			    r2->bottom = r1->top;
			    goto remerge;
			}
		    } else if (r1->left == r2->right) {
			if (r1->top < r2->top) {
			    r2->right = r1->right;
			    r1->bottom = r2->top;
			    goto remerge;
			} else {
			    r1->left = r2->left;
			    r2->bottom = r1->top;
			    goto remerge;
			}
		    }
		} else if (r1->left == r2->left && r1->right == r2->right) {
		    if (r1->bottom == r2->top) {
			r1->bottom = r2->bottom;
			goto free;
		    } else if (r1->top == r2->bottom) {
			r1->top = r2->top;
			goto free;
		    }
		}
		prev = &r2->next;
		continue;

free:		*prev = r2->next;
		FREERECT(r2);
		prev = recs;
		continue;

remerge:	*prev = r2->next;
		r2->next = r1;
		r1 = r2;
		prev = recs;
	    }
	    *prev = r1;
	    if ((r2 = r1->next) == NULL)
		return;
	    r1->next = NULL;
	    r1 = r2;
	}
}
