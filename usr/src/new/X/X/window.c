#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/* Routines for maintaining windows:
 *
 *	Create_root_window, Create_window, Destroy_window,
 *	Map_root_window, Map_window, Map_subwindows,
 *	Unmap_window, Unmap_subwindows, Remove_subwindows,
 *	Move_window, Change_window, Change_background, Change_border,
 *	Raise_window, Lower_window, Circulate_window_up, Circulate_window_down,
 *	Draw_window, Windex, Free_window_storage
 */
#ifndef lint
static char *rcsid_window_c = "$Header: window.c,v 10.10 86/11/06 19:14:50 jg Rel $";
#endif

#include "Xint.h"

extern u_char Xstatus;
extern RESOURCE **Resources;
extern PIXMAP *roottile;
extern RECTANGLE *free_rectangles;
extern int mouse_grabber;
extern WINDOW *button_window, *mouse_grab_window, *key_window;
extern RASTER mbox;

char *Xalloc();
long Add_resource();
RECTANGLE *Alloc_rectangle();
WINDOW *Unmap_window();

#define MBOX(w) if (w->vs.top < mbox.bottom && mbox.top < w->vs.bottom &&\
		    w->vs.left < mbox.right && mbox.left < w->vs.right)\
		    mbox.bottom = 0

#define alloc_at_once 25

WINDOW *rootwindow = NULL;	/* The background of the whole tree */
static WINDOW *mapped_list = NULL; /* List of windows mapped to the screeen */
WINDOW *free_windows = NULL;

/* Create_root_window creates and maps the root window on the screen.
 * The root window differs from other windows in that it has no parent.
 */

Create_root_window (height, width, tile)
	int height, width;
	PIXMAP *tile;
{
	int root;

	/* make sure no other windows are in the root's malloc'd area */
	free_windows = (WINDOW *) Xalloc (sizeof (WINDOW));
	free_windows->next = NULL;
	root = Create_window (height, width, 0, 0, 0, (PIXMAP *) NULL,
			      tile, IsOpaque, (WINDOW *) NULL, 0);
	rootwindow = (WINDOW *) Resources[RESIDX(root)]->value;
	rootwindow->clipmode = ClipModeDrawThru;
	Map_root_window ();
}

/* Create_window creates a new window with common parameters filled in.
 * The window returned is unmapped.
 */

int Create_window (height, width, x, y, bwidth, border, tile, kind,
			parent, client)
	int height, width, x, y, bwidth, kind, client;
	register WINDOW *parent;
	PIXMAP *border, *tile;
{
	register WINDOW *w;
	int i;

	if (height <= 0 || width <= 0 || bwidth < 0) {
	    Xstatus = BadValue;
	    return (NULL);
	} else if (kind != IsTransparent) {
	    if (parent && parent->kind == IsTransparent) {
		Xstatus = BadMatch;
		return (NULL);
	    } else if (bwidth > 0 && border == NULL) {
		Xstatus = BadPixmap;
		return (NULL);
	    } else if ((border && FALSE(border->tile)) ||
		       (tile && FALSE(tile->tile))) {
		Xstatus = BadTile;
		return (NULL);
	    } else if (tile == NULL) {
		tile = parent->tile;
	    }
	}

	if ((w = free_windows) == NULL) {
	    /* We allocate in chunks to minimize fragmentation. */
	    w = (WINDOW *) Xalloc (alloc_at_once * sizeof (WINDOW));
	    free_windows = w;
	    w->internal = 0;
	    i = alloc_at_once;
	    while (--i > 0) {
		w->next = w + 1;
		w++;
		w->internal = 1;
	    }
	    w->next = NULL;
	    w = free_windows;
	}
	free_windows = w->next;

	w->full.left = x + bwidth;
	w->full.top = y + bwidth;
	w->full.right = w->full.left + width;
	w->full.bottom = w->full.top + height;
	w->bwidth = bwidth;
	if (w->border = border)
	    border->refcnt++;
	if (w->tile = tile)
	    tile->refcnt++;
	if (kind == IsTransparent)
	    w->tilemode = TileModeRelative;
	else
	    w->tilemode = TileModeAbsolute;
	w->clipmode = ClipModeClipped;

	w->cursor = NULL;
	w->mask = NoEvent;
	w->client = 0;

	if (parent)
	    w->level = parent->level + 1;
	else
	    w->level = 0;
	w->kind = kind;
	w->first_child = w->last_child = NULL;
	w->parent = parent;
	w->visible = NULL;
	w->cmvisible = NULL;
	w->unobscured = OB_NOT;
	w->mapped = w->should_be_mapped = 0;
	w->bgrabs = 0;
	w->name = NULL;
	w->width0 = w->height0 = 0;
	w->widthinc = w->heightinc = 1;
	w->icon = NULL;

	if (parent) {
	    /* make it the top-most child */
	    if (parent->last_child) {
		parent->last_child->next_sib = w;
		w->prev_sib = parent->last_child;
		w->next_sib = NULL;
		parent->last_child = w;
	    } else {
		parent->first_child = parent->last_child = w;
		w->next_sib = w->prev_sib = NULL;
	    }
	} else {
	    w->next_sib = w->prev_sib = NULL;
	}

	return (w->rid = Add_resource (RT_WINDOW, client, (caddr_t) w));
}

/* Eliminate a window. */

Destroy_window (w)
	register WINDOW *w;
{
	register WINDOW *icon;

	/* Check icon status */

	if (icon = w->icon) {
	    if (w->kind == IsIcon && TRUE(w->mapped) && FALSE(icon->mapped))
		Map_window (icon);
	    else if (icon->kind == IsIcon && TRUE(icon->mapped)) {
		Unmap_window (icon, 1);
		Stash_simple (icon, (long) UnmapWindow, 0);
	    }
	    icon->kind = IsOpaque;
	    icon->icon = NULL;
	}

	/* Unmap it */

	if TRUE(w->mapped)
	    Unmap_window (w, 1);

	/* Destroy all the children */

	if (w->first_child)
	    Zap_subwindows (w, 1);

	/* Remove from parent's list */

	if (w->next_sib)
	    w->next_sib->prev_sib = w->prev_sib;
	else
	    w->parent->last_child = w->prev_sib;

	if (w->prev_sib)
	    w->prev_sib->next_sib = w->next_sib;
	else
	    w->parent->first_child = w->next_sib;

	/* Get it out of any binds */

	if (w == button_window || (mouse_grabber && w == mouse_grab_window))
	    Stash_ungrabs ();
	if (w->bgrabs)
	    Unbutton_window (w);
	if (w == key_window) {
	    key_window = NULL;
	    Focus_keyboard (w->parent);
	}

	/* Free the resources */

	if (w->cursor)
	    Unregister_cursor (w);

	if (w->name)
	    free (w->name);

	if (w->border && --w->border->refcnt == 0)
	    FreePixmap (w->border);
	if (w->tile && --w->tile->refcnt == 0)
	    FreePixmap (w->tile);

	w->next = free_windows;
	free_windows = w;
}

/* Maps the root window */

Map_root_window ()
{
	register WINDOW *w = rootwindow;
	register RECTANGLE *r;

	w->vs = w->full;
	w->ovs = w->full;
	w->clip.left = w->vs.left;
	w->clip.top = w->vs.top;
	w->clip.height = w->vs.bottom - w->vs.top;
	w->clip.width = w->vs.right - w->vs.left;
	RASTRECT(r, w->full, contents_rec);
	r->next = NULL;
	w->visible = r;
	RASTRECT(r, w->full, contents_rec);
	r->next = NULL;
	w->cmvisible = r;

	w->next = NULL;
	mapped_list = w;
	w->prev = NULL;

	w->mapped = w->should_be_mapped = 1;
	w->unobscured = OB_YES;

	Do_background (w, 1);
}

/* Map_window raises and displays the specified window on the screen.  It also
 * goes through the window's subwindows and maps all those for whom the
 * should_be_mapped flag is set.
 */

Map_window (w)
	register WINDOW *w;
{
	if FALSE(w->parent->mapped) {
	    w->should_be_mapped = 1;
	    return;
	}

	Map_window_main (w, 1, 0);
	MBOX(w);

	if (w->first_child)
	    Map_subwindows (w, 0, 0);

	Draw_window (w, 0, 1);
}

/* Does the common work of mapping a window.
 * If top is set, the window is top-most, else it is bottom-most.
 * If dontMove is set, this window is already placed in the parent's list.
 */

Map_window_main (w, top, dontMove)
	register WINDOW *w;
	int top, dontMove;
{
	register WINDOW *w1 = w->parent;

	w->mapped = w->should_be_mapped = 1;

	/* get absolute coordinates */
	w->full.left += w1->full.left;
	w->full.top += w1->full.top;
	w->full.right += w1->full.left;
	w->full.bottom += w1->full.top;

	/* clip to parent */
	w->vs = w->full;
	Clip_raster (&w->vs, &w1->vs);

	w->ovs.left = w->full.left - w->bwidth;
	w->ovs.right = w->full.right + w->bwidth;
	w->ovs.top  = w->full.top - w->bwidth;
	w->ovs.bottom = w->full.bottom + w->bwidth;
	Clip_raster (&w->ovs, &w1->vs);
	if (w->ovs.right <= w->ovs.left)
	    w->ovs.right = w->ovs.bottom = -1;

	w->clip.left = w->vs.left;
	w->clip.top = w->vs.top;
	w->clip.height = w->vs.bottom - w->vs.top;
	w->clip.width = w->vs.right - w->vs.left;

	if TRUE(top) {
	    /* Search the mapped_list backwards starting at w's parent until we
	     * either find a window with a lower level or we find the beginning
	     * of the list.  Insert after it.
	     */

	    do {
		w1 = w1->prev;
	    } while (w1 && w1->level >= w->level);

	    if (w1) {
		w->prev = w1;
		w->next = w1->next;
		w->next->prev = w;
		w1->next = w;
	    } else {
		w->next = mapped_list;
		mapped_list->prev = w;
		mapped_list = w;
		w->prev = NULL;
	    }

	    /* Move w to be the last child of its parent */
	    if (w->next_sib && FALSE(dontMove))
		Make_top_child (w);

	    /* Get its rectangles and obscure other windows */
	    if (w->kind != IsTransparent)
		Obscure_top (w);
	} else {
	    /* Place w just before its parent in the mapped_list */

	    w->next = w1;
	    if (w->prev = w1->prev)
		w->prev->next = w;
	    else
		mapped_list = w;
	    w1->prev = w;

	    /* Move w to be the first child of its parent */
	    if (w->prev_sib && FALSE(dontMove))
		Make_bottom_child (w);

	    /* Get its rectangles and obscure other windows */
	    if (w->kind != IsTransparent)
		Obscure_bottom (w);
	}
}

/* Maps the subwindows of a window.
 * If dontRedraw is set, the subwindows have already been displayed.
 * If all is set, all subwindows are mapped, not just the should_be_mapped.
 */

Map_subwindows (w, dontRedraw, all)
	WINDOW *w;
	int dontRedraw, all;
{
	register WINDOW *w1;

	if TRUE(w->mapped) {
	    /* mapping from top to bottom is the most efficient */
	    for (w1 = w->last_child; w1; w1 = w1->prev_sib) {
		if (FALSE(w1->mapped) &&
		    (TRUE(all) || TRUE(w1->should_be_mapped))) {
		    Map_window_main (w1, 0, 1);
		    if TRUE(all) {
			MBOX(w1);
		    }
		    if (w1->first_child)
			Map_subwindows (w1, dontRedraw, 0);
		    if FALSE(dontRedraw)
			Draw_window (w1, 0, 1);
		}
	    }
	} else if TRUE(all) {
	    for (w1 = w->first_child; w1; w1 = w1->next_sib)
		w1->should_be_mapped = 1;
	}
}

/* Unmap_window removes the window from the list of mapped windows and updates
 * all the windows that it may have been obscuring.  All the subwindows are
 * unmapped but their should_be_mapped flags are unchanged.  If cleanup = 1,
 * then update windows you uncover.  If cleanup < 0, then update the
 * windows you uncover without changing the bits on the screen.
 * Returns the next mapped window in the mapped_list chain.
 */

WINDOW *Unmap_window (w, cleanup)
	register WINDOW *w;
	int cleanup;
{
	WINDOW *w1;

	if (w->first_child) {
	    Zap_subwindows (w, 0);
	    Restore_rectangles (w);
	}
	MBOX(w);
	w1 = w->next;
	Unmap_window_main (w, 0, cleanup);
	return (w1);
}

/* Restore the visible rectangles as if there were no subwindows, i.e.,
 * make it a copy of cmvisible plus existing borders.
 */

Restore_rectangles (w)
	WINDOW *w;
{
	register RECTANGLE *vr, *cr, **prev;

	cr = w->cmvisible;
	prev = &w->visible;
	/* reuse what we can */
	while (vr = *prev) {
	    if (vr->type != border_rec) {
		if (cr == NULL) {
		    *prev = vr->next;
		    FREERECT(vr);
		    continue;
		}
		*(RASTER *) vr = *(RASTER *) cr;
		vr->type = contents_rec;
		cr = cr->next;
	    }
	    prev = &vr->next;
	}
	/* allocate the rest */
	for (; cr; cr = cr->next) {
	    RASTRECT(vr, *(RASTER *) cr, contents_rec);
	    *prev = vr;
	    prev = &vr->next;
	}
	*prev = NULL;
}

/* Do the common work of unmapping a window.
 * Set should_be_mapped to the indicated state.  If cleanup = 1, then update
 * windows you uncover.  If cleanup < 0, then update the windows you uncover
 * without changing the bits on the screen.
 */

Unmap_window_main (w, should_be_mapped, cleanup)
	register WINDOW *w;
	int should_be_mapped, cleanup;
{
	register WINDOW *w1;

	/* Remove it from the list */

	w1 = w->next;
	if (w1->prev = w->prev)
	    w->prev->next = w1;
	else
	    mapped_list = w1;

	/* Get rid of the rectangles */
	
	if (w->visible) {
	    if (w->cmvisible) {
		Free_rectangles (w->cmvisible);
		w->cmvisible = NULL;
	    }
	    Remove_rectangles (w, w1, cleanup);
	}

	w->next = w->prev = NULL;

	/* make coordinates relative */
	w1 = w->parent;
	w->full.left -= w1->full.left;
	w->full.top -= w1->full.top;
	w->full.right -= w1->full.left;
	w->full.bottom -= w1->full.top;

	w->should_be_mapped = should_be_mapped;
	w->mapped = 0;
	w->unobscured = OB_NOT;
}

/* Unmap all the subwindows of a window. */

Unmap_subwindows (w)
	WINDOW *w;
{
	register WINDOW *w1;

	for (w1 = w->first_child; w1; w1 = w1->next_sib) {
	    if TRUE(w1->mapped)
		Stash_simple (w1, (long) UnmapWindow, 0);
	}
	Remove_subwindows (w, 0, 1);
}

/* Unmap all subwindows, giving space back to parent.  Destroy the subwindows
 * if destroy flag is set.  Display the window if cleanup is set.
 */

Remove_subwindows (w, destroy, cleanup)
	WINDOW *w;
	int destroy;
{
	register WINDOW *w1;
	register RECTANGLE *rec, *rlist, **prev;
	int vn, cn;

	/* count visible rectangles */
	vn = 0;
	for (prev = &w->visible; rec = *prev; prev = &rec->next) {
	    if (rec->type != border_rec)
		vn++;
	}
	/* collect new visible rectangles and unmap subwindows */
	cn = 0;
	rlist = NULL;
	for (w1 = w->first_child; w1; w1 = w1->next_sib) {
	    if TRUE(w1->mapped) {
		if (rec = w1->cmvisible) {
		    while (1) {
			rec->type = new_rec;
			cn++;
			if (rec->next == NULL)
			    break;
			rec = rec->next;
		    }
		    rec->next = rlist;
		    rlist = w1->cmvisible;
		    w1->cmvisible = NULL;
		}
		while (rec = w1->visible) {
		    w1->visible = rec->next;
		    if (rec->type == border_rec) {
			rec->type = new_rec;
			rec->next = rlist;
			rlist = rec;
			cn++;
		    } else {
			FREERECT(rec);
		    }
		}
		if TRUE(cleanup) {
		    MBOX(w1);
		}
		if (w1->first_child)
		    Zap_subwindows (w1, destroy);
		Unmap_window_main (w1, cleanup ^ 1, 0);
	    } else if (TRUE(destroy) && w1->first_child)
		Zap_subwindows (w1, 1);
	    else
		w1->should_be_mapped = 0;
	}
	if TRUE(destroy) {
	    while (w1 = w->first_child)
		Free_resource (Resources[RESIDX(w1->rid)]);
	}
	if (cn == 0)
	    return;
	else if (vn == 0 && cn > 2) {
	    Free_rectangles (rlist);
	    for (rlist = w->cmvisible; rlist; rlist = rlist->next) {
		RASTRECT(rec, *(RASTER *) rlist, new_rec);
		*prev = rec;
		prev = &rec->next;
	    }
	    *prev = NULL;
	} else if (w->cmvisible->next == NULL && (cn >> 3) > vn) {
	    Free_rectangles (rlist);
	    RASTRECT(rec, *(RASTER *) w->cmvisible, new_rec);
	    *prev = rec;
	    rec->next = NULL;
	    for (rec = w->visible; ; rec = rec->next) {
		if (rec->type != border_rec)
		    Calc_overlaps ((RASTER *) rec, prev);
		if (prev == &rec->next)
		    break;
	    }
	} else
	    Merge_rectangles (rlist, prev);
	if TRUE(cleanup) {
	    Do_background (w, 0);
	    Stash_changes (w, 0);
	}
}

/* Unmap all subwindows, discarding their rectangles.  Destroy the subwindows
 * if destroy flag is set.
 */

Zap_subwindows (w, destroy)
	register WINDOW *w;
	int destroy;
{
	register WINDOW *w1;

	for (w1 = w->first_child; w1; w1 = w1->next_sib) {
	    if TRUE(w1->mapped) {
		if (w1->visible) {
		    Free_rectangles (w1->visible);
		    w1->visible = NULL;
		}
		if (w1->cmvisible) {
		    Free_rectangles (w1->cmvisible);
		    w1->cmvisible = NULL;
		}
		if (w1->first_child)
		    Zap_subwindows (w1, destroy);
		Unmap_window_main (w1, 1, 0);
	    }
	}
	if TRUE(destroy) {
	    while (w1 = w->first_child)
		Free_resource (Resources[RESIDX(w1->rid)]);
	}
}

/* Change the x-y coordinates of a window */

Move_window (w, new_x, new_y)
	register WINDOW *w;
	int new_x, new_y;
{
	short deltax, deltay;
	register WINDOW *w1 = NULL;
	register RECTANGLE *r;
	register WINDOW *head;
	register RECTANGLE *cr;
	RECTANGLE *vlist;
	WINDOW *tail;
	int unobscured = OB_NOT;
	int save_unobscured = w->unobscured;

	deltax = new_x - (w->full.left - w->bwidth);
	deltay = new_y - (w->full.top - w->bwidth);
	if TRUE(w->mapped) {
	    MBOX(w);
	    deltax += w->parent->full.left;
	    deltay += w->parent->full.top;
	    /* Check if all visible */
	    if (FALSE(w->tilemode) &&
		(r = w->cmvisible) && r->next == NULL &&
		r->left == w->full.left && r->top == w->full.top &&
		r->right == w->full.right && r->bottom == w->full.bottom)
		unobscured = OB_YES;
	    if (w->first_child) {
		if TRUE(unobscured) {
		    /* Find earliest mapped subwindow */
		    w1 = w;
		    do {
			head = w1;
			for (w1 = head->last_child;
			     w1 && FALSE(w1->mapped);
			     w1 = w1->prev_sib) ;
		    } while (w1);
		    if (head != w) {
			tail = w->prev;
			/* Remove all subwindows from list */
			if (w->prev = head->prev)
			    head->prev->next = w;
			else
			    mapped_list = w;
			/* Extract visible insides */
			vlist = NULL;
			cr = w->visible;
			w->visible = NULL;
			while (r = cr) {
			    cr = r->next;
			    if (r->type == border_rec) {
				r->next = w->visible;
				w->visible = r;
			    } else {
				r->left += deltax;
				r->right += deltax;
				r->top += deltay;
				r->bottom += deltay;
				r->next = vlist;
				vlist = r;
			    }
			}
			Restore_rectangles (w);
		    }
		} else {
		    Zap_subwindows (w, 0);
		    Restore_rectangles (w);
		}
	    }
	    w1 = w->next;
	    Unmap_window_main (w, 1, 0);
	}

	w->full.left += deltax;
	w->full.right += deltax;
	w->full.top += deltay;
	w->full.bottom += deltay;

	if (FALSE(w->should_be_mapped) || FALSE(w->parent->mapped))
	    return;
	Map_window_main (w, 1, 0);
	MBOX(w);
	if (w->first_child) {
	    if FALSE(unobscured)
		Map_subwindows (w, 0, 0);
	    else if (head != w) {
		/* Reinsert subwindows */
		if (head->prev = w->prev)
		    w->prev->next = head;
		else
		    mapped_list = head;
		tail->next = w;
		w->prev = tail;
		/* Check if still all visible */
		if ((r = w->cmvisible) && r->next == NULL &&
		    r->left == w->full.left && r->top == w->full.top &&
		    r->right == w->full.right && r->bottom == w->full.bottom) {
		    unobscured = OB_TMP;
		    /* Move all subwindows */
		    do {
			tail = head->next;
			head->full.left += deltax;
			head->full.right += deltax;
			head->full.top += deltay;
			head->full.bottom += deltay;
			head->vs.left += deltax;
			head->vs.right += deltax;
			head->vs.top += deltay;
			head->vs.bottom += deltay;
			head->ovs.left += deltax;
			head->ovs.right += deltax;
			head->ovs.top += deltay;
			head->ovs.bottom += deltay;
			head->clip.left += deltax;
			head->clip.top += deltay;
			for (r = head->visible; r; r = r->next) {
			    r->left += deltax;
			    r->right += deltax;
			    r->top += deltay;
			    r->bottom += deltay;
			}
			for (r = head->cmvisible; r; r = r->next) {
			    r->left += deltax;
			    r->right += deltax;
			    r->top += deltay;
			    r->bottom += deltay;
			}
		    } while ((head = tail) != w);
		    cr = w->visible;
		    while (r = cr) {
			cr = r->next;
			if (r->type == border_rec) {
			    r->next = vlist;
			    vlist = r;
			} else {
			    FREERECT(r);
			}
		    }
		    w->visible = vlist;
		} else {
		    /* Unmap all subwindows and move children */
		    do {
			tail = head->next;
			if (head->visible) {
			    Free_rectangles (head->visible);
			    head->visible = NULL;
			}
			if (head->cmvisible) {
			    Free_rectangles (head->cmvisible);
			    head->cmvisible = NULL;
			}
			Unmap_window_main (head, 1, 0);
			if (head->parent == w) {
			    head->full.left += deltax;
			    head->full.right += deltax;
			    head->full.top += deltay;
			    head->full.bottom += deltay;
			}
		    } while ((head = tail) != w);
		    if (vlist)
			Free_rectangles (vlist);
		    /* Remap all subwindows */
		    Map_subwindows (w, 1, 0);
		}
	    }
	}
	if TRUE(unobscured)
	    Do_refill (w, deltax, deltay);
	else
	    Draw_window (w, 0, 1);

	if (unobscured == OB_TMP)
	    w->unobscured = save_unobscured;

	for (; w1; w1 = w1->next) {
	    if (w1->unobscured == OB_TMP) {
		w1->unobscured = OB_NOT;
		Draw_window (w1, 0, 0);
	    }
	}
}

/* Change the size and position of a window. */

Change_window (w, x, y, height, width)
	register WINDOW *w;
	int x, y, height, width;
{
	register WINDOW *w1 = NULL;

	if (height <= 0 || width <= 0) {
	    Xstatus = BadValue;
	    return;
	}

	if TRUE(w->mapped) {
	    w1 = Unmap_window (w, 0);
	    w->should_be_mapped = 1;
	}

	x -= w->full.left - w->bwidth;
	y -= w->full.top - w->bwidth;

	w->full.left += x;
	w->full.top += y;
	w->full.right = w->full.left + width;
	w->full.bottom = w->full.top + height;

	if (FALSE(w->should_be_mapped) || FALSE(w->parent->mapped))
	    return;
	Map_window (w);
	for (; w1; w1 = w1->next) {
	    if (w1->unobscured == OB_TMP) {
		w1->unobscured = OB_NOT;
		Draw_window (w1, 0, 0);
	    }
	}
}

/* Change the background tile of a window */

Change_background (w, tile)
	register WINDOW *w;
	register PIXMAP *tile;
{
	if (w->kind == IsTransparent) {
	    Xstatus = BadMatch;
	    return;
	} else if (tile == NULL) {
	    if (w->parent)
		tile = w->parent->tile;
	    else
		tile = roottile;
	} else if FALSE(tile->tile) {
	    Xstatus = BadTile;
	    return;
	}
	if (tile != w->tile) {
	    tile->refcnt++;
	    if (--w->tile->refcnt == 0)
		FreePixmap (w->tile);
	    w->tile = tile;
	}
}

/* Change the border tile of a window. */

Change_border (w, tile)
	register WINDOW *w;
	register PIXMAP *tile;
{
	if (w->bwidth == 0) {
	    Xstatus = BadMatch;
	    return;
	} else if FALSE(tile->tile) {
	    Xstatus = BadTile;
	    return;
	}
	if (tile != w->border) {
	    tile->refcnt++;
	    if (--w->border->refcnt == 0)
		FreePixmap (w->border);
	    w->border = tile;
	    Do_border (w);
	}
}

/* Move a window to the top of (depthwise) the screen. */

Raise_window (w)
	register WINDOW *w;
{
	register WINDOW *ww, *nw;
	int changed;

	/* Check if we're already on top. */

	if (w->next_sib == NULL) return;

	/* If w is unmapped, simply put it on top. */

	if FALSE(w->mapped) {
	    Make_top_child (w);
	    return;
	}

	MBOX(w);

	/* If w is transparent, simply put it on top. */

	if (w->kind == IsTransparent) {
	    Make_top_child (w);
	    return;
	}

	changed = 0;

	/* Lower any of w's siblings that are above it. */

	while (ww = w->next_sib) {
	    /* move it over us */
	    if (w->next_sib = ww->next_sib)
		ww->next_sib->prev_sib = w;
	    else
		w->parent->last_child = w;
	    if (ww->prev_sib = w->prev_sib)
		w->prev_sib->next_sib = ww;
	    else
		w->parent->first_child = ww;
	    w->prev_sib = ww;
	    ww->next_sib = w;

	    /* If ww is unmapped, we're done. */
	    if FALSE(ww->mapped) continue;

	    /* If ww is transparent or doesn't overlap, just reposition */
	    if (ww->kind == IsTransparent ||
		ww->ovs.left >= w->ovs.right || w->ovs.left >= ww->ovs.right ||
		ww->ovs.top >= w->ovs.bottom || w->ovs.top >= ww->ovs.bottom) {
		/* Find earliest mapped subwindow of ww */
		do {
		    nw = ww;
		    for (ww = nw->last_child;
			 ww && FALSE(ww->mapped);
			 ww = ww->prev_sib) ;
		} while (ww);
		ww = w->prev_sib;
		/* Reposition in mapped_list just after w */
		if (ww->next->prev = nw->prev)
		    nw->prev->next = ww->next;
		else
		    mapped_list = ww->next;
		ww->next = w->next;
		w->next->prev = ww;
		w->next = nw;
		nw->prev = w;
		continue;
	    }

	    changed = 1;

	    /* Unmap all the children of ww. */
	    if (ww->first_child)
		Remove_subwindows (ww, 0, 0);

	    /* Remove ww from its current position. */
	    nw = ww->next;
	    if (nw->prev = ww->prev)
		ww->prev->next = nw;
	    else
		mapped_list = nw;

	    /* Insert ww just after w. */
	    ww->next = w->next;
	    w->next->prev = ww;
	    ww->prev = w;
	    w->next = ww;

	    /* Remove the rectangles from ww as appropriate */
	    Remove_rectangles (ww, nw, 1);

	    /* Remap the children without redrawing */
	    if (ww->first_child)
		Map_subwindows (ww, 1, 0);
	}

	if TRUE(changed)
	    Draw_window (w, 1, 0);
}

/* Move a window to the bottom of the screen. */

Lower_window (w)
	register WINDOW *w;
{
	register WINDOW *ww, *nw;

	/* Check if we're already on bottom. */
	if ((ww = w->prev_sib) == NULL) return;

	/* If w is unmapped or all higher siblings are unmapped, simply
	 * put it on the bottom.
	 */
	if (FALSE(w->mapped) || w->next == w->parent) {
	    Make_bottom_child (w);
	    return;
	}

	MBOX(w);

	/* If w is transparent, simply put it on the bottom. *
	if (w->kind == IsTransparent) {
	    Make_bottom_child (w);
	    return;
	}

	/* See if w obscures any lower opaque window. */
	while (FALSE(ww->mapped) ||
	       ww->kind == IsTransparent ||
	       ww->ovs.left >= w->ovs.right || w->ovs.left >= ww->ovs.right ||
	       ww->ovs.top >= w->ovs.bottom || w->ovs.top >= ww->ovs.bottom) {
	    if ((ww = ww->prev_sib) == NULL)
		break;
	}

	/* Move w to be the first child of its parent. */
	Make_bottom_child (w);

	if (ww == NULL) {
	    /* w doesn't obscure anything, find earliest mapped subwindow */
	    ww = w;
	    do {
		nw = ww;
		for (ww = nw->last_child; ww && FALSE(ww->mapped); ww = ww->prev_sib) ;
	    } while (ww);
	    /* Reposition in mapped_list just before parent */
	    if (w->next->prev = nw->prev)
		nw->prev->next = w->next;
	    else
		mapped_list = w->next;
	    ww = w->parent;
	    ww->prev->next = nw;
	    nw->prev = ww->prev;
	    w->next = ww;
	    ww->prev = w;
	    return;
	}

	/* w obscures something, unmap all the children. */
	if (w->first_child)
	    Remove_subwindows (w, 0, 0);

	/* Remove w from its current position. */
	if (w->next->prev = w->prev)
	    w->prev->next = w->next;
	else
	    mapped_list = w->next;

	nw = w->next;

	/* Put w just in front of its parent.  The parent cannot be
	 * right after w, because we already checked for that.
	 */

	ww = w->parent;
	w->prev = ww->prev;
	ww->prev->next = w;
	w->next = ww;
	ww->prev = w;

	/* Remove the rectangles from w as appropriate */
	Remove_rectangles (w, nw, 1);

	/* Remap the children */
	if (w->first_child)
	    Map_subwindows (w, 1, 0);
}

/* Raise the lowest mapped subwindow obscured by another subwindow. */

Circulate_window_up (w)
	register WINDOW *w;
{
	register WINDOW *ow;

	for (w = w->first_child; w; w = w->next_sib) {
	    if FALSE(w->should_be_mapped)
		continue;
	    for (ow = w->next_sib; ow; ow = ow->next_sib) {
		if (FALSE(ow->should_be_mapped) || ow->kind == IsTransparent ||
		    ow->ovs.left >= w->ovs.right ||
		    w->ovs.left >= ow->ovs.right ||
		    ow->ovs.top >= w->ovs.bottom ||
		    w->ovs.top >= ow->ovs.bottom)
		    continue;
		Raise_window (w);
		return;
	    }
	}
}

/* Lower the highest mapped subwindow obscuring another subwindow. */

Circulate_window_down (w)
	register WINDOW *w;
{
	register WINDOW *ow;

	for (w = w->last_child; w; w = w->prev_sib) {
	    if FALSE(w->should_be_mapped)
		continue;
	    for (ow = w->prev_sib; ow; ow = ow->prev_sib) {
		if (FALSE(ow->should_be_mapped) || ow->kind == IsTransparent ||
		    ow->ovs.left >= w->ovs.right ||
		    w->ovs.left >= ow->ovs.right ||
		    ow->ovs.top >= w->ovs.bottom ||
		    w->ovs.top >= ow->ovs.bottom)
		    continue;
		Lower_window (w);
		return;
	    }
	}
}

/* Make w the last sibling of its parent, making it top-most */

Make_top_child (w)
	register WINDOW *w;
{
	register WINDOW *w1 = w->parent;

	if (w->prev_sib)
	    w->prev_sib->next_sib = w->next_sib;
	else
	    w1->first_child = w->next_sib;

	w->next_sib->prev_sib = w->prev_sib;
	w1->last_child->next_sib = w;
	w->prev_sib = w1->last_child;
	w1->last_child = w;
	w->next_sib = NULL;
}

/* Make w the first sibling of its parent, making it bottom-most */

Make_bottom_child (w)
	register WINDOW *w;
{
	register WINDOW *w1 = w->parent;

	if (w->next_sib)
	    w->next_sib->prev_sib = w->prev_sib;
	else
	    w1->last_child = w->prev_sib;

	w->prev_sib->next_sib = w->next_sib;
	w1->first_child->prev_sib = w;
	w->next_sib = w1->first_child;
	w1->first_child = w;
	w->prev_sib = NULL;
}

/* Draw_window draws a window of the appropriate type.  If subflag is
   set, it also redisplays the windows subwindows.  If not_just_new
   is set, all the rectangles are redisplayed, not just the new ones.
   If not, after the new rectangles have been displayed they are
   changed to old ones and the window is Windexed. */

Draw_window (w, subflag, not_just_new)
	WINDOW *w;
	int subflag, not_just_new;
{
	register WINDOW *w1;

	if (w->kind == IsTransparent) return;

	if (w->bwidth)
	    Do_border (w);
	Do_background (w, not_just_new);

	if TRUE(subflag) {
	    for (w1 = w->first_child; w1; w1 = w1->next_sib) {
		if TRUE(w1->mapped)
		    Draw_window (w1, 1, not_just_new);
	    }
	}

	Stash_changes (w, not_just_new);
}

/* Windex recomputes if the window is obscured. */

Windex (w)
	WINDOW *w;
{
	register RECTANGLE *r1;

	for (r1 = w->visible; r1; r1 = r1->next) {
	    if (r1->type == border_rec)
		continue;
	    if (r1->type == contents_rec &&
		r1->left == w->vs.left && r1->top == w->vs.top &&
		r1->right == w->vs.right && r1->bottom == w->vs.bottom)
		w->unobscured = OB_YES;
	    break;
	}
}

/* Free all storage associated with unused windows */

Free_window_storage ()
{
	register WINDOW *w, **pw;

	/* drop the "internal" windows */
	pw = &free_windows;
	while (w = *pw) {
	    if TRUE(w->internal)
		*pw = w->next;
	    else
		pw = &w->next;
	}
	/* now free the "head" windows */
	while (w = free_windows) {
	    free_windows = w->next;
	    free ((caddr_t) w);
	}
	FREERECT(rootwindow->visible);
	FREERECT(rootwindow->cmvisible);
}
