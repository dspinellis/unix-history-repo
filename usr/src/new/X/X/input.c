#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*	Routines for dealing with input devices and events:
 *
 *	Register_cursor, Unregister_cursor, Interpret_locator,
 *	Grab_mouse, Ungrab_mouse, Grab_button, Ungrab_button, Warp_mouse,
 *	Focus_keyboard, Unbutton_window, Ungrab_client, Select_input,
 *	Set_shiftlock, Startup_mouse,
 *	Deal_with_movement, Deal_with_input,
 *	Stash_changes, Stash_misses, Stash_simple
 */

#ifndef lint
static char *rcsid_input_c = "$Header: input.c,v 10.12 86/12/17 20:25:13 swick Exp $";
#endif


#include "Xint.h"

extern u_char Xstatus;
extern DEVICE device;
extern WINDOW *rootwindow;
extern RECTANGLE *free_rectangles;
#ifdef DUALTCP
extern int swapped[];
#endif

typedef struct grab_info {
	int client;		/* Grabbing client */
	WINDOW *window;		/* Event window */
	long mask;		/* Event mask */
	CURSOR *cursor;		/* Cursor info */
} GRAB_INFO;

static short mouse_x = 0, mouse_y = 0;		/* The mouse state */
CURSOR *cursor = NULL;				/* Current cursor */
static WINDOW *cursor_window = NULL;		/* Where cursor came from */
static WINDOW *mouse_window;			/* Where mouse currently is */
RASTER mbox;					/* Mouse motion bounding box */
WINDOW *button_window = NULL;			/* Where button was pressed */
int mouse_grabber = 0;				/* Who has grabbed the mouse */
WINDOW *mouse_grab_window;			/* Window for grab events */
static long mouse_grab_mask;			/* Grab events of interest */
static unsigned mouse_grab_button;		/* Button that caused grab */
WINDOW *key_window;				/* Keyboard focus window */
static ushort key_level;			/* key_window->level */
unsigned state_mask = 0;			/* key and button state mask */
static int lock_mode = 1;			/* shiftlock mode */
static unsigned lock_mask = 0;			/* shiftlock shadow mask */
static long motion_mask = MouseMoved;		/* motion events */
#define GRABS 48
static GRAB_INFO bgrabs[GRABS];			/* button grab info */
#define GRABIDX(but,mask) ((but << 4) + FullKeyState(mask))
#define grabbits (ControlMask|MetaMask|ShiftMask|ShiftLockMask|LeftMask|MiddleMask|RightMask)

#define ShiftKeyCode 0256
#define ControlKeyCode 0257
#define LockKeyCode 0260
#define MetaKeyCode 0261

/* Search down the heirarchy for the smallest enclosing window.
 * This usually should be faster than doing a linear search of mapped_list,
 * and should be fast enough that we don't need to maintain extra, complicated
 * data structures like a layered dag.
 */

#define SEARCH(x,y,w,ww) \
	    ww = rootwindow;\
	    w = ww->last_child;\
	    while (w) {\
		if (TRUE(w->mapped) &&\
		    x >= w->vs.left && y >= w->vs.top &&\
		    x < w->vs.right && y < w->vs.bottom) {\
		    ww = w;\
		    w = ww->last_child;\
		} else\
		    w = w->prev_sib;\
	    }\
	    if (w == NULL)\
		w = ww

/* Define the mouse cursor for a window */

Register_cursor (w, curs)
	register WINDOW *w;
	register CURSOR *curs;
{
	register CURSOR *ocurs;

	ocurs = w->cursor;
	curs->refcnt++;
	w->cursor = curs;
	if (mouse_grabber == 0)
	    Check_cursor (w);
	if (ocurs && --ocurs->refcnt == 0)
	    FreeCursor (ocurs);
}

/* Undefine the mouse cursor for a window */

Unregister_cursor (w)
	register WINDOW *w;
{
	register CURSOR *curs;

	if ((curs = w->cursor) == NULL || w == rootwindow)
	    return;
	w->cursor = NULL;
	if (mouse_grabber == 0)
	    Check_cursor (w);
	if (--curs->refcnt == 0)
	    FreeCursor (curs);
}

/* Start up the mouse. */

Startup_mouse ()
{
	vsCursor mcursor;

	cursor = rootwindow->cursor;
	LoadCursor (cursor);
	InitMouse ();
	mcursor.x = (mouse_x = (device.width >> 1)) - cursor->xoff;
	mcursor.y = (mouse_y = (device.height >> 1)) - cursor->yoff;
	SetCursorPosition (&mcursor);
	mouse_window = rootwindow;
	mbox.bottom = 0;
	Set_mbox ();
}

/* The cursor in the given window has changed.  Check if it is/was the cursor
 * window, and update the cursor if it is.
 */

Check_cursor (w)
	register WINDOW *w;
{
	register WINDOW *ww;

	if ((ww = mouse_window) == NULL) return;
	for (; ww->cursor == NULL; ww = ww->parent) ;
	if (ww != cursor_window || ww == w) {
	    New_cursor (0, 0);
	    Deal_with_movement ();
	}
}

/* Change the cursor.  The deltas give the physical adjustment from the old
 * cursor to keep the "point" from moving.
 */

New_cursor (deltax, deltay)
	int deltax, deltay;
{
	vsCursor mcursor;
	register CURSOR *curs = cursor;
	short x, y;
	register WINDOW *w, *ww;
	int old = 0;

	mcursor = *device.mouse;
	x = mcursor.x + cursor->xoff + deltax;
	y = mcursor.y + cursor->yoff + deltay;

	while (1) {
	    /* force cursor to stay in bounds */
	    if (x < curs->xmin)
		x = curs->xmin;
	    else if (x > curs->xmax)
		x = curs->xmax;

	    if (y < curs->ymin)
		y = curs->ymin;
	    else if (y > curs->ymax)
		y = curs->ymax;

	    if (mouse_grabber) break;
	    SEARCH(x, y, w, ww);
	    while (w->cursor == NULL)
		w = w->parent;
	    if (old && w == cursor_window)
		break;
	    cursor_window = w;
	    cursor = curs = w->cursor;
	    old = 1;
	}

	if ((x - curs->xoff) != mcursor.x ||
	    (y - curs->yoff) != mcursor.y) {
	    mcursor.x = x - curs->xoff;
	    mcursor.y = y - curs->yoff;
	    SetCursorPosition (&mcursor);
	}
	LoadCursor (curs);
}

/* Deal with mouse motion or window changes */

Deal_with_movement ()
{
	vsCursor mcursor;
	register CURSOR *curs = cursor;
	register WINDOW *w, *ww, *oldw;
	short x, y;
	int new;

	/* read current mouse coordinates */
	mcursor = *device.mouse;
	x = mcursor.x + curs->xoff;
	y = mcursor.y + curs->yoff;

	oldw = mouse_window;

	/* fast check to see if we are still in box */
	if (y < mbox.bottom && y >= mbox.top &&
	    x < mbox.right && x >= mbox.left) {
	    if (x != mouse_x || y != mouse_y) {
		mouse_x = x;
		mouse_y = y;
		Stash_event (oldw, motion_mask, 0, x, y, 0);
	    }
	    /* may need to reset device mbox */
	    Set_mbox ();
	    return;
	}
	mbox.bottom = 0;

	new = 0;
	while (1) {
	    /* force cursor to stay in bounds */
	    if (x < curs->xmin)
		x = curs->xmin;
	    else if (x > curs->xmax)
		x = curs->xmax;

	    if (y < curs->ymin)
		y = curs->ymin;
	    else if (y > curs->ymax)
		y = curs->ymax;

	    SEARCH(x, y, w, ww);
	    if (w == mouse_window) break;
	    mouse_window = w;
	    if (mouse_grabber) break;
	    while (w->cursor == NULL)
		w = w->parent;
	    if (w != cursor_window) {
		cursor_window = w;
		cursor = curs = w->cursor;
		new = 1;
	    }
	}

	if ((x - curs->xoff) != mcursor.x ||
	    (y - curs->yoff) != mcursor.y) {
	    mcursor.x = x - curs->xoff;
	    mcursor.y = y - curs->yoff;
	    SetCursorPosition (&mcursor);
	}
	if TRUE(new)
	    LoadCursor (curs);

	w = mouse_window;
	if (w != oldw) {
	    mouse_x = x;
	    mouse_y = y;
	    while (oldw->level < w->level)
		w = w->parent;
	    if (w == oldw)
		Stash_event (oldw, (long) LeaveWindow,
			     IntoOrFromSubwindow, x, y, 0);
	    else {
		if TRUE(oldw->mapped)
		    Stash_event (oldw, (long) LeaveWindow, 0, x, y, 0);
		while (oldw->level > w->level) {
		    oldw = oldw->parent;
		    if (oldw == w) {
			Stash_event (oldw, (long) EnterWindow,
				     IntoOrFromSubwindow, x, y, 0);
			goto done;	/* grot */
		    } else if ((oldw->mask & LeaveWindow) && TRUE(oldw->mapped))
			Stash_event (oldw, (long) LeaveWindow,
				     VirtualCrossing, x, y, 0);
		}
		while (1) {
		    oldw = oldw->parent;
		    w = w->parent;
		    if (oldw == w)
			break;
		    if ((oldw->mask & LeaveWindow) && TRUE(oldw->mapped))
			Stash_event (oldw, (long) LeaveWindow,
				     VirtualCrossing, x, y, 0);
		}
	    }
	    w = mouse_window;
	    if (oldw != w->parent)
		Stash_enters (oldw, w->parent, 0);
	    Stash_event (w, (long) EnterWindow, 0, x, y, 0);
	} else if (x != mouse_x || y != mouse_y) {
	    Stash_event (w, motion_mask, 0, x, y, 0);
	    mouse_x = x;
	    mouse_y = y;
	}
done:	Set_mbox ();
}

/* Select events */

Select_input (w, client, mask)
	register WINDOW *w;
	int client;
	long mask;
{
	long omask = w->mask;

	/* stop a button grab in progress if no longer interested in release */
	if (w == button_window &&
	    (client != w->client || !(mask & ButtonReleased)))
	    button_window = NULL;
	w->mask = mask;
	w->client = client;

	/* recompute motion box if movement interest changes */
	if (mouse_grabber == 0 && ((omask ^ mask) & motion_mask))
	    Set_mbox ();
}

/* Change the ShiftLock mode */

Set_shiftlock (mode)
	register int mode;
{
	register unsigned mask;

	if (lock_mode != mode) {
	    lock_mode = mode;
	    mask = state_mask;
	    state_mask &= ~ShiftLockMask;
	    state_mask |= lock_mask;
	    lock_mask = mask & ShiftLockMask;
	}
	SetLockLED((lock_mode && (state_mask & ShiftLockMask)) ? 1 : 0);
}

/* Set motion box */

Set_mbox ()
{
	register WINDOW *w, *sw;
	register RECTANGLE *r;
	register CURSOR *curs;
	vsBox b;

	/* recalculate if need be */
	if (mbox.bottom <= mbox.top) {
	    w = mouse_window;
	    /* use containing visible rectangle if any */
	    for (r = w->visible;
		 r && (mouse_x < r->left || mouse_x >= r->right ||
		       mouse_y < r->top || mouse_y >= r->bottom);
		 r = r->next) ;
	    if (r)
		mbox = *(RASTER *) r;
	    else
		mbox = w->vs;
	    if (sw = w->first_child)
		w = sw;
	    else
		sw = w->next_sib;
	    /* clip with obscuring windows */
	    while (1) {
		if (sw == NULL) {
		    if ((w = w->parent) == NULL)
			break;
		    sw = w->next_sib;
		    continue;
		} else if (FALSE(sw->mapped) || (r && sw->kind != IsTransparent)) {
		    sw = sw->next_sib;
		    continue;
		}
		if (sw->vs.top < mbox.bottom && sw->vs.bottom > mbox.top) {
		    if (sw->vs.right > mbox.left && sw->vs.right <= mouse_x)
			mbox.left = sw->vs.right;
		    else if (sw->vs.left < mbox.right && sw->vs.left >= mouse_x)
			mbox.right = sw->vs.left;
		}
		if (sw->vs.left < mbox.right && sw->vs.right > mbox.left) {
		    if (sw->vs.bottom > mbox.top && sw->vs.bottom <= mouse_y)
			mbox.top = sw->vs.bottom;
		    else if (sw->vs.top < mbox.bottom && sw->vs.top >= mouse_y)
			mbox.bottom = sw->vs.top;
		}
		sw = sw->next_sib;
	    }
	}

	/* if anyone wants motion events, we lose */
	if ((mouse_grabber && (mouse_grab_mask & motion_mask)) ||
	    (button_window && (button_window->mask & motion_mask))) {
	    device.mbox->bottom = 0;
	    return;
	}
	for (w = mouse_window; w; w = w->parent) {
	    if (w->mask & motion_mask) {
		device.mbox->bottom = 0;
		return;
	    }
	}
	curs = cursor;
	b.left = mbox.left - curs->xoff;
	b.right = mbox.right - curs->xoff;
	b.top = mbox.top - curs->yoff;
	b.bottom = mbox.bottom - curs->yoff;
	*device.mbox = b;
}

/* Deal with a button/key transition */

Deal_with_input (ev)
	register vsEvent *ev;
{
	register WINDOW *w, *ww;
	short x = ev->vse_x + cursor->xoff;
	short y = ev->vse_y + cursor->yoff;

	w = mouse_window;
	/* lightning usually strikes twice */
	if (y >= mbox.bottom || y < mbox.top ||
	    x >= mbox.right || x < mbox.left) {
	    /* but not this time */
	    SEARCH(x, y, w, ww);
	}
	if (ev->vse_device == VSE_DKB) {
	    if (ev->vse_direction == VSE_KBTUP) {
		Stash_event (w, (long) KeyReleased, ev->vse_key,
			     x, y, ev->vse_time);
		switch (ev->vse_key) {
		    case ShiftKeyCode:
			state_mask &= ~ShiftMask;
			break;
		    case ControlKeyCode:
			state_mask &= ~ControlMask;
			break;
		    case LockKeyCode:
			if TRUE(lock_mode)
			    lock_mask = 0;
			else
			    state_mask &= ~ShiftLockMask;
			break;
		    case MetaKeyCode:
			state_mask &= ~MetaMask;
			break;
		}
	    } else {
		Stash_event (w, (long) KeyPressed, ev->vse_key,
			     x, y, ev->vse_time);
		switch (ev->vse_key) {
		    case ShiftKeyCode:
			state_mask |= ShiftMask;
			break;
		    case ControlKeyCode:
			state_mask |= ControlMask;
			break;
		    case LockKeyCode:
			if TRUE(lock_mode) {
			    state_mask ^= ShiftLockMask;
			    lock_mask = ShiftLockMask;
			    SetLockLED (state_mask & ShiftLockMask ? 1 : 0);
			} else {
			    state_mask |= ShiftLockMask;
			    lock_mask ^= ShiftLockMask;
			}
			break;
		    case MetaKeyCode:
			state_mask |= MetaMask;
			break;
		}
	    }
	} else {
	    if (ev->vse_direction == VSE_KBTUP) {
		Stash_event (w, (long) ButtonReleased, 2 - ev->vse_key,
				x, y, ev->vse_time);
		motion_mask &= ~(LeftDownMotion >> ev->vse_key);
		state_mask &= ~(LeftMask >> ev->vse_key);
		if (!(state_mask & (LeftMask|MiddleMask|RightMask))) {
		    /* check for end of grab */
		    if (button_window || (mouse_grabber && mouse_grab_button))
			Stash_ungrabs ();
		}
	    } else {
		if (button_window == NULL && mouse_grabber == 0) {
		    /* check for start of grab */
		    if (bgrabs[GRABIDX(ev->vse_key, state_mask)].client)
			Button_grab (ev->vse_key, state_mask);
		    else {
			ww = w;
			while (!(ww->mask & ButtonPressed)) {
			    if ((ww = ww->parent) == NULL) break;
			}
			if (ww && (ww->mask & ButtonReleased)) {
			    Stash_grabs (ww->client);
			    button_window = ww;
			}
		    }
		}
		Stash_event (w, (long) ButtonPressed, 2 - ev->vse_key,
				x, y, ev->vse_time);
		motion_mask |= (LeftDownMotion >> ev->vse_key);
		state_mask |= (LeftMask >> ev->vse_key);
	    }
	    Deal_with_movement ();
	}
}

/* Give the client sole possession of the mouse */

Grab_mouse (w, curs, mask, client)
	register WINDOW *w;
	register CURSOR *curs;
	long mask;
	int client;
{
	register CURSOR *ocurs;
	int deltax, deltay;

	if ((button_window && button_window->client != client) ||
	    (mouse_grabber && mouse_grabber != client)) {
	    Xstatus = BadGrab;
	    return;
	}
	ocurs = cursor;
	deltax = ocurs->xoff - curs->xoff;
	deltay = ocurs->yoff - curs->yoff;
	button_window = NULL;
	if (mouse_grabber == 0) {
	    Stash_grabs (client);
	    mouse_grabber = client;
	    ocurs = NULL;
	} else if (mouse_grab_button)
	    ocurs = NULL;
	mouse_grab_window = w;
	mouse_grab_mask = mask;
	mouse_grab_button = 0;

	curs->refcnt++;
	cursor = curs;
	New_cursor (deltax, deltay);
	cursor_window = NULL;
	Deal_with_movement ();
	if (ocurs && --ocurs->refcnt == 0)
	    FreeCursor (ocurs);
}

/* Ungrab the mouse */

Ungrab_mouse (client)
	int client;
{
	if (client == mouse_grabber && mouse_grab_button == 0) {
	    Stash_ungrabs ();
	    Deal_with_movement ();
	}
}

/* Indicates that a client wants sole possession of the mouse when the
 * specified button is down.
 */

Grab_button (w, curs, button, mask, client)
	register WINDOW *w;
	CURSOR *curs;
	unsigned button;
	long mask;
	int client;
{
	register int i;
	register GRAB_INFO *grab;
	register CURSOR *ocurs;

	i = ButtonState(button);
	if (i == 0 || (i & (i - 1))) {
	    Xstatus = BadValue;
	    return;
	} else if (i == 4)
	    i = 3;
	grab = &bgrabs[GRABIDX(3 - i, button)];
	if (grab->client && grab->client != client) {
	    Xstatus = BadGrab;
	    return;
	}
	if (grab->client) {
	    ocurs = grab->cursor;
	    grab->window->bgrabs--;
	} else
	    ocurs = NULL;

	grab->client = client;
	grab->window = w;
	grab->mask = mask;
	grab->cursor = curs;
	curs->refcnt++;
	w->bgrabs++;

	/* check if this is an in-progress update */
	if (mouse_grabber && mouse_grab_button == (button & grabbits)) {
	    Button_grab ((unsigned) 3 - i, button);
	    Deal_with_movement ();
	}
	if (ocurs && --ocurs->refcnt == 0)
	    FreeCursor (ocurs);
}

/* Ungrab a button */

Ungrab_button (button, client)
	unsigned button;
	int client;
{
	register int i;
	register GRAB_INFO *grab;

	i = ButtonState(button);
	if (i == 0 || (i & (i - 1))) {
	    Xstatus = BadValue;
	    return;
	} else if (i == 4)
	    i = 3;
	grab = &bgrabs[GRABIDX(3 - i, button)];
	if (grab->client != client)
	    return;
	grab->client = 0;
	grab->window->bgrabs--;
	/* check if aborting */
	if (client == mouse_grabber && mouse_grab_button == (button & grabbits))
	    Stash_ungrabs ();
	Deal_with_movement ();
	if (--grab->cursor->refcnt == 0)
	    FreeCursor (grab->cursor);
}

/* Start a button grab */

Button_grab (key, mask)
	register unsigned key, mask;
{
	register GRAB_INFO *grab;
	int deltax, deltay;

	grab = &bgrabs[GRABIDX(key, mask)];
	if (mouse_grabber == 0) {
	    Stash_grabs (grab->client);
	    mouse_grabber = grab->client;
	}
	mouse_grab_window = grab->window;
	mouse_grab_mask = grab->mask;
	mouse_grab_button = (mask & (ControlMask|MetaMask|ShiftMask|ShiftLockMask)) |
			    (LeftMask >> key);
	deltax = cursor->xoff - grab->cursor->xoff;
	deltay = cursor->yoff - grab->cursor->yoff;
	cursor = grab->cursor;
	New_cursor (deltax, deltay);
	cursor_window = NULL;
}

/* Conditionally warp mouse to new position */

Warp_mouse (dstw, dstx, dsty, srcw, src)
	WINDOW *dstw, *srcw;
	int dstx, dsty;
	register REGION *src;
{
	int x, y, width, height;
	register int cx, cy;
	register RECTANGLE *v;
	register WINDOW *w;
	vsCursor mcursor;

	w = srcw;
	if FALSE(w->mapped) return;
	/* get absolute coordinates */
	x = w->full.left + src->left;
	y = w->full.top + src->top;
	if ((width = src->width) == 0)
	    width = w->full.right - x;
	if ((height = src->height) == 0)
	    height = w->full.bottom - y;
	mcursor = *device.mouse;
	cx = mcursor.x + cursor->xoff;
	cy = mcursor.y + cursor->yoff;
	if (cx < x || cy < y || cx >= x + width || cy >= y + height)
	    return;
	for (v = w->cmvisible; v; v = v->next) {
	    if (cx < v->left || cy < v->top ||
		cx >= v->right || cy >= v->bottom)
		continue;
	    cx = dstx;
	    cy = dsty;
	    w = dstw;
	    /* get absolute coordinates */
	    while (1) {
		cx += w->full.left;
		cy += w->full.top;
		if TRUE(w->mapped) break;
		w = w->parent;
	    }
	    mcursor = *device.mouse;
	    New_cursor (cx - cursor->xoff - mcursor.x,
			cy - cursor->yoff - mcursor.y);
	    Deal_with_movement ();
	}
}

/* Change keyboard focus */

Focus_keyboard (w)
	register WINDOW *w;
{
	if (key_window != w) {
	    if (key_window)
		Stash_simple (key_window, (long) FocusChange, LeaveWindow);
	    key_window = w;
	    key_level = w->level;
	    Stash_simple (w, (long) FocusChange, EnterWindow);
	}
}

/* Remove grabs that use this window */

Unbutton_window (w)
	register WINDOW *w;
{
	register GRAB_INFO *grab;

	for (grab = &bgrabs[0]; ; grab++) {
	    if (w == grab->window && grab->client) {
		grab->client = 0;
		if (--grab->cursor->refcnt == 0)
		    FreeCursor (grab->cursor);
		if (--w->bgrabs == 0)
		    return;
	    }
	}
}

/* Remove grabs by this client */

Ungrab_client (client)
	int client;
{
	register GRAB_INFO *grab;

	if (client == mouse_grabber ||
	    (button_window && client == button_window->client))
	    Stash_ungrabs ();
	for (grab = &bgrabs[0]; grab != &bgrabs[GRABS]; grab++) {
	    if (client == grab->client) {
		grab->client = 0;
		grab->window->bgrabs--;
		if (--grab->cursor->refcnt == 0)
		    FreeCursor (grab->cursor);
	    }
	}
}

/* Generate leave events at mouse grab */

Stash_grabs (client)
	register int client;
{
	register WINDOW *w;

	w = mouse_window;
	if (w == rootwindow) return;
	if (w->client != client) {
	    while (!(w->mask & LeaveWindow) && (w = w->parent) &&
		   w->client != client) ;
	    if (w && w->client != client)
		Stash_event (mouse_window, (long) LeaveWindow,
			     0, mouse_x, mouse_y, 0);
	    w = mouse_window;
	}
	while ((w = w->parent) != rootwindow) {
	    if (w->client != client && (w->mask & LeaveWindow))
		Stash_event (w, (long) LeaveWindow, VirtualCrossing,
			     mouse_x, mouse_y, 0);
	}
}

/* Restore cursor and generate enter events at mouse ungrab */

Stash_ungrabs ()
{
	register WINDOW *w;
	register int client;
	CURSOR *curs;

	if (button_window) {
	    client = button_window->client;
	    button_window = NULL;
	} else {
	    client = mouse_grabber;
	    mouse_grabber = 0;
	    if (mouse_grab_button)
		New_cursor (0, 0);
	    else {
		curs = cursor;
		New_cursor (0, 0);
		if (--curs->refcnt == 0)
		    FreeCursor (curs);
	    }
	}
	w = mouse_window;
	while (w != rootwindow &&
	       (w->client == client || !(w->mask & EnterWindow)))
	    w = w->parent;
	if (w != rootwindow) {
	    w = mouse_window;
	    if (rootwindow != w->parent)
		Stash_enters (rootwindow, w->parent, client);
	    if (w->client != client) {
		while (!(w->mask & EnterWindow) && (w = w->parent) &&
		       w->client != client) ;
		if (w && w->client != client)
		    Stash_event (mouse_window, (long) EnterWindow,
				 0, mouse_x, mouse_y, 0);
	    }
	}
}

/* Stash enter events in windows */

Stash_enters (p, c, client)
    WINDOW *p, *c;
    int client;
{
    if (p != c->parent)
	Stash_enters (p, c->parent, client);
    if (c->client != client && (c->mask & EnterWindow))
	Stash_event (c, (long) EnterWindow, VirtualCrossing,
		     mouse_x, mouse_y, 0);
}

/* Place an event in the event queue of a window */

Stash_event (w, event, detail, x, y, time)
	register WINDOW *w;
	register long event;
	short x, y;
	unsigned detail, time;
{
	register WINDOW *sub = NULL;
	WINDOW *ww;
	XRep rep;
	int client;
#ifdef DUALTCP
	register swaptype n;
#endif

	/* Find someone who is interested in dealing with this event
	 * and set w to the window lowest in the hierarchy at or
	 * above the original event window that is interested.
	 */

	if (event & (KeyPressed|KeyReleased)) {
	    while (1) {
		if (!(w->mask & event)) {
		    sub = w;
		    if (w->level > key_level) {
			w = w->parent;
			continue;
		    }
		} else if (key_level == 0) {
		    break;
		} else {
		    ww = w;
		    while (ww->level > key_level)
			ww = ww->parent;
		    if (ww == key_window)
			break;
		}
		w = key_window;
		if (!(w->mask & event))
		    return;
		sub = NULL;
		break;
	    }
	    client = w->client;
	} else if (button_window) {
	    while (w && !(w->mask & event)) {
		sub = w;
		w = w->parent;
	    }
	    client = button_window->client;
	    if (w == NULL || w->client != client) {
		sub = w;
		w = button_window;
		if (!((w->mask & event) & ~(EnterWindow|LeaveWindow)))
		    return;
		while (sub && sub->parent != w)
		    sub = sub->parent;
	    }
	} else if (mouse_grabber) {
	    while (w &&
		   !(event & ((w == mouse_grab_window) ? mouse_grab_mask :
							 w->mask))) {
		sub = w;
		w = w->parent;
	    }
	    client = mouse_grabber;
	    if (w == NULL || (w != mouse_grab_window && w->client != client)) {
		if (!((mouse_grab_mask & event) & ~(EnterWindow|LeaveWindow)))
		    return;
		sub = w;
		w = mouse_grab_window;
		while (sub && sub->parent != w)
		    sub = sub->parent;
	    }
	} else {
	    while (!(w->mask & event)) {
		sub = w;
		if ((w = w->parent) == NULL) return;
	    }
	    client = w->client;
	}

	rep.code = event & ~(LeftDownMotion|MiddleDownMotion|RightDownMotion);
	rep.param.l[0] = w->rid;
	rep.param.s[2] = time;
	rep.param.s[3] = state_mask | detail;
	rep.param.s[4] = x - w->full.left;
	rep.param.s[5] = y - w->full.top;
	rep.param.l[3] = (sub ? sub->rid : 0);
#ifdef vax
	rep.param.s[8] = y;
	rep.param.s[9] = x;
#else
#ifdef mc68000
	rep.param.s[9] = y;
	rep.param.s[8] = x;
#else
	rep.param.l[4] = (x << 16) | y;
#endif
#endif
#ifdef DUALTCP
	if (swapped[client]) {
	    swapl(&rep.code);
	    pswapl(&rep, 0);
	    pswaps(&rep, 2);
	    pswaps(&rep, 3);
	    pswaps(&rep, 4);
	    pswaps(&rep, 5);
	    pswapl(&rep, 3);
	    pswapl(&rep, 4);
	}
#endif
	Write (client, (caddr_t) &rep, sizeof (XRep));
}

/* Place window changes in the event queue of a window.
 * If not_just_new, generate an ExposeWindow, else update visible list and
 * generate ExposeRegions (or ExposeWindow).
 */

Stash_changes (w, not_just_new)
	register WINDOW *w;
	register int not_just_new;
{
	register WINDOW *ew = w;
	register RECTANGLE *r, **prev;
	RECTANGLE *changed = NULL;
	XRep rep;
#ifdef DUALTCP
	register swaptype n;
#endif

	while (!(ew->mask & (ExposeWindow|ExposeRegion))) {
	    if (ew = ew->parent)
		continue;
	    if TRUE(not_just_new)
		return;
	    /* nobody interested, just update */
	    prev = &w->visible;
	    while (r = *prev) {
		if (r->type == new_rec) {
		    r->type = contents_rec;
		    *prev = r->next;
		    r->next = changed;
		    changed = r;
		} else
		    prev = &r->next;
	    }
	    if (changed) {
		Merge_rectangles (changed, &w->visible);
		Windex (w);
	    }
	    return;
	}

	rep.param.l[0] = ew->rid;
	rep.param.s[3] = 0;
	rep.param.l[3] = (w == ew) ? 0 : w->rid;
#ifdef DUALTCP
	if (swapped[ew->client]) {
	    rep.code = lswapl(ExposeRegion);
	    pswapl(&rep, 0);
	    pswapl(&rep, 3);
	} else
#endif
	rep.code = ExposeRegion;

	if FALSE(not_just_new) {
	    prev = &w->visible;
	    while (r = *prev) {
		if (r->type == new_rec) {
		    r->type = contents_rec;
		    *prev = r->next;
		    r->next = changed;
		    changed = r;
		    if (ew->mask & ExposeRegion) {
			rep.param.s[4] = r->right - r->left;
			rep.param.s[5] = r->bottom - r->top;
			rep.param.s[8] = r->top - w->full.top;
			rep.param.s[9] = r->left - w->full.left;
#ifdef DUALTCP
			if (swapped[ew->client]) {
			    pswaps(&rep, 4);
			    pswaps(&rep, 5);
			    pswaps(&rep, 8);
			    pswaps(&rep, 9);
			}
#endif
			Write (ew->client, (caddr_t) &rep, sizeof (XRep));
		    } else
			not_just_new = 1;
		} else
		    prev = &r->next;
	    }
	    if (changed) {
		Merge_rectangles (changed, &w->visible);
		Windex (w);
	    }
	}
	if TRUE(not_just_new) {
	    rep.param.s[4] = w->full.right - w->full.left;
	    rep.param.s[5] = w->full.bottom - w->full.top;
	    rep.param.s[8] = 0;
	    rep.param.s[9] = 0;
#ifdef DUALTCP
	    if (swapped[ew->client]) {
		rep.code = lswapl(ExposeWindow);
		pswaps(&rep, 4);
		pswaps(&rep, 5);
	    } else
#endif
	    rep.code = ExposeWindow;
	    Write (ew->client, (caddr_t) &rep, sizeof (XRep));
	}
}

/* Stash CopyArea misses in the event queue of a window */

Stash_misses (w, vis)
	register WINDOW *w;
	register RECTANGLE *vis;
{
	register RECTANGLE *rec;
	register WINDOW *ew = w;
	XRep rep;
#ifdef DUALTCP
	register swaptype n;
#endif

	while (!(ew->mask & ExposeCopy)) {
	    if ((ew = ew->parent) == NULL)
		return;
	}

	rep.param.l[0] = ew->rid;
	rep.param.l[3] = (w == ew) ? 0 : w->rid;
#ifdef DUALTCP
	if (swapped[ew->client]) {
	    rep.code = lswapl(ExposeRegion);
	    pswapl(&rep, 0);
	    rep.param.s[3] = lswaps(ExposeCopy);
	    pswapl(&rep, 3);
	} else {
#endif
	rep.code = ExposeRegion;
	rep.param.s[3] = ExposeCopy;
#ifdef DUALTCP
	}
#endif
	while (rec = vis) {
	    rep.param.s[4] = rec->right - rec->left;
	    rep.param.s[5] = rec->bottom - rec->top;
	    rep.param.s[8] = rec->top - w->full.top;
	    rep.param.s[9] = rec->left - w->full.left;
#ifdef DUALTCP
	    if (swapped[ew->client]) {
		pswaps(&rep, 4);
		pswaps(&rep, 5);
		pswaps(&rep, 8);
		pswaps(&rep, 9);
	    }
#endif
	    Write (ew->client, (caddr_t) &rep, sizeof (XRep));
	    vis = rec->next;
	    FREERECT(rec);
	}

#ifdef DUALTCP
	if (swapped[ew->client])
	    rep.code = lswapl(ExposeCopy);
	else
#endif
	rep.code = ExposeCopy;
	Write (ew->client, (caddr_t) &rep, sizeof (XRep));
}

/* Stash unmap or focus event in the event queue of a window */

Stash_simple (w, event, detail)
	register WINDOW *w;
	register long event;
	unsigned detail;
{
	register WINDOW *ew = w;
	XRep rep;
#ifdef DUALTCP
	register swaptype n;
#endif

	while (!(ew->mask & event)) {
	    if ((ew = ew->parent) == NULL)
		return;
	}

	rep.code = event;
	rep.param.l[0] = ew->rid;
	rep.param.s[3] = detail;
	rep.param.l[3] = (w == ew) ? 0 : w->rid;
#ifdef DUALTCP
	if (swapped[ew->client]) {
	    swapl(&rep.code);
	    pswapl(&rep, 0);
	    pswaps(&rep, 3);
	    pswapl(&rep, 3);
	}
#endif
	Write (ew->client, (caddr_t) &rep, sizeof (XRep));
}

/* Find out coordinates of a point relative to a window. */

Interpret_locator (w, x, y, rep)
	register WINDOW *w;
	short x, y;
	register XRep *rep;
{
	register WINDOW *sw;

	rep->param.l[0] = 0;
	if (x >= w->vs.left && y >= w->vs.top &&
	    x < w->vs.right && y < w->vs.bottom) {
	    /* see if it is in a subwindow */
	    for (sw = w->last_child; sw; sw = sw->prev_sib) {
		if (TRUE(sw->mapped) &&
		    x >= sw->full.left && y >= sw->full.top &&
		    x < sw->full.right && y < sw->full.bottom) {
		    rep->param.l[0] = sw->rid;
		    break;
		}
	    }
	}
	rep->param.s[2] = x - w->full.left;
	rep->param.s[3] = y - w->full.top;
	rep->param.s[4] = state_mask;
}
