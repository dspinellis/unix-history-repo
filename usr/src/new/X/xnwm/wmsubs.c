#include "wm.h"

#include "../cursors/icon.cursor"
#include "../cursors/xterm.cursor"
#include "../cursors/xterm_mask.cursor"

#define solid_vcount (1 + 4*2)	/* We're goin' around twice */
#define flash_vcount (1 + 4*4)	/* 4 times, if flashing! */

#ifndef lint
static char *rcsid_wmsubs_c = "$Header: wmsubs.c,v 10.3 86/02/01 16:02:10 tony Rel $";
#endif

int vcount;
Vertex solidBox[solid_vcount], flashBox[flash_vcount], *box;

int iconHeight;
Window sizeWin;
int sizeWidth, sizeHeight;
int variableWidth;
int sizeX, sizeY;		/* Where the size window is */
Pixmap behindSize = 0;		/* And what it obscures */

Window oldfocus;

InitializeWm()
{
	SetUpSizeWindow();
	SetUpBox();
	StoreWmCursors();

	FocusOn(RootWindow);
	oldfocus = 0;
}

SetUpSizeWindow()
{
	FontInfo finfo;
	register int i, minwid, maxwid;
	short cwid[10];

	status = XQueryFont(sizefont, &finfo);
	if (status == 0) Error("Couldn't query size font in SetUpSizeWindow");

	status = XCharWidths("0123456789x", 11, sizefont, cwid);
	if (status == 0) Error("Couldn't get char widths in SetUpSizeWindow");

	minwid = 99999;
	maxwid = 0;
	for (i = 0; i <= 10; i++) {
	    if (cwid[i] < minwid) minwid = cwid[i];
	    else if (cwid[i] > maxwid) maxwid = cwid[i];
	}

	variableWidth = (minwid != maxwid);

	sizeWidth = 7 * maxwid + 4;
	sizeHeight = finfo.height + 4;

	sizeWin = XCreateWindow(RootWindow, 0, 0,
		sizeWidth - 2, sizeHeight - 2,
		1, bgPixmap, fgPixmap);
	if (sizeWin == 0) Error("Couldn't create sizeWin in SetUpSizeWindow");

	status = XQueryFont(iconfont, &finfo);
	if (status == 0) Error("Couldn't query icon font in SetUpSizeWindow");

	iconHeight = finfo.height + 8;
}

SetUpBox()
{
	register int i;

	if (freeze) {
	    vcount = solid_vcount;
	    i = solid_vcount - 1;
	    box = solidBox;
	} else {
	    vcount = flash_vcount;
	    i = flash_vcount - 1;
	    box = flashBox;
	}

	box[i--].flags = VertexRelative | VertexDrawLastPoint;
	while (i > 0) box[i--].flags = VertexRelative;
	box[0].flags = 0;

	if (!freeze) box[solid_vcount-1].flags |= VertexDrawLastPoint;
}

StoreWmCursors()
{
	iconCursor = XCreateCursor(icon_width, icon_height, 
		(caddr_t) icon_bits, (caddr_t) NULL,
		8, 8,
		WhitePixel, BlackPixel,
		GXcopyInverted);
	if (iconCursor == 0) {
	    Error("Couldn't store iconCursor in StoreWmCursors");
	}
	textCursor = XCreateCursor(xterm_width, xterm_height, 
		(caddr_t) xterm_bits, (caddr_t) xterm_mask_bits,
		8, 8,
		bgColor, fgColor,
		GXcopyInverted);
	if (textCursor == 0) {
	    Error("Couldn't store textCursor in StoreWmCursors");
	}
}

/* ARGSUSED */

Raise(which, loc, w, winfo)
	int which;
	Locator loc;
	Window w;
	WindowInfo *winfo;
{
	BEvent newbutton;
	Window neww;

	if (w == RootWindow) return;

	GetButton(&newbutton);
	if (!MatchUp(newbutton, which)) return;

	InterpretLocatorW(RootWindow, &neww, newbutton.location);
	if (neww != w) return;

	if (popup) UnmapPopup();

	XRaiseWindow(w);
}

/* ARGSUSED */

Lower(which, loc, w, winfo)
	int which;
	Locator loc;
	Window w;
	WindowInfo *winfo;
{
	BEvent newbutton;
	Window neww;

	if (w == RootWindow) return;

	GetButton(&newbutton);
	if (!MatchUp(newbutton, which)) return;

	InterpretLocatorW(RootWindow, &neww, newbutton.location);
	if (neww != w) return;

	if (popup) UnmapPopup();

	XLowerWindow(w);
}

Move(which, loc, w, winfo)
	int which;
	Locator loc;
	Window w;
	WindowInfo *winfo;
{
	Window subwindow;
	BEvent newbutton;
	int x, y, oldx, oldy, left, top;
	int stop = FALSE;

	if (w == RootWindow) return;

	InterpretLocatorXY(RootWindow, &x, &y, loc);

	left = winfo->x;
	top = winfo->y;

	oldx = x;
	oldy = y;

	StoreBox(left, top, winfo->width + (winfo->bdrwidth << 1) - 1,
		winfo->height + (winfo->bdrwidth << 1) - 1);

	/* If we're willing to freeze the server, we don't flicker the box.
	   If not, we do double inverts that draw and then erase the box,
	   so we have to do them more often */

	if (freeze) XGrabServer();
	DrawBox();

	while (!stop) {
	    /* If we've moved at all, change the box and reset old x,y */

	    if (x != oldx || y != oldy) {
		if (freeze) DrawBox();		/* Erase */
		oldx = x;
		oldy = y;
		box[0].x = left;
		box[0].y = top;
		DrawBox();		/* Redraw */
	    } else if (!freeze) DrawBox();

	    /* Find the new x,y.  If there's an event, use that; otherwise
	       query the mouse */

	    if (XPending()) {
		if (!GetEvent(&newbutton)) {
		    QueryMouse(RootWindow, &x, &y, &subwindow);
		}
		else if (!MatchUp(newbutton, which)) break;
		else {
		    x = newbutton.x;
		    y = newbutton.y;
		    stop = TRUE;
		}
	    } else QueryMouse(RootWindow, &x, &y, &subwindow);

	    left += x - oldx;
	    top += y - oldy;
	}

	if (freeze) {
	    DrawBox();			/* Erase */
	    XUngrabServer();
	}

	if (!stop) return;

	if (popup) UnmapPopup();

	XMoveWindow(w, left, top);

	FrameFocus();	/* Fix up frame */
}

Resize(which, loc, w, winfo)
	int which;
	Locator loc;
	Window w;
	WindowInfo *winfo;
{
	BEvent newbutton;
	int t;
	int baseheight, hinc, basewidth, winc;
	int x0, y0;		/* Initial x,y of mouse */
	int fixedx, fixedy;	/* x,y of fixed corner */
	int movex, movey;	/* x,y of movable corner */
	int limitx, limity;	/* limit to movement due to min window size */
	int oldx, oldy;		/* previous location of moving corner */
	int newx, newy;		/* new location of moving corner */
	int lastx, lasty;	/* previous cursor location */
	int x, y;		/* new cursor location */
	int dx, dy;		/* flags indicating movement direction */
	int width, height;	/* width & height of window */
	int usesize, stop = FALSE;
	Window subwindow;

	if (w == RootWindow) return;

	/* Find out about window and get resizing info */

	status = XGetResizeHint(w, &basewidth, &baseheight, &winc, &hinc);
	if (status == 0) Error("Couldn't get resize hint in Resize");
	InterpretLocatorXY(RootWindow, &x0, &y0, loc);

	/* Initially fixedx is left, fixedy top, movex right, and
	   movey bottom */

	fixedx = winfo->x;
	fixedy = winfo->y;
	width = winfo->width + (winfo->bdrwidth << 1) - 1;
	height = winfo->height + (winfo->bdrwidth << 1) - 1;
	movex = fixedx + width;
	movey = fixedy + height;

	/* We only will use a size window if the increments are large
	   enough and the current window size corresponds to the hints */

	usesize = (winc > 3 && hinc > 3 &&
	     (winfo->width - basewidth) % winc == 0 &&
	     (winfo->height - baseheight) % hinc == 0);

	if (basewidth == 0 && winc == 1 && baseheight == 0 && hinc == 1) {
	    basewidth = 1;
	    baseheight = 1;
	}

	/* movex,y are still right and bottom */

	limitx = movex - winfo->width + basewidth + winc;
	limity = movey - winfo->height + baseheight + hinc;

	basewidth += (winfo->bdrwidth << 1) - 1;
	baseheight += (winfo->bdrwidth << 1) - 1;

	/* Calculate the moving directions dx,dy */

	CalculateMovingEdges(&dx, &dy, x0, fixedx, movex,
		y0, fixedy, movey, winfo);

	/* Figure out which edges to move depending upon which ninth
	   the cursor is in.  Adjust fixed and move edges accordingly:

	   dx,y indicate which edges are fixed.  Values:

		dx	fixedx	movex		dy	fixedy	movey
		1,0	left	right		1,0	top	bottom
		-1	right	left		-1	bottom	top

	   A value of 0 means that both edges are fixed in that direction */

	/* If we're moving left edge, switch */

	if (dx == -1) {
	    limitx = movex - (limitx - fixedx);
	    t = fixedx; fixedx = movex; movex = t;
	    width = -width;
	}

	/* If we're moving top edge, switch */

	if (dy == -1) {
	    limity = movey - (limity - fixedy);
	    t = fixedy; fixedy = movey; movey = t;
	    height = -height;
	}

	oldx = newx = movex;
	oldy = newy = movey;
	lastx = x0;
	lasty = y0;

	StoreBox(fixedx, fixedy, width, height);

	/* If we're willing to freeze the server, we don't flicker the box.
	   If not, we do double inverts that draw and then erase the box,
	   so we have to do them more often */

	if (freeze) XGrabServer();
	DrawBox();		/* Draw box */

	if (usesize) {
	    CreateSize (dx, dy, fixedx, fixedy, winfo);
	    FillinSize ((abs(width) - basewidth) / winc,
		    (abs(height) - baseheight) / hinc);
	}

	/* Loop until a button event occurs */

	while (!stop) {
	    /* If we've moved at all, change the box, fill in the
	       size window and reset old x,y */

	    if (newx != oldx || newy != oldy) {
		if (freeze) DrawBox();		/* Erase */
		StoreBox(fixedx, fixedy, width, height);

		if (usesize) {
		    FillinSize ((abs(width) - basewidth) / winc,
			    (abs(height) - baseheight) / hinc);
		}
		oldx = newx;
		oldy = newy;
		DrawBox();		/* Redraw */
	    } else if (!freeze) DrawBox();

	    /* Find the cursor x,y -- by an event if there is one, by
	       querying if not */

	    if (XPending()) {
		if (!GetEvent(&newbutton)) {
		    QueryMouse(RootWindow, &x, &y, &subwindow);
		}
		else if (!MatchUp(newbutton, which)) break;
		else {
		    x = newbutton.x;
		    y = newbutton.y;
		    stop = TRUE;
		}
	    } else QueryMouse(RootWindow, &x, &y, &subwindow);

	    /* If we haven't moved since last time, skip the rest */

	    if (x == lastx && y == lasty) continue;

	    lastx = x;
	    lasty = y;

	    newx = CalculateChange(dx, x, x0, winc, limitx, movex, oldx);
	    newy = CalculateChange(dy, y, y0, hinc, limity, movey, oldy);
	    width += newx - oldx;
	    height += newy - oldy;
	}

	if (freeze) {
	    DrawBox();			/* Erase */
	    XUngrabServer();
	}

	if (!stop) {
	    if (usesize) DestroySize();
	    return;
	}


	if (newx == movex && newy == movey) {		/* i.e. no change */
	    if (usesize) DestroySize();
	    XRaiseWindow(w);
	} else {

	    /* Re-exchange things so that fixedx,y is the left top */

	    if (newx < fixedx) {
	        t = fixedx; fixedx = newx; newx = t;
	    }
	    if (newy < fixedy) {
		t = fixedy; fixedy = newy; newy = t;
	    }

	    /* Calculate new width and height. */

	    width = newx - fixedx + 1 - (winfo->bdrwidth << 1);
	    height = newy - fixedy + 1 - (winfo->bdrwidth << 1);

	    if (usesize) DestroySize();

	    if (popup) UnmapPopup();

	    XConfigureWindow(w, fixedx, fixedy, width, height);
	    FrameFocus();
	}
}

CalculateMovingEdges(dx, dy, x0, fixedx, movex, y0, fixedy, movey, winfo)
	int *dx, *dy, x0, fixedx, movex, y0, fixedy, movey;
	WindowInfo *winfo;
{
	int xthird, ythird;

	*dx = *dy = 1;

	/* If we're closer to the left than to the right, switch */

	if (x0 - fixedx < movex - x0) *dx = -1;

	/* If we're closer to the top than the bottom, switch */

	if (y0 - fixedy < movey - y0) *dy = -1;

	/* Now, watch closely!  We take the offset from the point to the
	   left edge, multiply by 3 and divide by the width.  This gives
	   a value of 0, 1, or 2 depending if the point is in the left,
	   middle, or right thirds.  Do the same for y.  Add them together.
	   If the result is odd, the point must be in one of the edge ninths,
	   rather than a corner ninth or the middle ninth.  Figure out
	   which one and set dx,y accordingly. (gag) */

	if (winfo->width > 2 && winfo->height > 2) {
	    xthird = ((x0 - winfo->x - winfo->bdrwidth) * 3) / winfo->width;
	    ythird = ((y0 - winfo->y - winfo->bdrwidth) * 3) / winfo->height;

	    if ((xthird + ythird) & 1) {
		if (xthird & 1) *dx = 0;
		else *dy = 0;
	    }
	}
}

FillinSize(hsize, vsize)
	register int hsize, vsize;
{
	static char sizeText[7] = {'0', '0', '0', 'x', '0', '0', '0'};

	sizeText[0] = vsize / 100 + '0';
	sizeText[1] = (vsize / 10) % 10 + '0';
	sizeText[2] = vsize % 10 + '0';
	sizeText[4] = hsize / 100 + '0';
	sizeText[5] = (hsize / 10) % 10 + '0';
	sizeText[6] = hsize % 10 + '0';
	if (variableWidth) XClear(sizeWin);
	XText(sizeWin, 1, 1, sizeText, sizeof(sizeText), sizefont,
		bgColor, fgColor);
}

CreateSize(dx, dy, fixedx, fixedy, winfo)
	int dx, dy, fixedx, fixedy;
	WindowInfo *winfo;
{
	int px, py;			/* Size x, y */

	/* If a corner is being moved, put the size window in the opposite
	   corner.  If an edge, put in middle of opposite edge. */

	if (dx > 0) px = fixedx + winfo->bdrwidth;
	else if (dx < 0) px = fixedx - sizeWidth - winfo->bdrwidth + 1;
	else px = winfo->x + winfo->bdrwidth +
		(winfo->width - sizeWidth) / 2;

	if (dy > 0) py = fixedy + winfo->bdrwidth;
	else if (dy < 0) py = fixedy - sizeHeight - winfo->bdrwidth + 1;
	else py = winfo->y + winfo->bdrwidth +
		(winfo->height - sizeHeight) / 2;

	if (freeze) {
	    sizeX = px;
	    sizeY = py;
	    behindSize = XPixmapSave(RootWindow, px, py,
		    sizeWidth, sizeHeight);
	    /* Ok to return 0; this means it wasn't on the screen */
	}

	XMoveWindow(sizeWin, px, py);
	XMapWindow(sizeWin);
}

DestroySize()
{
	if (behindSize != 0) {
	    XUnmapTransparent(sizeWin);
	    XPixmapPut(RootWindow, 0, 0, sizeX, sizeY, sizeWidth, sizeHeight,
		    behindSize, GXcopy, AllPlanes);
	    XFreePixmap(behindSize);
	    behindSize = 0;
	} else XUnmapWindow(sizeWin);
}

int CalculateChange(direction, new, orig, inc, limit, origedge, oldedge)
	int direction, new, orig, inc, limit, origedge, oldedge;
{
	register int d, newedge;

	if (direction) {		/* If we're changing this way */

	    /* Calculate the change in cursor position in inc units */

	    d = abs(new - orig) + (inc >> 1);;
	    d = (d / inc) * inc;

	    /* Adjust the new position and check against the limit */

	    if (new < orig) {
		newedge = origedge - d;
		if (direction > 0 && newedge < limit) newedge = limit;
	    } else {
		newedge = origedge + d;
		if (direction < 0 && newedge > limit) newedge = limit;
	    }
	} else newedge = oldedge;

	return(newedge);
}

/* ARGSUSED */

Iconify(which, loc, w, winfo)
	int which;
	Locator loc;
	Window w;
	WindowInfo *winfo;
{
	int x, y;
	BEvent newbutton;
	int width, height;
	char *iconName;
	WindowInfo iconInfo;
	Window icon = 0;
	struct _xy {short x, y;} *xy;	/* To turn locators into xy pairs */
	int downx, downy;

	if (w == RootWindow) return;

	/* First change the cursor into the icon cursor */

	status = XGrabMouse(RootWindow, iconCursor,
		ButtonPressed | ButtonReleased);
	if (status == 0) Error("Couldn't grab mouse in Iconify");

	/* Then wait for the upbutton; if it doesn't occur abort */

	GetButton(&newbutton);
	if (!MatchUp(newbutton, which)) return;
	x = newbutton.x;
	y = newbutton.y;

	/* See if it already has an associated icon window;
	   if not, figure out the size */

	if (winfo->assoc_wind == 0) {
	    height = iconHeight;

	    /* Now get the name of the window */

	    status = XFetchName(w, &iconName);
	    if (status == 0) Error("Couldn't fetch name in Iconify");

	    width = XQueryWidth(iconName, iconfont);
	    if (width == 0) width = height;
	    else width += 8;

	} else {
	    icon = winfo->assoc_wind;
	    QueryWindow(icon, &iconInfo);
	    height = iconInfo.height;
	    width = iconInfo.width;

	    xy = (struct _xy *) &loc;
	    downx = xy->x;
	    downy = xy->y;
	}

	/* Center the icon on the new cursor position */

	x -= width >> 1 + 1;
	if (x < 0) x = 0;
	if (x + width + 2 > screen_width) x = screen_width - width - 2;

	y -= height >> 1 + 1;
	if (y < 0) y = 0;
	if (y + height + 2 > screen_height) y = screen_height - height - 2;

	/* Open the icon, give it a text cursor, choose key events, and
	   map it */

	if (icon == 0) {
	    icon = XCreateWindow(RootWindow, x, y, width, height,
		    1, fgPixmap, gray);
	    if (icon == 0) Error("Couldn't create icon in Iconify");
	    XSelectInput(icon, KeyPressed|ExposeWindow|UnmapWindow);
	    XDefineCursor(icon, textCursor);
	    XSetIconWindow(w, icon);
	} else {
	    xy = (struct _xy *) &(newbutton.location);
	    if (abs(xy->x - downx) > iconifyDelta ||
		    abs(xy->y - downy) > iconifyDelta) {
		XMoveWindow(icon, x, y);
	    }
	}

	if (popup) UnmapPopup();

	XMapWindow(icon);
	XUnmapWindow(w);
	
	/* If the iconified window was the focus, change to the background */

	if (focus == w) FocusOn(RootWindow);
	FrameFocus();
}

/* ARGSUSED */

Deiconify(which, loc, w, winfo)
	int which;
	Locator loc;
	Window w;
	WindowInfo *winfo;
{
	BEvent newbutton;
	Window neww;

	if (w == RootWindow) return;

	GetButton(&newbutton);

	if (!MatchUp(newbutton, which)) return;

	InterpretLocatorW(RootWindow, &neww, newbutton.location);
	if (neww != w) return;

	if (popup) UnmapPopup();

	XUnmapWindow(w);
	XMapWindow(winfo->assoc_wind);

	/* If icon was the focus, change to the window itself */

	if (focus == w) FocusOn(winfo->assoc_wind);
	FrameFocus();
}

/* ARGSUSED */

Select(which, loc, w, winfo)
	int which;
	Locator loc;
	Window w;
	WindowInfo *winfo;
{
	BEvent newbutton;
	Window neww;

	GetButton(&newbutton);
	if (!MatchUp(newbutton, which)) return;

	InterpretLocatorW(RootWindow, &neww, newbutton.location);
	if (neww == 0) neww = RootWindow;
	if (neww != w) return;

	/* See if the current focus is an icon.  If not, set oldfocus
	   so we can go back to it if we want to */

	if (focusInfo.type != IsIcon) oldfocus = focus;

	if (popup) UnmapPopup();

	FocusOn(w);
	FrameFocus();		/* Now frame the focus window */
	XRaiseWindow(focus);
}

FocusOn(w)
	Window w;
{
	focus = w;
	QueryWindow(focus, &focusInfo);
	XFocusKeyboard(focus);
}

FrameFocus()
{
	if (frameWidth == 0) return;	/* Nothing to do */

	/* Remove the frame from around any old focus window first */

	XClear(RootWindow);

	if (focus == RootWindow) return;
	QueryWindow(focus, &focusInfo);	/* Refresh it */

	/* Undo the default clipmode for the base window */
	
	XClipClipped(RootWindow);

	XPixSet(RootWindow, 
		focusInfo.x - frameWidth, focusInfo.y - frameWidth,
		focusInfo.width + 2 * (focusInfo.bdrwidth + frameWidth),
		focusInfo.height + 2 * (focusInfo.bdrwidth + frameWidth),
		fgColor);

	XClipDrawThrough(RootWindow);	/* Put it back */
}

/* ARGSUSED */

Circulate(which, loc, w, winfo)
	int which;
	Locator loc;
	Window w;
	WindowInfo *winfo;
{
	if (popup) UnmapPopup();

	XCircWindowUp(RootWindow);
}

EditIconName(be, name)
        XKeyPressedEvent *be;
	register char *name;
{
	Window w = be->window;
	short charcode = be->detail;
	register int nameLen, c;
	int x0, y0, size;
	WindowInfo winfo;
	register char *string;
	int nbytes;
	register int i;

	string = XLookupMapping (be, &nbytes);
	nameLen = (name == NULL) ? 0 : strlen(name);

	for (i = 0; i < nbytes; i++) {
		c = string[i];

	
		if (c == '\177') {	/* a 'delete' */
		    if (nameLen > 0)
		       name[--nameLen] = '\0';
			 /* control-u if you can't read ascii */
		} else if (c == '\025') {
		    if (nameLen > 0) {
			*name = '\0';
			nameLen = 0;
		    }

		} else if (c == '\r') {		/* return means reset focus */
		    FocusOn(oldfocus ? oldfocus : RootWindow);
		    oldfocus = 0;
		    FrameFocus();

		} else if (c <= 0)
		    ;	/* unknown character, ignore it */

		/* Else append the letter to the name */

		else {
		    if (name == NULL)
		    	name = (char *) malloc (nameLen + 2);
		    else
		    	name = (char *) realloc(name, nameLen + 2);
		    if (name == NULL) {
			errno = ENOMEM;
			perror("newwm:");
		    }
		    name[nameLen] = c;
		    name[++nameLen] = '\0';
		}
	}

	QueryWindow(w, &winfo);
	x0 = winfo.x;
	y0 = winfo.y;

	size = XQueryWidth(name, iconfont);

	/* Make sure icon is entirely on screen */

	if (x0 < 0) x0 = 0;
	else if (x0 + size + 10 > screen_width) {
	    x0 = screen_width - size - 10;
	}

	if (y0 < 0) y0 = 0;
	else if (y0 + iconHeight + 2 > screen_height) {
	    y0 = screen_height - iconHeight - 2;
	}

	XConfigureWindow(w, x0, y0, size + 8, iconHeight);
	XWarpMouse(w, (size+8) >> 1, iconHeight >> 1);
	FrameFocus();		/* Redraw frame */
	XStoreName(winfo.assoc_wind, name);

	/* Don't redraw the text yet; the expose event will cause that */
}

StoreBox (x, y, w, h)
	int x, y, w, h;
{
	box[0].x = x;		box[0].y = y;
	box[1].x = w+2;		box[1].y = 0;
	box[2].x = 0;		box[2].y = h+2;
	box[3].x = -(w+2);	box[3].y = 0;
	box[4].x = 0;		box[4].y = -(h+1);
	box[5].x = w+1;		box[5].y = 0;
	box[6].x = 0;		box[6].y = h;
	box[7].x = -w;		box[7].y = 0;
	box[8].x = 0;		box[8].y = -(h-1);

	if (!freeze) {
	    box[9].x = 0;	box[9].y = h-1;
	    box[10].x = w;	box[10].y = 0;
	    box[11].x = 0;	box[11].y = -h;
	    box[12].x = -(w+1);	box[12].y = 0;
	    box[13].x = 0;	box[13].y = h+1;
	    box[14].x = w+2;	box[14].y = 0;
	    box[15].x = 0;	box[15].y = -(h+2);
	    box[16].x = -(w+2);	box[16].y = 0;
	}
}

DrawBox()
{
	XDraw(RootWindow, box, vcount, 1, 1, 0,
		GXinvert, AllPlanes);
}

InterpretLocator (w, x, y, subw, loc)
	Window w;
	Window *subw;
	Locator loc;
	int *x, *y;
{
	status = XInterpretLocator(w, x, y, subw, loc);
	if (status == 0) Error("Couldn't interpret in InterpretLocator");
}

InterpretLocatorW (w, subw, loc)
	Window w;
	Window *subw;
	Locator loc;
{
	int x, y;

	status = XInterpretLocator(w, &x, &y, subw, loc);
	if (status == 0) Error("Couldn't interpret in InterpretLocator");
}

InterpretLocatorXY (w, x, y, loc)
	Window w;
	Locator loc;
	int *x, *y;
{
	Window subw;

	status = XInterpretLocator(w, x, y, &subw, loc);
	if (status == 0) Error("Couldn't interpret in InterpretLocator");
}

QueryMouse (w, x, y, subw)
	Window w;
	Window *subw;
	int *x, *y;
{
	status = XQueryMouse(w, x, y, subw);
	if (status == 0) Error("Couldn't query mouse in QueryMouse");
}

QueryWindow (w, info)
	Window w;
	WindowInfo *info;
{
	status = XQueryWindow(w, info);
	if (status == 0) Error("Couldn't query windown in QueryWindow");
}
