#include <X/mit-copyright.h>

/* $Header: XCreateTerm.c,v 10.10 86/02/01 15:31:14 tony Rel $ */
/* Copyright 1985, Massachusetts Institute of Technology */
/* stolen from CLU routine x_tcons, redone by J. Gettys */

#include "XlibInternal.h"
#include <stdio.h>
#include <strings.h>
#define TRUE  1
#define FALSE 0
#define max(a,b) ( (a) > (b) ? (a) : (b) )
#define min(a,b) ( (a) > (b) ? (b) : (a) )
#define abs(a) ( (a) > 0 ? (a) : -(a))

#define DCOUNT 2
#define VCOUNT 1 + (4  * 2 * DCOUNT)
#define FCOUNT 1 + 4

#include "../cursors/cross.cursor"
#include "../cursors/cross_mask.cursor"

Window XCreateTerm(name, prog, geometry, def, frame, minwidth, minheight,
	xadder, yadder, cwidth, cheight, f, fwidth, fheight)
	char *name, *prog;		/* prompt string and name of program */
	register OpaqueFrame *frame;	/* frame for resulting window 	   */
	int minwidth, minheight;	/* this time in units of characters*/
	int *cwidth, *cheight;		/* returns the size of the window  */
	FontInfo *f;			/* major bodyfont of the window	   */
	int fwidth, fheight;		/* width and height of increments  */
	int xadder, yadder;		/* add is size of internal padding */
	{
	int pr;				/* parse geometry result	    */
	int defwidth, defheight;	/* frame from parse...		    */
	int defx, defy;
	FontInfo *pfont;
	int pfore, pback;		/* prompt foreground and background */
	int bpix;
	int mfore;			/* mouse cursor colors		    */
	int mback;			/* background color for mouse	    */
	int clip = 0;			/* clip window to screen	    */
	int freeze = 0;			/* freeze server		    */
	Color cdef;			/* color structure		    */
	int events;			/* what events we want.		    */
	int popw, poph;			/* width and height of prompt window*/
	char text[64];			/* text for prompt string	    */
	int zero = '0';			/* zero offset for char conversion  */
	int x1, y1;			/* location of mouse 		*/
	int x2, y2;			/* other corner of box		*/
	XButtonEvent e;			/* someplace to put the event	*/
	Cursor cr;			/* cursor for rubber banding    */
	Window pop;			/* pop up prompt window		*/
	Pixmap save = 0;		/* saved pixmap 		*/
	Pixmap backmap, bdrmap;		/* background and border pixmaps*/
	Vertex box[VCOUNT];		/* vertex list for box 		*/
	Window subw;			/* window cursor was in (not used) */
	register int i;			/* ye olde indexe variabel 	*/
	int count = VCOUNT;		/* vertex count 		*/
	int nz;				/* count where zeros are 	*/
	int xadd, yadd;
	int hsize, vsize;
	char *opt;			/* option back from XGetdefault */
	int ibw = 0;			/* internal border width 	*/
	int pbw = 0;			/* prompt window border width 	*/
	int bwidth = frame->bdrwidth;	/* border width of final window */
	int xa = -1, ya = -1, xb = -1, yb = -1;
	int chosen = -1;
	int stop = FALSE;
	int changed = TRUE;
	int doit = TRUE;
	int xmindim, ymindim;
	int d;
	
	pr = XGeometry(geometry, def, bwidth, fwidth, fheight, xadder, yadder,
			&defx, &defy, &defwidth, &defheight);
	defwidth = max(defwidth, minwidth);
	defheight = max(defheight, minheight);

	/* "do the right thing" if the user asked for placement */
	if ((pr & XValue) || (pr & YValue)) {
		*cwidth = defwidth;
		*cheight = defheight;
		frame->width	= defwidth * fwidth + xadder;
		frame->height	= defheight * fheight + yadder;
		frame->x	= defx;
		frame->y	= defy;
		goto makeit;
	}
	if ((opt = XGetDefault(prog, "MakeWindow.BodyFont")) == NULL)
		pfont = f;
	else
		if ((pfont = XOpenFont(opt)) == NULL) pfont = f;

	pfore = WhitePixel;
	pback = BlackPixel;

	if ((opt = XGetDefault(prog, "MakeWindow.ReverseVideo")) != NULL)
		if (strcmp(opt, "on") == 0) {
			pfore = BlackPixel;
			pback = WhitePixel;
		}
	bpix = pback;
	mfore = pback;
	mback = pfore;

	if ((opt = XGetDefault(prog, "MakeWindow.BorderWidth")) != NULL)
		pbw = atoi (opt);

	if ((opt = XGetDefault(prog, "MakeWindow.InternalBorder")) != NULL)
		ibw = atoi (opt);

	if ((opt = XGetDefault(prog, "MakeWindow.Freeze")) != NULL)
		if (strcmp (opt, "on") == 0) freeze = 1;

	if ((opt = XGetDefault(prog, "MakeWindow.ClipToScreen")) != NULL)
		if (strcmp (opt, "on") == 0) clip = 1;

	if (DisplayPlanes() > 2) { /* on color display, do color stuff */
	
		if ((opt = XGetDefault(prog,"MakeWindow.Foreground")) != NULL)
		    if (XParseColor(opt, &cdef) && XGetHardwareColor(&cdef))
			pfore = cdef.pixel;

		if ((opt = XGetDefault(prog,"MakeWindow.Background")) != NULL)
		    if (XParseColor(opt, &cdef) && XGetHardwareColor(&cdef))
			pback = cdef.pixel;

		if ((opt = XGetDefault(prog,"MakeWindow.Border")) != NULL)
		    if (XParseColor(opt, &cdef) && XGetHardwareColor(&cdef))
			bpix = cdef.pixel;

		if ((opt = XGetDefault(prog,"MakeWindow.Mouse")) != NULL)
		    if (XParseColor(opt, &cdef) && XGetHardwareColor(&cdef))
			mfore = cdef.pixel;

		if ((opt = XGetDefault(prog,"MakeWindow.MouseMask")) != NULL)
		    if (XParseColor(opt, &cdef) && XGetHardwareColor(&cdef))
			mback = cdef.pixel;
	}
	cr = XCreateCursor (cross_width, cross_height, cross_bits, 
		cross_mask_bits, cross_x_hot, cross_y_hot,
		mfore, mback, GXcopy);

	events = ButtonPressed | ButtonReleased;

	if (freeze) events |= MouseMoved;

	/* 
	 * go get the mouse as soon as you can 
	 */

	while (1) {
		if (XGrabMouse ( RootWindow, cr, events ) != 0) break;
		sleep (1);
	}
	(void) strncpy(text, name, sizeof(text) - 10);
	(void) strncat(text, ": 000x000", 9);
	nz = strlen(name) + 8;		/* compute number of characters */
	popw = XStringWidth (text, pfont, 0, 0) + 2 * ibw;
	poph = pfont->height + 2 * ibw;

	if (freeze) {
		XGrabServer();
		count = FCOUNT;
		save = XPixmapSave (RootWindow, 0, 0, 
			popw + 2 * pbw, poph +2 * pbw);
	}

	backmap = XMakeTile (pback);
	bdrmap = XMakeTile (bpix);

	pop = XCreateWindow (RootWindow, 
		0, 0, popw, poph, pbw, bdrmap, backmap);
	XMapWindow (pop);

	xadd = fwidth / 2 - xadder;
	yadd = fheight / 2 - yadder;

	XQueryMouse (RootWindow, &x1, &y1, &subw);

	x2 = x1 + minwidth * fwidth + xadder + 2 * bwidth - 1;
	y2 = y1 + minheight * fheight + yadder + 2 * bwidth - 1;
	hsize = minwidth;
	vsize = minheight;


	xmindim = xadder + 2 * bwidth;
	ymindim = yadder + 2 * bwidth;

	while (stop == FALSE) {
	    if ( (xb != max (x1, x2)) || (yb != max (y1, y2))
		||(xa != min (x1, x2)) || (ya != min (y1, y2)) ) {
		if (freeze && !doit) {
			XDraw (RootWindow, box, count, 1, 1, 0, GXinvert, 1);
		}
		xa = min (x1, x2);
		ya = min (y1, y2);
		xb = max (x1, x2);
		yb = max (y1, y2);
		for ( i = 0; i < count; i += 4) {
		    box[i].x = xa; box[i].y = ya; box[i].flags = 0;
		    if (i+1 == count) break;
		    box[i+1].x = xb; box[i+1].y = ya, box[i+1].flags = 0;
		    box[i+2].x = xb; box[i+2].y = yb, box[i+2].flags = 0;
		    box[i+3].x = xa; box[i+3].y = yb, box[i+3].flags = 0;
		}
		doit = TRUE;
	    }
	    if (changed) {
		changed = FALSE;
		text[nz - 6] = hsize / 100 + zero;
		text[nz - 5] = (hsize / 10) % 10 + zero;
		text[nz - 4] = hsize % 10 + zero;
		text[nz - 2] = vsize / 100 + zero;
		text[nz - 1] = (vsize / 10) % 10 + zero;
		text[nz]     = vsize % 10 + zero;
		XText (pop, ibw, ibw, 
			text, strlen(text), pfont->id, pfore, pback);
	    }
	    if (doit) {
		XDraw(RootWindow, box, count, 1, 1, 0, GXinvert, 1);
		doit = !freeze;
	    }
	    if (freeze || XPending() ) {
		XNextEvent(&e);
		x2 = e.x;
		y2 = e.y;
		if ((chosen < 0) && (e.type == ButtonPressed)) {
			x1 = x2;
			y1 = y2;
			chosen = e.detail & ValueMask;
		} 
		else if ((e.type == ButtonReleased) && 
			((e.detail & ValueMask) == chosen))
			stop = TRUE;
		else
			XQueryMouse(RootWindow, &x2, &y2, &subw);
	    }
	    else	XQueryMouse(RootWindow, &x2, &y2, &subw);
	    if (chosen != MiddleButton) {
		x1 = x2;
		y1 = y2;
		if (chosen >= 0) {
			x2 = defwidth;
			if (chosen == LeftButton)
				y2 = defheight;
			else
				y2 = (DisplayHeight() - ymindim - cross_y_hot)
					/ fheight;
			if (clip) {
				x2 = min (max((DisplayWidth() - x1 - xmindim) /
					fwidth, 0), x2);
				y2 = min (max((DisplayHeight() - y1 - ymindim)/
					fheight, 0), y2);
			}
			x2 = x1 + x2 * fwidth + xadder - 1;
			y2 = y1 + y2 * fheight + yadder - 1;
		}
	    }
	    d = max (( abs (x2 - x1) + xadd) / fwidth, minwidth);
	    if (d != hsize) {
	    	hsize = d;
		changed = TRUE;
	    }
	    d = d * fwidth + xmindim - 1;
	    if (x2 < x1)
		x2 = x1 - d;
	    else
		x2 = x1 + d;
	    d = max ((abs(y2 - y1) + yadd) / fheight, minheight);
	    if (d != vsize) {
	    	vsize = d;
		changed = TRUE;
	    }
	    d = d * fheight + ymindim - 1;
	    if (y2 < y1)
		y2 = y1 - d;
	    else
	    	y2 = y1 + d;
	}
	if (freeze) XDraw (RootWindow, box, count, 1, 1, 0, GXinvert, 1);
	XUngrabMouse();

	if (save) {
		XUnmapTransparent (pop);
		XPixmapPut (RootWindow, 0, 0, 0, 0,
			popw + 2 * pbw, poph + 2 * pbw,
			save, GXcopy, AllPlanes);
		XFreePixmap (save);
		}
	XDestroyWindow (pop);
	if (freeze) XUngrabServer();
	if (pfont != f) XCloseFont (pfont);
	XFreeCursor (cr);
	XFreePixmap (backmap);
	XFreePixmap (bdrmap);
	frame->x = min(x1, x2);
	frame->y = min(y1, y2);
	frame->width = hsize * fwidth + xadder;
	frame->height = vsize * fheight + yadder;
	*cwidth = hsize;
	*cheight = vsize;
makeit:	XCreateWindows(RootWindow, frame, 1);
	/* store default name of the window and set the resize hint */
	XStoreName(frame->self, name);
	XSetResizeHint(frame->self, xadder, yadder, fwidth, fheight);
	XSync(1);		/* get rid of any extraneous events */
	return (frame->self);
	}
