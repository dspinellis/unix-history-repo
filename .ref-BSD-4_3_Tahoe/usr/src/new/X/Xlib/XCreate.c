#include <X/mit-copyright.h>

/* $Header: XCreate.c,v 10.10 86/08/11 12:55:45 wesommer Rel $ */
/* Copyright 1985, Massachusetts Institute of Technology */
/* stolen from CLU routine x_cons, redone by J. Gettys */

#include "XlibInternal.h"
#include <stdio.h>
#define TRUE  1
#define FALSE 0
#define max(a,b) ( (a) > (b) ? (a) : (b) )
#define min(a,b) ( (a) > (b) ? (b) : (a) )
#define abs(a) ( (a) > 0 ? (a) : -(a))

#define DCOUNT 2
#define VCOUNT 1 + (4  * 2 * DCOUNT)
#define FCOUNT 1 + 4

#define CURSOR_LL 0
#include "../cursors/ll_angle.cursor"
#include "../cursors/ll_angle_mask.cursor"
#define CURSOR_LR 1
#include "../cursors/lr_angle.cursor"
#include "../cursors/lr_angle_mask.cursor"
#define CURSOR_UL 2
#include "../cursors/ul_angle.cursor"
#include "../cursors/ul_angle_mask.cursor"
#define CURSOR_UR 3
#include "../cursors/ur_angle.cursor"
#include "../cursors/ur_angle_mask.cursor"


Window XCreate(prompt, name, geometry, def, frame, minwidth, minheight)
	char *prompt, *name;
	register OpaqueFrame *frame;
	int minwidth, minheight;
	char *geometry, *def;
	{
	char *fn = "8x13";		/* default default font */
	FontInfo *pfninfo;		/* font info on prompt */
	Window pop;			/* pop up window */
	char *opt;
	int pfore, pback;		/* prompt window colors */
	int bpix;			/* border color */
	int mfore, mback;		/* mouse colors */
	int pbw = 1;			/* prompt window border width */
	int ibw = 1;			/* internal border width */
	int freeze = 0;			/* should we freeze the server */
	Color cdef;			/* color structure for lookup */
	Cursor ur,ul,lr,ll;		/* cursors for rubber banding    */
	int events;			/* event mask */
	int popw, poph;			/* prompt window height and width */
	int count = VCOUNT;		/* vertex count */
	Pixmap save = 0;		/* saved pixmap */
	Pixmap backmap, bdrmap;		/* background and border pixmaps */
	Vertex box[VCOUNT];		/* vertex list for box */
	int x1, y1;			/* location of mouse */
	int x2, y2;			/* other corner of box */
	Window subw;			/* subwindow it is in */
	int mindim;			/* minimum dimension */
	int width, height;		/* width and height of box */
	int left, stop;
	int mouse;
	int force;			/* force default window to track */
	int xa, ya, xb, yb;
	int doit;
	XButtonEvent event;		/* someplace to put the event */
	register int i;			/* ye olde indexe variabel */
	int fx, fy, fw, fh;		/* frame from parse... */
	int pr;				/* parse geometry result */
	int change_cursor = FALSE;
	int current_cursor = CURSOR_UL;

	int HereButton = LeftButton;	/* make window at locator */
	int ResizeButton = MiddleButton;/* make window of specified size */
	int DefaultButton = RightButton;/* make window at default location */

	pr = XGeometry (geometry, def, frame->bdrwidth, 1, 1, 0, 0,
		&fx, &fy, &fw, &fh);
	/*
	 * none of this nonsense would be necessary if I hadn't blown
	 * the frame structure definition.
	 */
	frame->x = fx;
	frame->y = fy;
	frame->width = fw;
	frame->height = fh;
	if (geometry != NULL) {
		/*
		 * if x or y offsets are specified, then we should autoplace
		 */
		if ( (pr & XValue ) || (pr & YValue) ) {
		    if (frame->width < minwidth) frame->width = minwidth;
		    if (frame->height < minheight) frame->height = minheight;
		    goto makeit;
		}
	}

	if ((opt = XGetDefault(name, "MakeWindow.BodyFont")) != NULL) fn = opt;

	if ((pfninfo = XOpenFont(fn)) == NULL) {
		fprintf(stderr, "Can't open font %s!\n", fn);
		return(0);
		}

	pfore = mback = WhitePixel;
	pback = bpix = mfore = BlackPixel;
	if ((opt = XGetDefault(name, "MakeWindow.ReverseVideo")) != NULL)
		if (strcmp (opt, "on") == 0) {
			pfore = BlackPixel;
			pback = WhitePixel;
			}

	if ((opt = XGetDefault(name, "MakeWindow.BorderWidth")) != NULL)
		pbw = atoi (opt);

	if ((opt = XGetDefault(name, "MakeWindow.InternalBorder")) != NULL)
		ibw = atoi (opt);

	if ((opt = XGetDefault(name, "MakeWindow.Freeze")) != NULL)
		if (strcmp (opt, "on") == 0) freeze = 1;

	if (DisplayPlanes() > 2) { /* on color display, do color stuff */
	
		if ((opt = XGetDefault(name,"MakeWindow.Foreground")) != NULL)
		    if (XParseColor(opt, &cdef) && XGetHardwareColor(&cdef))
			pfore = cdef.pixel;

		if ((opt = XGetDefault(name,"MakeWindow.Background")) != NULL)
		    if (XParseColor(opt, &cdef) && XGetHardwareColor(&cdef))
			pback = cdef.pixel;

		if ((opt = XGetDefault(name,"MakeWindow.Border")) != NULL)
		    if (XParseColor(opt, &cdef) && XGetHardwareColor(&cdef))
			bpix = cdef.pixel;

		if ((opt = XGetDefault(name,"MakeWindow.Mouse")) != NULL)
		    if (XParseColor(opt, &cdef) && XGetHardwareColor(&cdef))
			mfore = cdef.pixel;

		if ((opt = XGetDefault(name,"MakeWindow.MouseMask")) != NULL)
		    if (XParseColor(opt, &cdef) && XGetHardwareColor(&cdef))
			mback = cdef.pixel;

			}

	if ((opt = XGetDefault(name, "MakeWindow.Here")) != NULL) {
		switch (*opt) {
		case 'L':
		case 'l':
			HereButton = LeftButton;
			break;

		case 'M':
		case 'm':
			HereButton = MiddleButton;
			break;

		case 'R':
		case 'r':
			HereButton = RightButton;
			break;
		/*
		 * Should have a default case, to check for invalid
		 * specification, but what should be done in that case?
		 */
		}
	}

	if ((opt = XGetDefault(name, "MakeWindow.Resize")) != NULL) {
		switch (*opt) {
		case 'L':
		case 'l':
			ResizeButton = LeftButton;
			break;

		case 'M':
		case 'm':
			ResizeButton = MiddleButton;
			break;

		case 'R':
		case 'r':
			ResizeButton = RightButton;
			break;
		/*
		 * Should have a default case, to check for invalid
		 * specification, but what should be done in that case?
		 */
		}
	}

	if ((opt = XGetDefault(name, "MakeWindow.Default")) != NULL) {
		switch (*opt) {
		case 'L':
		case 'l':
			DefaultButton = LeftButton;
			break;

		case 'M':
		case 'm':
			DefaultButton = MiddleButton;
			break;

		case 'R':
		case 'r':
			DefaultButton = RightButton;
			break;
		/*
		 * Should have a default case, to check for invalid
		 * specification, but what should be done in that case?
		 */
		}
	}

	/*
	 * Should verify that HereButton != ResizeButton != DefaultButton,
	 * but what should be done if that's false?
	 */

	ur = XCreateCursor (ur_angle_width, ur_angle_height, ur_angle_bits, 
		ur_angle_mask_bits, ur_angle_x_hot, ur_angle_y_hot,
		mfore, mback, GXcopy);
	ul = XCreateCursor (ul_angle_width, ul_angle_height, ul_angle_bits, 
		ul_angle_mask_bits, ul_angle_x_hot, ul_angle_y_hot,
		mfore, mback, GXcopy);
	ll = XCreateCursor (ll_angle_width, ll_angle_height, ll_angle_bits, 
		ll_angle_mask_bits, ll_angle_x_hot, ll_angle_y_hot,
		mfore, mback, GXcopy);
	lr = XCreateCursor (lr_angle_width, lr_angle_height, lr_angle_bits, 
		lr_angle_mask_bits, lr_angle_x_hot, lr_angle_y_hot,
		mfore, mback, GXcopy);

	events = ButtonPressed | ButtonReleased;

	if (freeze) events |= MouseMoved;

	/* 
	 * go get the mouse as soon as you can 
	 */

	while (1) {
		if (XGrabMouse ( RootWindow, ul, events ) != 0) break;
		sleep (1);
		}
	popw = XStringWidth (prompt, pfninfo, 0, 0) + 2 * ibw;
	poph = pfninfo->height + 2 * ibw;

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
	XMapWindow( pop);
	XText (pop, ibw, ibw, prompt, strlen(prompt), 
		pfninfo->id, pfore, pback);

	XQueryMouse (RootWindow, &x1, &y1, &subw);

	mindim = 2 * frame->bdrwidth - 1;
	minwidth  = minwidth  + mindim;
	minheight = minheight + mindim;

	x2 = x1 + minwidth;
	y2 = y1 + minheight;

	width  = minwidth;
	height = minheight;

	left = TRUE;
	force = FALSE;
	stop = FALSE;
	mouse = TRUE;

	xa = ya = xb = yb = -1;

	doit = TRUE;

	while (stop == FALSE) {
	    if (  (xb != max (x1, x2)) || (yb != max(y1, y2))
		||(xa != min (x1, x2)) || (ya != min(y1, y2))) {
		if (freeze && (doit == FALSE)) {
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
	    if (doit) {
		XDraw(RootWindow, box, count, 1, 1, 0, GXinvert, 1);
		doit = !freeze;
		}
	    if (freeze || XPending() ) {
		register int button;
		XNextEvent(&event);
		button = event.detail & ValueMask;
		if (mouse) {
			x2 = event.x;
			y2 = event.y;
			}
		if ( (left == TRUE) && ( event.type == ButtonPressed ) && 
			( button == ResizeButton ) ) {
			x1 = x2;
			y1 = y2;
			left = FALSE;
			change_cursor = TRUE;
			}
		else if ( (left == FALSE) && (event.type == ButtonReleased) &&
			( button == ResizeButton))
					stop = TRUE;
		else if ( (left == TRUE) && (event.type == ButtonPressed) &&
			( button == DefaultButton)) {
			x1 = frame->x;
			y1 = frame->y;
			x2 = x1 + frame->width + mindim;
			y2 = y1 + frame->height + mindim;
			mouse = FALSE;
			left = FALSE;
			}
		else if ( (left == FALSE) && (event.type == ButtonReleased) &&
			( button == DefaultButton)) {
			x1 = frame->x;
			y1 = frame->y;
			x2 = x1 + frame->width + mindim;
			y2 = y1 + frame->height + mindim;
			stop = TRUE;
			}
		else if ( (left == TRUE) && (event.type == ButtonPressed) &&
			( button == HereButton)) {
			force = TRUE;
			left = FALSE;
			}
		else if ( (left == FALSE) && (event.type == ButtonReleased) &&
			( button == HereButton)) {
			stop = TRUE;
			}
		else if (mouse) XQueryMouse (RootWindow, &x2, &y2, &subw);
		}
	    else if (mouse) XQueryMouse (RootWindow, &x2, &y2, &subw);
	    if (change_cursor) {
		if ((x2 >= x1) && (y2 >= y1) &&
		    current_cursor != CURSOR_LR) {
		    XGrabMouse ( RootWindow, lr, events );
		    current_cursor = CURSOR_LR;
		}
		else if ((x2 >= x1) && (y2 < y1) &&
			 current_cursor != CURSOR_UR) {
		    XGrabMouse ( RootWindow, ur, events);
		    current_cursor = CURSOR_UR;
		}
		else if ((x2 < x1) && (y2 >= y1) &&
			 current_cursor != CURSOR_LL) {
		    XGrabMouse ( RootWindow, ll, events);
		    current_cursor = CURSOR_LL;
		}
		else if ((x2 < x1) && (y2 < y1) &&
			 (current_cursor != CURSOR_UL)) {
		    XGrabMouse ( RootWindow, ul, events);
		    current_cursor = CURSOR_UL;
		}
	    }
	    if (force) {	/* we force the default box */
		x1 = x2;
		y1 = y2;
		x2 = x1 + frame->width + mindim;
		y2 = y1 + frame->height + mindim;
		}

	    if (left) {
		x1 = x2;
		y1 = y2;
		}
	    width = max (abs (x2 - x1), minwidth);

	    if (x2 < x1) x2 = x1 - width;
	    else 	 x2 = x1 + width;

	    height = max (abs (y2 - y1), minheight);
	    if (y2 < y1) y2 = y1 - height;
	    else 	 y2 = y1 + height;
	}
	if (freeze) XDraw (RootWindow, box, count, 1, 1, 0, GXinvert, 1);
	XUngrabMouse();

	if (save != 0) {
		XUnmapTransparent (pop);
		XPixmapPut (RootWindow, 0, 0, 0, 0,
			popw + 2 * pbw, poph + 2 * pbw,
			save, GXcopy, AllPlanes);
		XFreePixmap (save);
		}
	XDestroyWindow (pop);
	if (freeze) XUngrabServer();
	XCloseFont (pfninfo);
	XFreeCursor (ur);
	XFreeCursor (ul);
	XFreeCursor (lr);
	XFreeCursor (ll);
	XFreePixmap (backmap);
	XFreePixmap (bdrmap);
	frame->x = min(x1, x2);
	frame->y = min(y1, y2);
	frame->width = width - mindim;
	frame->height = height - mindim;
makeit:	XCreateWindows(RootWindow, frame, 1);
	/* store default name of the window and set the resize hint */
	XStoreName(frame->self, prompt);
	XSetResizeHint(frame->self, minwidth, minheight, 1, 1);
	XSync(1);		/* get rid of any extraneous events */
	return (frame->self);
	}

