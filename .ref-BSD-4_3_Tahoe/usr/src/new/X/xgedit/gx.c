#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

#include "gedit.h"
#include <X/Xlib.h>

#ifndef lint
static char *rcsid_gx_c = "$Header: gx.c,v 10.7 86/11/19 19:20:37 jg Rel $";
#endif	lint

char *index();

static short crbits[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

Window w, trw;
Font font;
int forepix, backpix, invplanes;

char mousechanged;
short mousex, mousey;

short wminx;	/* lower left of display window */
short wminy;
short wmaxx;	/* upper right of display window */
short wmaxy;
short chrwid;
short chrhgt;

/* command dispatch per key */
fptr dispatch[256];

/* used to fill in dispatch upon initialization */
int mleft(),newlabel(),stop(),delobj(),editobj(),mright(),toggle();
int quit(),instantiate(),newline(),redisplay(),mdown(),neworg();
int mup(),rotateobj(),selobj(),multiplier(),setwindow(),scale();
int letgo(),rescale(),newin(),newout(),snap(),curup(),curdown();
int home(),setcolor(),lastgo(),angle(),Help();
int editlast(), scaleup(), scaledn(), fixwindow();

struct icmd { char *keys; fptr func; } cmds[] = {
  "[",			scaleup,
  "]",			scaledn,
  "Aa\001",		angle,
  "Bb\002Hh\010",	mleft,
  "Cc",			newlabel,
  "\003\021",		stop,
  "Dd\004",		delobj,
  "Ee\005",		editobj,
  "Ff\006",		mright,
  "Gg",			toggle,
  "\007",		quit,
  "Hh",			Help,
  "Ii",			instantiate,
  "Ll",			newline,
  "Mm",			lastgo,
  "\014",		redisplay,
  "Nn\016",		mdown,
  "\017",		neworg,
  "Pp\020",		mup,
  "Rr\022",		rotateobj,
  "Ss\023",		selobj,
  "Uu\025",		multiplier,
  "Ww\027",		setwindow,
  "Xx\030",		scale,
  "Zz\032 ",		letgo,
  "^",			editlast,
  "*",			rescale,
  "<",			newin,
  ">",			newout,
  "!",			snap,
  "+",			curup,
  "-",			curdown,
  "@",			home,
  "\031",		fixwindow,
  NULL, NULL
};

/* see if there is input ready from the user */
UserReady()
 {	XEvent ev;
	int n, x, y;
	Window sub;

	while (XPending()) {
		XPeekEvent(&ev);
		switch (ev.type) {
		case MouseMoved:
			mousechanged = 1;
			XNextEvent(&ev);
			XUpdateMouse(w, &x, &y, &sub);
			mousex = x;
			mousey = y;
			return(1);
		case ButtonPressed:
			return(1);
		case KeyPressed:
			XLookupMapping((XKeyPressedEvent *)(&ev), &n);
			if (n == 1) return (1);
			XNextEvent(&ev);
			break;
		case ExposeRegion:
		case ExposeWindow:
			return(1);
		}
	}
	return(0);
}

/* get character from user */
UserChar()
 {	XEvent ev;
	int n;
	char *str;

	while (1) {
		XNextEvent(&ev);
		switch (ev.type) {
		case ButtonPressed:
			n = ((XButtonPressedEvent *) (&ev))->detail & ValueMask;
			switch (n) {
			case LeftButton:
				return('l');
			case MiddleButton:
				return('z');
			case RightButton:
				return('s');
			}
			break;
		case KeyPressed:
			str = XLookupMapping((XKeyPressedEvent *)(&ev), &n);
			if (n == 1) return (*str);
			break;
		case ExposeRegion:
			return('\014');
		case ExposeWindow:
			if (((XExposeWindowEvent *) (&ev))->width == wmaxx &&
			    ((XExposeWindowEvent *) (&ev))->height == wmaxy)
				return('\014');
			wmaxx = ((XExposeWindowEvent *) (&ev))->width;
			wmaxy = ((XExposeWindowEvent *) (&ev))->height;
			XChangeWindow(trw, wmaxx, wmaxy - (chrhgt << 1));
			return('\031');
		}
	}
}


/* display grid */
drawgrid(x,y,incrx,incry)
  {	int i;
	Vertex vert[2];

	if (incrx <= 16) {
		vert[0].x = x;
		vert[0].flags = 0;
		vert[1].x = wmaxx;
		vert[1].flags = 0;
		while (y > 0) {
			vert[0].y = y;
			vert[1].y = y;
			XDrawDashed(trw, vert, 2, 1, 1, forepix,
				    XMakePattern(1, incrx, 1), GXcopy, AllPlanes);
			y -= incry;
		}
		XClearVertexFlag();
	} else if (incry <= 16) {
		vert[0].y = y;
		vert[0].flags = 0;
		vert[1].y = 0;
		vert[1].flags = 0;
		while (x < wmaxx) {
			vert[0].x = x;
			vert[1].x = x;
			XDrawDashed(trw, vert, 2, 1, 1, forepix,
				    XMakePattern(1, incry, 1), GXcopy, AllPlanes);
			x += incrx;
		}
		XClearVertexFlag();
	} else {
		while (x < wmaxx) {
		  for (i = y; i > 0; i -= incry) {
		    XPixSet(trw, x, i, 1, 1, forepix);
		  }
		  x += incrx;
		}
	}
}

/* set up display adaptor for 80x25 alpha color, release keyboard */
gexit()
  {
}

track()
 {	int x,y;
	int code = 0;

	mousechanged = 0;

	x = mousex * cur_state.dscale;
	y = (wmaxy - mousey) * cur_state.dscale;
	x += cur_state.worgx;
	y += cur_state.worgy;

	if (x!=cur_state.curx || y!=cur_state.cury) {
	  if (x >= cur_state.wmaxx)
		x = cur_state.wmaxx - 1;
	  else if (x < cur_state.worgx)
		x = cur_state.worgx;
	  if (y >= cur_state.wmaxy)
		y = cur_state.wmaxy - 1;
	  else if (y < cur_state.worgy)
		y = cur_state.worgy;
	  cur_state.curx = x;
	  cur_state.cury = y;
	}

	return(code);
}

remouse(x, y, doit)
 {	
	if (x >= cur_state.wmaxx || x < cur_state.worgx ||
	    y >= cur_state.wmaxy || y < cur_state.worgy) {
		cur_state.curx = x;
		cur_state.cury = y;
		return(RECENTER);
	}
	if (doit || (x != cur_state.curx || y != cur_state.cury)) {
		cur_state.curx = x;
		cur_state.cury = y;
		x -= cur_state.worgx;
		y -= cur_state.worgy;
		x /= cur_state.dscale;
		y /= cur_state.dscale;
		y = wmaxy - y;
		if (x != mousex || y != mousey) {
			XCondWarpMouse(w, x, y, w, mousex, mousey, 1, 1);
/*			XWarpMouse(w, x, y);			*/
			XFlush();
		}
	}
	return(0);
}

/* set up display adaptor for graphics and initialize keyboard */
char *gentry(argc,argv)
	int argc;
	char **argv;
  {	char *prog;
	char *fname = NULL;
	register struct icmd *p;
	register char *q;
	WindowInfo winfo;
	FontInfo finfo;
	char *fore_color, *back_color, *brdr_color, *font_name;
	char *geometry = NULL;
	char *display = NULL;
	int reverse = 0;
	Color cdef;
	char def[32];
	OpaqueFrame frame;

	/* fill in command dispatch table */
	for (p = cmds; p->keys != NULL; p++)
	  for (q = p->keys; *q; q++) dispatch[*q & 0xFF] = p->func;

	prog = *argv;
	if ((q = XGetDefault(prog, "ReverseVideo")) &&
	    strcmp(q, "on") == 0)
		reverse = 1;
	frame.bdrwidth = 2;
	if (q = XGetDefault(prog, "BorderWidth"))
		frame.bdrwidth = atoi(q);
	fore_color = XGetDefault(prog, "ForeGround");
	back_color = XGetDefault(prog, "BackGround");
	brdr_color = XGetDefault(prog, "Border");
	font_name = XGetDefault(prog, "BodyFont");
	for (argv++; --argc; argv++) {
	    if (!strcmp(*argv, "-rv")) {
		reverse = 1;
	    } else if (!strcmp(*argv, "-fg") && argc) {
		argv++;
		argc--;
		fore_color = *argv;
	    } else if (!strcmp(*argv, "-bg") && argc) {
		argv++;
		argc--;
		back_color = *argv;
	    } else if (!strcmp(*argv, "-bd") && argc) {
		argv++;
		argc--;
		brdr_color = *argv;
	    } else if (!strcmp(*argv, "-fn") && argc) {
		argv++;
		argc--;
		font_name = *argv;
	    } else if (**argv == '=') {
		geometry = *argv;
	    } else if (index(*argv, ':')) {
		display = *argv;
	    } else if (**argv == '-' || fname != NULL) {
		printf("usage: xgedit [-rv] [-fn <font>] [-fg <color>] [-bg <color>] [-bd <color>] [=<geometry>] [host:display] [file]\n");
		exit(1);
	    } else {
		fname = *argv;
	    }
	}
	if (!XOpenDisplay(display)) {
	    fprintf(stderr, "%s: Can't open display '%s'\n",
		    prog, XDisplayName(display));
		exit(1);
	}
	if (reverse) {
		forepix = WhitePixel;
		backpix = BlackPixel;
		frame.background = BlackPixmap;
		frame.border = WhitePixmap;
	} else {
		forepix = BlackPixel;
		backpix = WhitePixel;
		frame.background = WhitePixmap;
		frame.border = BlackPixmap;
	}
	if (DisplayCells() > 2) {
		if (fore_color && XParseColor(fore_color, &cdef) &&
			XGetHardwareColor(&cdef))
			forepix = cdef.pixel;
		if (back_color && XParseColor(back_color, &cdef) &&
			XGetHardwareColor(&cdef))
			frame.background = XMakeTile(backpix = cdef.pixel);
		if (brdr_color && XParseColor(brdr_color, &cdef) &&
			XGetHardwareColor(&cdef))
			frame.border = XMakeTile(cdef.pixel);
	}
	invplanes = forepix ^ backpix;
	if (font_name == NULL) font_name = "8x13";
	font = XGetFont(font_name);
	if (!font) {
		fprintf(stderr, "no such font: %s\n", font_name);
		exit(1);
	}
	XQueryFont(font, &finfo);
	XQueryWindow(RootWindow, &winfo);
	chrwid = finfo.width;
	chrhgt = finfo.height;
	frame.x = 1;
	frame.y = 1;
	frame.width = 80 * chrwid;
	if (frame.width + (frame.bdrwidth<<1) > winfo.width)
		frame.width = winfo.width - (frame.bdrwidth<<1);
	frame.height = 25 * chrhgt;
	if (frame.height + (frame.bdrwidth<<1) > winfo.height)
		frame.height = winfo.height - (frame.bdrwidth<<1);
	sprintf(def, "=%dx%d+1+1", frame.width, frame.height);
	wminx = 0;
	wminy = 2*chrhgt;
	w = XCreate(prog, prog, geometry, def, &frame, 5 * chrwid, wminy + 5 * chrhgt);
	wmaxx = frame.width;
	wmaxy = frame.height;
	trw = XCreateTransparency(w, 0, 0, wmaxx, wmaxy - (chrhgt << 1));
	XMapWindow(trw);
	XDefineCursor(w, XCreateCursor(16, 16, crbits, crbits, 7, 7,
					backpix, forepix, GXnoop));
	XSetResizeHint(w, 1, wminy + 1, 1, 1);
	XMapWindow(w);
	XSelectInput(w, KeyPressed|ButtonPressed|MouseMoved|ExposeRegion);
	mousechanged = 0;
	return(fname);
}

Beep()
 {	XFeep(0);
}

/* give status info on second to last line */
banner()
  {	char temp[100];

	if (cur_state.curobj != NULL)
	  sprintf(temp,"XGEDIT %d:%d    %c %s",
		  cur_state.mscale,cur_state.dscale,
		  cur_state.curobj->modified ? '*':' ',
		  cur_state.curobj->name);
	else sprintf(temp,"XGEDIT %d:%d",
		  cur_state.mscale,cur_state.dscale);

	XPixSet(w, 0, wmaxy - (chrhgt << 1), wmaxx, chrhgt, backpix);

	disp_str(temp, 0, chrhgt, 1, HIGHLIGHT, 1);
}

/* display a string at the specified position, return lenth */
disp_str(s,x,y,reverse,xorflag,info)
	char *s;
  {	int len;
	len = strlen(s);
	if (reverse)
		XText(info ? w : trw, x, wmaxy - y - chrhgt, s, len, font,
			 backpix, forepix);
	else
		XTextMaskPad(info ? w : trw, x, wmaxy - y - chrhgt, s, len, font,
			     0, 0, forepix, xorflag ? GXinvert : GXcopy,
			     xorflag ? invplanes: AllPlanes);
	return(len);
}

/* display a character at the specified position */
disp_char(ch,x,y,reverse,xorflag,info)
  {	char c = ch;
	if (reverse)
		XText(info ? w : trw, x, wmaxy - y - chrhgt, &c, 1, font,
			 backpix, forepix);
	else
		XTextMaskPad(info ? w : trw, x, wmaxy - y - chrhgt, &c, 1, font,
			     0, 0, forepix, xorflag ? GXinvert : GXcopy,
			     xorflag ? invplanes : AllPlanes);
}

/* Update display, cursor */
DpyUp(tflag)
 {	XEvent ev;

	if (tflag) tcurxor();
	dcurxor();
	do {
		XPeekEvent(&ev);
	} while (UserReady() == 0);
	if (tflag) tcurxor();
	dcurxor();
}

/* xor the text cursor */
tcurxor()
  {	XPixFill(w, incol*chrwid, wmaxy - chrhgt, chrwid, chrhgt, forepix, (Bitmap) NULL,
		 GXinvert, invplanes);
}

clearprompt()
  {	incol = 0;
	XPixSet(w, 0, wmaxy - chrhgt, wmaxx, chrhgt, backpix);
}

clearscreen()
  {	XClear(w);
	banner();
	clearprompt();
}

static int lastx, lasty;
static int lastflag;
static int lastcnt = 1000;

/* draw a vector from <FromX,FromY> to <ToX,ToY> */
line(FromX,FromY,ToX,ToY,xorflag)
  {	Vertex vert[2];
	vert[0].x = FromX;
	vert[0].y = wmaxy - FromY;
	vert[0].flags = VertexDontDraw;
	vert[1].x = ToX;
	vert[1].y = wmaxy - ToY;
	vert[1].flags = VertexDrawLastPoint;
	if (xorflag == lastflag && lastcnt < 250 &&
	    ((FromX == lastx && FromY == lasty &&
		XAppendVertex(&vert[1], 1) > 0)
		|| XAppendVertex(vert, 2) > 0)) {
		lastx = ToX;
		lasty = ToY;
		lastcnt++;
		return;
	}
	lastx = ToX;
	lasty = ToY;
	lastflag = xorflag;
	lastcnt = 1;
	XDraw(trw, vert, 2, 1, 1, forepix, xorflag ? GXinvert : GXcopy,
		xorflag ? invplanes : AllPlanes);
}
