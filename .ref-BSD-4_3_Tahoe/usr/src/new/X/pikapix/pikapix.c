#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

#ifndef lint
static char *rcsid_pikapix_c = "$Header: pikapix.c,v 10.5 86/02/01 16:11:15 tony Rel $";
#endif	lint

#include <X/Xlib.h>
#include <stdio.h>
#include <strings.h>
#include "../cursors/target.cursor"
#include "../cursors/target_mask.cursor"
#include <sys/types.h>

#define pik_width 15
#define pik_height 15
static short pik_bits[] = {
   0x8080, 0x8080, 0x81c0, 0x83e0,
   0x87f0, 0x8ff8, 0x9ffc, 0xbffe,
   0xbffe, 0xbffe, 0xbffe, 0xbffe,
   0xbffe, 0xbffe, 0x8000};
static short pik_mask_bits[] = {
   0x8080, 0x81c0, 0x83e0, 0x87f0,
   0x8ff8, 0x9ffc, 0xbffe, 0xffff,
   0xffff, 0xffff, 0xffff, 0xffff,
   0xffff, 0xffff, 0xffff};

#define MIN(a,b) (((a) < (b)) ? (a) : (b))

#define MAXIDX 200
#define PROMPT "#ffffff -> "
#define ASKSIZE (sizeof(PROMPT) + 25)

#define RGBSIDE 50
#define RGBWIDTH (RGBSIDE * 3)
#define RGBHEIGHT (RGBSIDE * 2)

#define BASEDELTA (1<<8)

char *malloc();

Window win;
u_char *buf;
u_char transbuf[256];
int width, height;
int pixels[5];
Color cdefs[2];
Color origdefs[256];
WindowInfo rinfo;
Font font;
Color rgbdefs[3];
Window rgb, redw, greenw, bluew;
int rgbx, rgby;
int shared = 0;

main(argc, argv)
int argc;
char **argv;
{
    Cursor cursor;
    XEvent ev;
    XExposeEvent *exp = (XExposeEvent *) &ev;
    XMouseMovedEvent *mot = (XMouseMovedEvent *) &ev;
    register XButtonPressedEvent *but = (XButtonPressedEvent *) &ev;
    WindowInfo winfo;
    Window ask;
    int asking = 0;
    int askwidth;
    int askheight;
    char colorbuf[MAXIDX + sizeof(PROMPT) + 1];
    register char *colorname = &colorbuf[sizeof(PROMPT)];
    int coloridx = 0;
    int mixing = 0;
    int mixtrack = 0;
    char *string;
    int nbytes;
    int tpix, cnt;
    unsigned char pixel;
    Color cdef;
    FontInfo finfo;
    int xoff, yoff;
    char *display = NULL;

    for (cnt = 1; cnt < argc; cnt++) {
	if (!*argv[cnt]) continue;
	if (strcmp(argv[cnt], "-s") == 0)
	    shared = 1;
	else if (index(argv[cnt], ':'))
	    display = argv[cnt];
	else {
	    fprintf(stderr, "usage: pikapix [-s] [host:display]\n");
	    (void) fflush(stderr);
	    exit(1);
	}
    }
    if (!XOpenDisplay(display)) {
	perror("pikapix");
	exit(1);
    }
    if (DisplayPlanes() < 2 || DisplayPlanes() > 8)
	Error("only works on displays with 2 to 8 planes");
    cursor = XCreateCursor(target_width, target_height,
			   target_bits, target_mask_bits,
			   8, 8, BlackPixel, WhitePixel, GXcopy);
    while (!XGrabMouse(RootWindow, cursor, ButtonPressed))
	sleep(1);
    XNextEvent(&ev);
    XUngrabMouse();
    win = but->subwindow;
    if (!win)
	Error("no window selected");
    xoff = 0;
    yoff = 0;
    switch (but->detail & ValueMask) {
    case LeftButton:
    case MiddleButton:
	do {
	    XInterpretLocator(win, &but->x, &but->y, &but->subwindow,
			      but->location);
	    if (!but->subwindow)
		break;
	    if (!XQueryWindow(win, &winfo))
		Error("window disappeared");
	    xoff += winfo.x + winfo.bdrwidth;
	    yoff += winfo.y + winfo.bdrwidth;
	    but->window = win;
	    win = but->subwindow;
	} while ((but->detail & ValueMask) == LeftButton);
    }
    XFreeCursor(cursor);
    if (!XGetColorCells(0, 5, 0, &cnt, pixels))
	Error("not enough color map entries");
    cdefs[0].pixel = pixels[0];
    cdefs[0].red = cdefs[0].green = cdefs[0].blue = ~0;
    cdefs[1].pixel = pixels[1];    
    cdefs[1].red = cdefs[1].green = cdefs[1].blue = 0;
    XStoreColors(2, cdefs);
    cursor = XCreateCursor(pik_width, pik_height,
			   pik_bits, pik_mask_bits,
			   7, 0, pixels[0], pixels[1], GXcopy);
    if (!XQueryWindow(win, &winfo))
	Error("window disappeared");
    width = winfo.width + (winfo.bdrwidth << 1);
    height = winfo.height + (winfo.bdrwidth << 1);
    nbytes = BZPixmapSize(width, height);
    buf = (u_char *) malloc(nbytes);
    if (buf == NULL)
	Error("window too large");
    XPixmapGetZ(but->window, winfo.x, winfo.y, width, height, buf);
    if (!shared) {
	for (cnt = 256; --cnt >= 0; )
	    transbuf[cnt] = cnt;
	while (--nbytes >= 0) {
	    pixel = buf[nbytes];
	    if (transbuf[pixel] != pixel)
		continue;
	    cdef.pixel = pixel;
	    XQueryColor(&cdef);
	    if (!XGetColorCells(0, 1, 0, &cnt, &cdef.pixel))
		Error("not enough color map entries, try -s option");
	    transbuf[pixel] = cdef.pixel;
	    origdefs[cdef.pixel] = cdef;
	    origdefs[pixel] = cdef;
	    origdefs[pixel].pixel = pixel;
	    XStoreColors(1, &cdef);
	}
    } else {
	transbuf[0] = 1;
	while (--nbytes >= 0) {
	    pixel = buf[nbytes];
	    if (transbuf[pixel] == pixel)
		continue;
	    cdef.pixel = pixel;
	    XQueryColor(&cdef);
	    transbuf[pixel] = pixel;
	    origdefs[pixel] = cdef;
	}
    }
    win = XCreateWindow(RootWindow, xoff + winfo.x, yoff + winfo.y,
			width, height, 0, (Pixmap) 0, (Pixmap) 0);
    XStoreName(win, "pikapix");
    XDefineCursor(win, cursor);
    XSelectInput(win, KeyPressed|ButtonPressed|ExposeRegion|MouseMoved);
    XMapWindow(win);
    font = XGetFont("6x10");
    if (!font) Error("couldn't open font");
    XQueryFont(font, &finfo);
    askwidth = finfo.width * ASKSIZE + 2;
    askheight = finfo.height + 2;
    ask = XCreateWindow(RootWindow, 0, 0, askwidth, askheight,
			2, BlackPixmap, WhitePixmap);
    askwidth += 4;
    askheight += 4;
    XSelectInput(ask, KeyPressed|ExposeWindow|ButtonPressed);
    XDefineCursor(ask, cursor);
    rgb = XCreateWindow(RootWindow, 0, 0, RGBWIDTH, RGBHEIGHT,
			2, XMakeTile(pixels[1]), XMakeTile(pixels[0]));
    redw = XCreateWindow(rgb, 0, 0, RGBSIDE, RGBSIDE,
			 0, (Pixmap) 0, XMakeTile(pixels[2]));
    rgbdefs[0].pixel = pixels[2];
    greenw = XCreateWindow(rgb, RGBSIDE, 0, RGBSIDE, RGBSIDE,
			   0, (Pixmap) 0, XMakeTile(pixels[3]));
    rgbdefs[1].pixel = pixels[3];
    bluew = XCreateWindow(rgb, RGBSIDE * 2, 0, RGBSIDE, RGBSIDE,
			  0, (Pixmap) 0, XMakeTile(pixels[4]));
    rgbdefs[2].pixel = pixels[4];
    rgbx = (RGBWIDTH - (finfo.width * 6)) >> 1;
    rgby = RGBSIDE + ((RGBSIDE - finfo.height) >> 1);
    XMapSubwindows(rgb);
    XSelectInput(rgb, ButtonPressed|ExposeWindow);
    XSelectInput(redw, ButtonPressed|ButtonReleased|MiddleDownMotion);
    XSelectInput(greenw, ButtonPressed|ButtonReleased|MiddleDownMotion);
    XSelectInput(bluew, ButtonPressed|ButtonReleased|MiddleDownMotion);
    XDefineCursor(rgb, cursor);
    XQueryWindow(RootWindow, &rinfo);
    RedoCursor(1);
    while (1) {
	XNextEvent(&ev);
	switch ((int)ev.type) {
	case MouseMoved:
	    if (asking || (mixing && !mixtrack))
		break;
	    if (mixtrack) {
		XUpdateMouse(mot->window, &xoff, &yoff, &mot->subwindow);
		CalcRGB(mot->window, pixel, yoff);
	    } else
		RedoCursor(0);
	    break;
	case KeyPressed:
	    string = XLookupMapping (&ev, &nbytes);
	    if (nbytes == 1 && (*string == '\003' || *string == '\004')) {
		if (asking) {
		    coloridx = 0;
		    asking = 0;
		    XUnmapWindow(ask);
		    RedoCursor(0);
		} else if (mixing) {
		    mixing = 0;
		    XUnmapWindow(rgb);
		    RedoCursor(1);
		} else
		    exit(0);
	    } else if (asking && ev.window == ask) {
		while (--nbytes >= 0)
		    colorname[coloridx++] = *string++;
		if (colorname[coloridx - 1] == '\r') {
			coloridx--;
			asking = 0;
		} else if (colorname[coloridx - 1] == '\177') {
		    if (--coloridx) {
			--coloridx;
			XClear(ask);
		    }
		} else if (colorname[coloridx - 1] == '\025') {
		    if (--coloridx)
			XClear(ask);
		    coloridx = 0;
		}
		XText(ask, 1, 1, colorbuf, sizeof (PROMPT) + coloridx, font,
			BlackPixel, WhitePixel);
		if (!asking) {
		    XUnmapWindow(ask);
		    if (coloridx) {
			colorname[coloridx] = '\0';
			if (XParseColor(colorname, &cdef)) {
			    if (!shared) {
				cdef.pixel = transbuf[pixel];
				XStoreColors(1, &cdef);
				origdefs[cdef.pixel] = cdef;
				RedoCursor(1);
			    } else if (XGetHardwareColor(&cdef)) {
				if (transbuf[pixel] != pixel) {
				    tpix = transbuf[pixel];
				    XFreeColors(&tpix, 1, 0);
				}
				transbuf[pixel] = cdef.pixel;
				origdefs[cdef.pixel] = cdef;
				RedoCursor(1);
				BitsPut(0, 0, width, height);
			    } else XFeep(0);
			} else
			    XFeep(0);
			coloridx = 0;
		    }
		}
	    }
	    break;
	case ButtonPressed:
	    if (asking) {
		coloridx = 0;
		asking = 0;
		XUnmapWindow(ask);
		RedoCursor(1);
		break;
	    }
	    if (mixing && but->window == rgb) {
		switch (but->detail & ValueMask) {
		case LeftButton:
		case MiddleButton:
		    if ((but->detail & ValueMask) == LeftButton)
			cdefs[0] = origdefs[pixel];
		    else
			cdefs[0] = cdef;
		    if (!shared) {
			cdefs[0].pixel = transbuf[pixel];
			origdefs[cdefs[0].pixel] = cdefs[0];
			XStoreColors(1, &cdefs[0]);
		    }
		    cdefs[0].pixel = pixels[0];
		    rgbdefs[0].red = cdefs[0].red;
		    rgbdefs[1].green = cdefs[0].green;
		    rgbdefs[2].blue = cdefs[0].blue;
		    XStoreColors(3, rgbdefs);
		    ResetCursor();
		    PrintRGB();
		    break;
		case RightButton:
		    XUnmapWindow(rgb);
		    if (cdef.red != cdefs[0].red ||
			cdef.green != cdefs[0].green ||
			cdef.blue != cdefs[0].blue) {
			cdef = cdefs[0];
			if (!shared) {
			    cdef.pixel = transbuf[pixel];
			    origdefs[cdef.pixel] = cdef;
			    XStoreColors(1, &cdef);
			} else if (XGetHardwareColor(&cdef)) {
			    if (transbuf[pixel] != pixel) {
				tpix = transbuf[pixel];
				XFreeColors(&tpix, 1, 0);
			    }
			    transbuf[pixel] = cdef.pixel;
			    origdefs[cdef.pixel] = cdef;
			    RedoCursor(1);
			    BitsPut(0, 0, width, height);
			} else
			    XFeep(0);
		    }
		    RedoCursor(1);
		    mixing = 0;
		    mixtrack = 0;
		    break;
		}
		break;
	    }
	    if (mixing && but->window == win)
		break;
	    if (mixing) {
		switch (but->detail & ValueMask) {
		case MiddleButton:
		    mixtrack = 1;
		    CalcRGB(but->window, pixel, but->y);
		    break;
		case LeftButton:
		    UpdateRGB(but->window, pixel, -BASEDELTA);
		    break;
		case RightButton:
		    UpdateRGB(but->window, pixel, BASEDELTA);
		    break;
		}
		break;
	    }
	    pixel = buf[BZPixmapSize(width, but->y) + BZPixmapSize(but->x, 1)];
	    switch (but->detail & ValueMask) {
	    case LeftButton:
		if (!shared) {
		    cdef = origdefs[pixel];
		    cdef.pixel = transbuf[pixel];
		    origdefs[cdef.pixel] = cdef;
		    XStoreColors(1, &cdef);
		    RedoCursor(1);
		} else if (transbuf[pixel] != pixel) {
		    tpix = transbuf[pixel];
		    XFreeColors(&tpix, 1, 0);
		    transbuf[pixel] = pixel;
		    RedoCursor(1);
		    BitsPut(0, 0, width, height);
		}
		break;
	    case MiddleButton:
		PopWindow(ask, but, askwidth, askheight);
		(void)sprintf(colorbuf, "#%02x%02x%02x -> ", cdefs[0].red >> 8,
			cdefs[0].green >> 8, cdefs[0].blue >> 8);
		asking = 1;
		break;
	    case RightButton:
		mixing = 1;
		cdef.red = rgbdefs[0].red = cdefs[0].red;
		cdef.green = rgbdefs[1].green = cdefs[0].green;
		cdef.blue = rgbdefs[2].blue = cdefs[0].blue;
		XStoreColors(3, rgbdefs);
		PopWindow(rgb, but, RGBWIDTH+4, RGBHEIGHT+4);
		break;
	    }
	    break;
	case ButtonReleased:
	    if (mixtrack && ((but->detail & ValueMask) == MiddleButton)) {
		CalcRGB(but->window, pixel, but->y);
		mixtrack = 0;
	    }
	    break;
	case ExposeWindow:
	case ExposeRegion:
	    if (exp->window == ask) {
		XText(ask, 1, 1, colorbuf, sizeof (PROMPT) + coloridx, font,
			BlackPixel, WhitePixel);
	    } else if (exp->window == rgb) {
		if (exp->subwindow == NULL ) PrintRGB();
	    } else {
		exp->width = MIN(exp->x + exp->width, width) - exp->x;
		exp->height = MIN(exp->y + exp->height, height) - exp->y;
		if (exp->width > 0 && exp->height > 0)
		    BitsPut(exp->x, exp->y, exp->width, exp->height);
	    }
	    break;
	}
    }
}

PopWindow (pop, but, w, h)
	Window pop;
	XButtonPressedEvent *but;
	int w, h;
{
	int x, y;

	x = ((but->location >> 16) & 0xffff) - (w >> 1);
	if (x < 0)
	    x = 0;
	else if (x + w > rinfo.width)
	    x = rinfo.width - w;
	y = (but->location & 0xffff) - (h >> 1) - 3;
	if (y < 0)
	    y = 0;
	else if (y + h > rinfo.height)
	    y = rinfo.height - h;
	XMoveWindow(pop, x, y);
	XMapWindow(pop);
}

RedoCursor (force)
    int force;
{
    int x, y;
    Window sub;
    unsigned char pixel;
    static unsigned short curspix = ~0;

    XUpdateMouse(win, &x, &y, &sub);
    if (x < 0 || x >= width || y < 0 || y >= height)
	return;
    pixel = transbuf[buf[BZPixmapSize(width, y) + BZPixmapSize(x, 1)]];
    if (!force && (pixel == curspix))
	return;
    curspix = pixel;
    cdefs[0] = origdefs[pixel];
    cdefs[0].pixel = pixels[0];
    ResetCursor();
}

ResetCursor()
{
	if (cdefs[0].red <= 0x8000 &&
	    cdefs[0].green <= 0x8000 &&
	    cdefs[0].blue <= 0x8000)
	    cdefs[1].red = cdefs[1].green = cdefs[1].blue = ~0;
	else
	    cdefs[1].red = cdefs[1].green = cdefs[1].blue = 0;
	XStoreColors(2, cdefs);
}

UpdateRGB (ew, pixel, value)
	Window ew;
	unsigned pixel;
	int value;
{
	Color cdef;

	if (ew == redw) {
	    value += rgbdefs[0].red;
	    if (value < 0)
		value = 0;
	    else if (value > 0xffff)
		value = 0xffff;
	    rgbdefs[0].red = value;
	    XStoreColors(1, &rgbdefs[0]);
	} else if (ew == greenw) {
	    value += rgbdefs[1].green;
	    if (value < 0)
		value = 0;
	    else if (value > 0xffff)
		value = 0xffff;
	    rgbdefs[1].green = value;
	    XStoreColors(1, &rgbdefs[1]);
	} else if (ew == bluew) {
	    value += rgbdefs[2].blue;
	    if (value < 0)
		value = 0;
	    else if (value > 0xffff)
		value = 0xffff;
	    rgbdefs[2].blue = value;
	    XStoreColors(1, &rgbdefs[2]);
	} else
	    return;
	cdefs[0].red = rgbdefs[0].red;
	cdefs[0].green = rgbdefs[1].green;
	cdefs[0].blue = rgbdefs[2].blue;
	ResetCursor();
	if (!shared) {
	    cdef = cdefs[0];
	    cdef.pixel = transbuf[pixel];
	    origdefs[cdef.pixel] = cdef;
	    XStoreColors(1, &cdef);
	}
	PrintRGB();
}

CalcRGB (ew, pixel, value)
	Window ew;
	unsigned pixel;
	int value;
{
	Color cdef;

	if (value < 0)
	    value = 0;
	else if (value >= RGBSIDE)
	    value = (RGBSIDE - 1);
	value = (0xffff * value) / (RGBSIDE - 1);
	if (ew == redw) {
	    rgbdefs[0].red = value;
	    XStoreColors(1, &rgbdefs[0]);
	} else if (ew == greenw) {
	    rgbdefs[1].green = value;
	    XStoreColors(1, &rgbdefs[1]);
	} else if (ew == bluew) {
	    rgbdefs[2].blue = value;
	    XStoreColors(1, &rgbdefs[2]);
	} else
	    return;
	cdefs[0].red = rgbdefs[0].red;
	cdefs[0].green = rgbdefs[1].green;
	cdefs[0].blue = rgbdefs[2].blue;
	ResetCursor();
	if (!shared) {
	    cdef = cdefs[0];
	    cdef.pixel = transbuf[pixel];
	    origdefs[cdef.pixel] = cdef;
	    XStoreColors(1, &cdef);
	}
	PrintRGB();
}

PrintRGB ()
{
	char rgbbuf[7];

	(void) sprintf(rgbbuf, "%02x%02x%02x", cdefs[0].red >> 8,
		cdefs[0].green >> 8, cdefs[0].blue >> 8);
	XText(rgb, rgbx, rgby, rgbbuf, 6, font, BlackPixel, WhitePixel);
}

#define CHUNKSIZE 2048
u_char outbuf[CHUNKSIZE];

BitsPut (x, y, w, h)
	int x, y, w, h;
{
	register u_char *data, *ptr, *trans;
	register int i, j;
	int per, delta, linesize;

	trans = transbuf;
	linesize = BZPixmapSize(width, 1);
	data = &buf[y * linesize + BZPixmapSize(x, 1)];

	per = BZPixmapSize(w, 1);
	delta = CHUNKSIZE / per;
	linesize -= per;

	while (h) {
	    if (h < delta)
		delta = h;
	    for (ptr = outbuf, i = delta; --i >= 0; data += linesize) {
		for (j = per; --j >= 0; )
		    *ptr++ = trans[*data++];
	    }
	    XPixmapBitsPutZ(win, x, y, w, delta, outbuf,
			    NULL, GXcopy, AllPlanes);
	    y += delta;
	    h -= delta;
	}
}

Error (why)
	char *why;
{
	fprintf(stderr, "pikapix: %s\n", why);
	(void) fflush(stderr);
	exit(1);
}
