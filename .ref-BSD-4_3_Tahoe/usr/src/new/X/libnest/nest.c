#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1986 */

#include <sys/types.h>
#include "../Xlib/Xlib.h"
#undef CURSOR
#include "../Xlib/Xkeyboard.h"
#include <stdio.h>
#include <errno.h>
#include "../X/vsinput.h"
#include "../X/Xdev.h"

extern int errno, XFlush();
char *Xalloc(), *Xrealloc(), *strcpy();

#define Xpixmap(p) ((Pixmap) p->data)
#define Xbitmap(b) ((Bitmap) b->data)
#define Xfinf(f) ((FontInfo *) f->data)
#define Xfont(f) (Xfinf(f)->id)
#define Xcursor(c) ((Cursor) c->data)
#define npix(p) (((p) << pixshift) | basepix)
#define opix(p) (((p) &~ basepix) >> pixshift)
#define nmask(m) ((m) << pixshift)

static Window w, t, ot, tt;
static int extrapix;
static int pixplanes = 0;
static int pixshift = 0;
static int basepix = 0;
static int screen_height, screen_width;
static int tilex = 0;
static int tiley = 0;
static CLIP tileclip = {0, 0, 0, 0};
static vsCursor mouse = {0, 0};
static vsBox mbox = {0, 0, 0, 0};
static int cursor_x, cursor_y;
static DEVICE *dev;

/*ARGSUSED*/
ProcessInput (ev)
	vsEvent ev;
{
}

/*ARGSUSED*/
OpenDisplay (display)
	char *display;			/* display number */
{
	OpaqueFrame frame;
	char def[32];
	char *option;
	Color cdef;

	if (XOpenDisplay((char *) NULL) == NULL) {
		errno = EINVAL;
		return(-1);
	}
	if (option = XGetDefault("X", "BorderWidth"))
	    frame.bdrwidth = atoi(option);
	else
	    frame.bdrwidth = 0;
	if (frame.bdrwidth && DisplayCells() > 2 &&
	    (option = XGetDefault("X", "Border")) &&
	    XParseColor(option, &cdef) &&
	    XGetHardwareColor(&cdef))
	    frame.border = XMakeTile(cdef.pixel);
	else
	    frame.border = BlackPixmap;
	frame.background = NULL;
	frame.x = 0;
	frame.y = 0;
	sprintf(def, "=%dx%d+0+0",
		DisplayWidth() - (frame.bdrwidth << 1),
		DisplayHeight() - (frame.bdrwidth << 1));
	w = XCreate("X", "X", "", def, &frame, 50, 50);
	screen_height = frame.height;
	screen_width = frame.width;
	XMapWindow(w);
	XSelectInput(w, KeyPressed|KeyReleased|ButtonPressed|ButtonReleased|
			MouseMoved|EnterWindow|LeaveWindow);
	t = XCreateTransparency(w, 0, 0, screen_width, screen_height);
	ot = XCreateTransparency(w, 0, 0, screen_width, screen_height);
	XTileAbsolute(ot);
	tt = XCreateTransparency(ot, 0, 0, screen_width, screen_height);
	XMapWindow(t);
	XMapWindow(tt);
	XMapWindow(ot);
	return(dpyno());
}

InputReader ()
{
	XEvent xev;
	vsEvent ev;
	int nstate;
	static int state = 0;
	vsCursor ms;

	XPending();
	while (QLength()) {
	    XNextEvent(&xev);
	    switch (xev.type) {
	    case EnterWindow:
		if (xev.subwindow)
		    continue;
	    case MouseMoved:
		ms.x = ((XMouseMovedEvent *) (&xev))->x - cursor_x;
		ms.y = ((XMouseMovedEvent *) (&xev))->y - cursor_y;
		if ((ms.x != mouse.x || ms.y != mouse.y) &&
		    ms.x >= 0 && ms.x < screen_width &&
		    ms.y >= 0 && ms.y < screen_height) {
		    mouse = ms;
		    if (ms.y >= mbox.bottom || ms.y < mbox.top ||
			ms.x >= mbox.right || ms.x < mbox.left) {
			mbox.bottom = 0;
			Deal_with_movement();
		    }
		}
		if (xev.type != EnterWindow)
		    continue;
		ev.vse_time = ((XKeyOrButtonEvent *) (&xev))->time;
		nstate = ((XEnterWindowEvent *) (&xev))->detail & ~ValueMask;
		nstate ^= state;
		ev.vse_direction = VSE_KBTUP;
		ButtonTransition(&ev, nstate & state);
		KeyTransition(&ev, nstate & state);
		ev.vse_direction = VSE_KBTDOWN;
		KeyTransition(&ev, nstate & ~state);
		ButtonTransition(&ev, nstate & ~state);
		continue;
	    case LeaveWindow:
		state = ((XLeaveWindowEvent *) (&xev))->detail & ~ValueMask;
		continue;
	    }
	    ev.vse_x = mouse.x;
	    ev.vse_y = mouse.y;
	    ev.vse_time = ((XKeyOrButtonEvent *) (&xev))->time;
	    ev.vse_key = ((XKeyOrButtonEvent *) (&xev))->detail & ValueMask;
	    switch (xev.type) {
	    case KeyPressed:
		ev.vse_device = VSE_DKB;
		ev.vse_direction = VSE_KBTDOWN;
		break;
	    case KeyReleased:
		ev.vse_device = VSE_DKB;
		ev.vse_direction = VSE_KBTUP;
		break;
	    case ButtonPressed:
		ev.vse_device = VSE_MOUSE;
		ev.vse_direction = VSE_KBTDOWN;
		ev.vse_key = 2 - ev.vse_key;
		break;
	    case ButtonReleased:
		ev.vse_device = VSE_MOUSE;
		ev.vse_direction = VSE_KBTUP;
		ev.vse_key = 2 - ev.vse_key;
		break;
	    }
	    Deal_with_input(&ev);
	}
}

KeyTransition (ev, bits)
	register vsEvent *ev;
	register int bits;
{
	ev->vse_device = VSE_DKB;
	if (bits & ControlMask) {
	    ev->vse_x = mouse.x;
	    ev->vse_y = mouse.y;
	    ev->vse_key = KC_CTRL;
	    Deal_with_input(ev);
	}
	if (bits & MetaMask) {
	    ev->vse_x = mouse.x;
	    ev->vse_y = mouse.y;
	    ev->vse_key = KC_META;
	    Deal_with_input(ev);
	}
	if (bits & ShiftMask) {
	    ev->vse_x = mouse.x;
	    ev->vse_y = mouse.y;
	    ev->vse_key = KC_SHIFT;
	    Deal_with_input(ev);
	}
	if (bits & ShiftLockMask) {
	    ev->vse_x = mouse.x;
	    ev->vse_y = mouse.y;
	    ev->vse_key = KC_LOCK;
	    Deal_with_input(ev);
	}
}

ButtonTransition (ev, bits)
	register vsEvent *ev;
	register int bits;
{
	ev->vse_device = VSE_MOUSE;
	if (bits & LeftMask) {
	    ev->vse_x = mouse.x;
	    ev->vse_y = mouse.y;
	    ev->vse_key = 2 - LeftButton;
	    Deal_with_input(ev);
	}
	if (bits & MiddleMask) {
	    ev->vse_x = mouse.x;
	    ev->vse_y = mouse.y;
	    ev->vse_key = 2 - MiddleButton;
	    Deal_with_input(ev);
	}
	if (bits & RightMask) {
	    ev->vse_x = mouse.x;
	    ev->vse_y = mouse.y;
	    ev->vse_key = 2 - RightButton;
	    Deal_with_input(ev);
	}
}

InitDisplay (info)
	DEVICE *info;			/* device data */
{
	char *option;
	int planes;
	static vsEventQueue queue = {NULL, 0, 0, 0};

	dev = info;
	info->id = XDEV_XNEST;
	info->width = screen_width;
	info->height = screen_height;
	info->planes = DisplayPlanes();
	info->entries = 0;
	pixplanes = 0;
	if (DisplayCells() > 2 &&
	    (option = XGetDefault("X", "CellExponent")) &&
	    (pixplanes = atoi(option)) &&
	    XGetColorCells(0, 1, 0, &planes, &extrapix)) {
	    while (pixplanes &&
		   !XGetColorCells(1, 1, pixplanes, &planes, &basepix))
		pixplanes--;
	    if (pixplanes) {
		info->entries = 1 << pixplanes;
		while (!(planes & 1)) {
		    pixshift++;
		    planes >>= 1;
		}
	    } else
		XFreeColors(&extrapix, 1, 0);
	}
	info->mouse = &mouse;
	info->mbox = &mbox;
	info->queue = &queue;
	Define_input_handler(InputReader);
	Define_block_handler(XFlush);
	return(0);
}

DisplayDead ()
{
	return(0);
}

InitMouse ()
{
}

caddr_t AllocateSpace (bytes)
	int bytes;			/* number of bytes to allocate */
{
	static char *buf = NULL;
	static int buflen = 0;

	if (bytes > buflen) {
	    free(buf);
	    buf = Xalloc((bytes + 0xff) &~ 0xff);
	}
	return(buf);
}

PixFill (srcpix, xymask, dstx, dsty, width, height, clips, clipcount,
	 func, zmask)
	int srcpix;			/* source pixel */
	BITMAP *xymask;			/* source mask or NULL */
	int dstx, dsty;			/* destination */
	int width, height;
	register CLIP *clips;		/* clipping rectangles */
	int clipcount;			/* count of rectangles */
	int func;			/* GX display function */
	int zmask;			/* plane mask */
{
	srcpix = npix(srcpix);
	zmask = nmask(zmask);
	for (; --clipcount >= 0; clips++) {
	    SetClip(clips);
	    XPixFill(t, dstx - clips->left, dsty - clips->top, width, height,
		     srcpix, xymask ? Xbitmap(xymask) : NULL, func, zmask);
	}
}

TileFill (tile, xoff, yoff, xymask, dstx, dsty, width, height,
	  clips, clipcount, func, zmask)
	PIXMAP *tile;			/* source tile */
	int xoff, yoff;			/* tile origin */
	BITMAP *xymask;			/* source mask or NULL */
	int dstx, dsty;			/* destination */
	int width, height;
	register CLIP *clips;		/* clipping rectangles */
	int clipcount;			/* count of rectangles */
	int func;			/* GX display function */
	int zmask;			/* plane mask */
{
	zmask = nmask(zmask);
	SetTileOrigin(tile, xoff, yoff);
	for (; --clipcount >= 0; clips++) {
	    SetTileClip(clips);
	    XTileFill(tt, dstx - clips->left, dsty - clips->top, width, height,
		      Xpixmap(tile), xymask ? Xbitmap(xymask) : NULL,
		      func, zmask);
	}
}

int StippleFill (srcpix, xoff, yoff, stipmask, dstx, dsty, width, height,
	clips, clipcount, func, zmask)
	int srcpix;		/* source pixel */
	int xoff, yoff;		/* stipple origin */
	BITMAP *stipmask;	/* stipple mask */
	int dstx, dsty;		/* destination */
	int width, height;
	CLIP *clips;		/* clipping rectangles */
	int clipcount;
	int func;		/* GX display function */
	int zmask;		/* plane mask */
{
	return (1);
}
PixmapPut (src, srcx, srcy, width, height, dstx, dsty, clips, clipcount,
	   func, zmask)
	PIXMAP *src;			/* source */
	int srcx, srcy;			/* region of source */
	int width, height;
	int dstx, dsty;			/* destination */
	register CLIP *clips;		/* clipping rectangles */
	int clipcount;			/* count of rectangles */
	int func;			/* GX display function */
	int zmask;			/* plane mask */
{
	zmask = nmask(zmask);
	for (; --clipcount >= 0; clips++) {
	    SetClip(clips);
	    XPixmapPut(t, srcx, srcy, dstx - clips->left, dsty - clips->top,
		       width, height, Xpixmap(src), func, zmask);
	}
}

PixmapBitsPut (width, height, format, data, xymask, dstx, dsty,
	       clips, clipcount, func, zmask)
	int width;			/* source width */
	int height;			/* source height */
	int format;			/* 0: XY-format, 1: Z-format */
	char *data;			/* source data */
	BITMAP *xymask;			/* source mask or NULL */
	int dstx, dsty;			/* destination */
	register CLIP *clips;		/* clipping rectangles */
	int clipcount;			/* count of rectangles */
	int func;			/* GX display function */
	int zmask;			/* plane mask */
{
	ToPixmap(width, height, format, data);
	zmask = nmask(zmask);
	for (; --clipcount >= 0; clips++) {
	    SetClip(clips);
	    if (format == XYFormat)
		XPixmapBitsPutXY(t, dstx - clips->left, dsty - clips->top,
				 width, height, data,
				 xymask ? Xbitmap(xymask) : NULL, func, zmask);
	    else {
		XPixmapBitsPutZ(t, dstx - clips->left, dsty - clips->top,
				 width, height, data,
				 xymask ? Xbitmap(xymask) : NULL, func, zmask);
	    }
	}
}

BitmapBitsPut (width, height, data, fore, back, xymask, dstx, dsty,
	       clips, clipcount, func, zmask)
	int width;			/* source width */
	int height;			/* source height */
	char *data;			/* source data */
	int fore;			/* foreground source pixel */
	int back;			/* background source pixel */
	BITMAP *xymask;			/* source mask or NULL */
	int dstx, dsty;			/* destination */
	register CLIP *clips;		/* clipping rectangles */
	int clipcount;			/* count of rectangles */
	int func;			/* GX display function */
	int zmask;			/* plane mask */
{
	fore = npix(fore);
	back = npix(back);
	zmask = nmask(zmask);
	for (; --clipcount >= 0; clips++) {
	    SetClip(clips);
	    XBitmapBitsPut(t, dstx - clips->left, dsty - clips->top,
			   width, height, data, fore, back,
			   xymask ? Xbitmap(xymask) : NULL, func, zmask);
	}
}

CopyArea (srcx, srcy, width, height, dstx, dsty, clips, clipcount, func, zmask)
	int srcx, srcy;			/* source */
	int width, height;
	int dstx, dsty;			/* destination */
	register CLIP *clips;		/* clipping rectangles */
	int clipcount;			/* count of rectangles */
	int func;			/* GX display function */
	int zmask;			/* plane mask */
{
	int dstr, dstb, left, top;

	dstr = dstx + width;
	dstb = dsty + height;
	srcx -= dstx;
	srcy -= dsty;
	for (; --clipcount >= 0; clips++) {
	    left = clips->left;
	    if (left < dstx)
		left = dstx;
	    width = clips->left + clips->width;
	    if (width > dstr)
		width = dstr;
	    width -= left;
	    top = clips->top;
	    if (top < dsty)
		top = dsty;
	    height = clips->top + clips->height;
	    if (height > dstb)
		height = dstb;
	    height -= top;
	    if (width > 0 && height > 0)
		XCopyArea(w, srcx + left, srcy + top, left, top,
			  width, height, func, zmask);
	}
}

PrintText (string, strlen, font, fore, back, charpad, spacepad, dstx, dsty,
	   clips, clipcount, func, zmask)
	char *string;			/* character string */
	int strlen;			/* string length */
	FONT *font;			/* source font */
	int fore;			/* foreground source pixel */
	int back;			/* background source pixel */
	int charpad;			/* inter-character pad */
	int spacepad;			/* space-character pad */
	int dstx, dsty;			/* destination */
	register CLIP *clips;		/* clipping rectangles */
	int clipcount;			/* count of rectangles */
	int func;			/* GX display function */
	int zmask;			/* plane mask */
{
	fore = npix(fore);
	back = npix(back);
	zmask = nmask(zmask);
	for (; --clipcount >= 0; clips++) {
	    SetClip(clips);
	    XTextPad(t, dstx - clips->left, dsty - clips->top, string, strlen,
		     Xfont(font), charpad, spacepad, fore, back, func, zmask);
	}
}

PrintTextMask (string, strlen, font, srcpix, charpad, spacepad, dstx, dsty,
	       clips, clipcount, func, zmask)
	char *string;			/* character string */
	int strlen;			/* string length */
	FONT *font;			/* font mask */
	int srcpix;			/* source pixel */
	int charpad;			/* inter-character pad */
	int spacepad;			/* space-character pad */
	int dstx, dsty;			/* destination */
	register CLIP *clips;		/* clipping rectangles */
	int clipcount;			/* count of rectangles */
	int func;			/* GX display function */
	int zmask;			/* plane mask */
{
	srcpix = npix(srcpix);
	zmask = nmask(zmask);
	for (; --clipcount >= 0; clips++) {
	    SetClip(clips);
	    XTextMaskPad(t, dstx - clips->left, dsty - clips->top, string,
			 strlen, Xfont(font), charpad, spacepad, srcpix,
			 func, zmask);
	}
}

DrawCurve (verts, vertcount, xbase, ybase, srcpix, altpix, mode,
	   bwidth, bheight, pat, patlen, patmul, clips, clipcount, func, zmask)
	Vertex *verts;			/* vertexes */
	int vertcount;			/* vertex count */
	int xbase, ybase;		/* draw origin */
	int srcpix;			/* source pixel *
	int altpix;			/* alternate source pixel */
	int mode;			/* 0: solid, 1: dashed, 2: patterned */
	int bwidth;			/* brush width */
	int bheight;			/* brush height */
	int pat;			/* pattern */
	int patlen;			/* pattern length */
	int patmul;			/* pattern repeat count */
	register CLIP *clips;		/* clipping rectangles */
	int clipcount;			/* count of rectangles */
	int func;			/* GX display function */
	int zmask;			/* plane mask */
{
	srcpix = npix(srcpix);
	altpix = npix(altpix);
	zmask = nmask(zmask);
	for (; --clipcount >= 0; clips++) {
	    SetClip(clips);
	    PathOffsets(verts, vertcount,
			xbase - clips->left, ybase - clips->top);
	    xbase = clips->left;
	    ybase = clips->top;
	    switch (mode) {
	    case DrawSolidLine:
		XDraw(t, verts, vertcount, bwidth, bheight, srcpix,
		      func, zmask);
		break;
	    case DrawDashedLine:
		XDrawDashed(t, verts, vertcount, bwidth, bheight, srcpix,
			    XMakePattern(pat, patlen, patmul), func, zmask);
		break;
	    case DrawPatternedLine:
		XDrawPatterned(t, verts, vertcount, bwidth, bheight, srcpix,
			       altpix, XMakePattern(pat, patlen, patmul),
			       func, zmask);
		break;
	    }
	}
}

DrawFilled (verts, vertcount, xbase, ybase, srcpix, tile, xoff, yoff,
	    clips, clipcount, func, zmask)
	Vertex *verts;			/* vertexes */
	int vertcount;			/* vertex count */
	int xbase, ybase;		/* draw origin */
	int srcpix;			/* source pixel */
	PIXMAP *tile;			/* optional tile or NULL */
	int xoff, yoff;			/* tile origin */
	register CLIP *clips;		/* clipping rectangles */
	int clipcount;			/* count of rectangles */
	int func;			/* GX display function */
	int zmask;			/* plane mask */
{
	if (tile)
	    SetTileOrigin(tile, xoff, yoff);
	else
	    srcpix = npix(srcpix);
	zmask = nmask(zmask);
	for (; --clipcount >= 0; clips++) {
	    PathOffsets(verts, vertcount,
			xbase - clips->left, ybase - clips->top);
	    xbase = clips->left;
	    ybase = clips->top;
	    if (tile) {
		SetTileClip(clips);
		XDrawTiled(tt, verts, vertcount, Xpixmap(tile), func, zmask);
	    } else {
	        SetClip(clips);
		XDrawFilled(t, verts, vertcount, srcpix, func, zmask);
	    }
	}
}

PathOffsets (verts, vertcount, xoff, yoff)
	register Vertex *verts;
	register int vertcount, xoff, yoff;
{
	for (; --vertcount >= 0; verts++) {
	    if (!(verts->flags & VertexRelative)) {
		verts->x += xoff;
		verts->y += yoff;
	    }
	}
}

SetClip (clip)
	register CLIP *clip;
{
	static CLIP normclip = {0, 0, 0, 0};

	if (clip->left != normclip.left || clip->top != normclip.top ||
	    clip->width != normclip.width || clip->height != normclip.height) {
	    XConfigureWindow(t, clip->left, clip->top,
			     clip->width, clip->height);
	    normclip = *clip;
	}
}

SetTileClip (clip)
	register CLIP *clip;
{
	if (clip->left != tileclip.left || clip->top != tileclip.top ||
	    clip->width != tileclip.width || clip->height != tileclip.height) {
	    XConfigureWindow(tt, clip->left - tilex, clip->top - tiley,
			     clip->width, clip->height);
	    tileclip = *clip;
	}
}

SetTileOrigin (tile, xoff, yoff)
	register PIXMAP *tile;
	register int xoff, yoff;
{
	if (tile->width) {
	    xoff %= tile->width;
	    if (xoff)
		xoff -= tile->width;
	    yoff %= tile->height;
	    if (yoff)
		yoff -= tile->height;
	    if (xoff != tilex || yoff != tiley) {
		XConfigureWindow(ot, xoff, yoff,
				 screen_width - xoff, screen_height - yoff);
		tileclip.left += (xoff - tilex);
		tileclip.top += (yoff - tiley);
		tilex = xoff;
		tiley = yoff;
	    }
	}
}

PIXMAP *PixmapSave (srcx, srcy, width, height)
	int srcx, srcy;			/* source */
	int width, height;
{
	Pixmap pix;
	PIXMAP *pm;

	pix = XPixmapSave(w, srcx, srcy, width, height);
	if (pix == NULL)
	    return(NULL);
	pm = (PIXMAP *) Xalloc (sizeof (PIXMAP));
	pm->width = width;
	pm->height = height;
	pm->refcnt = 1;
	pm->data = (caddr_t) pix;
	IsTile(pm);
	return(pm);
}

PixmapGet (srcx, srcy, width, height, client, format, swapit)
	int srcx, srcy;			/* source */
	int width, height;
	int client;			/* file desc to write to */
	int format;			/* 0: XY-format, 1: Z-format */
	int swapit;			/* 1: swap shorts */
{
	int count;
	caddr_t data;

	if (format == XYFormat) {
	    count = (XYPixmapSize(width, height, dev->planes) + 3) & ~3;
	    data = Xalloc(count);
	    if (XPixmapGetXY(w, srcx, srcy, width, height, data) == NULL)
		return;
	} else {
	    if (dev->planes > 8)
		count = WZPixmapSize(width, height);
	    else {
		count = BZPixmapSize(width, height);
		swapit = 0;
	    }
	    count = (count + 3) & ~3;
	    data = Xalloc(count);
	    if (XPixmapGetZ(w, srcx, srcy, width, height, data) == NULL)
		return;
	}
	FromPixmap(width, height, format, data);
	if (swapit)
	    Swap_shorts(data, count >> 1);
	Write(client, data, count);
	free(data);
}

ResolveColor (red, green, blue)
	unsigned short *red, *green, *blue;	/* update in place */
{
	Color def;

	if (dev->entries) {
	    def.pixel = extrapix;
	    def.red = *red;
	    def.green = *green;
	    def.blue = *blue;
	    XStoreColor(&def);
	    XQueryColor(&def);
	    *red = def.red;
	    *green = def.green;
	    *blue = def.blue;
	}
}

StoreColors (count, entries)
	int count;			/* number of entries */
	register ColorDef *entries;
{
	register Color *ptr;
	register int n;
	static Color *ents = NULL;
	static entcnt = 0;

	if (count > entcnt) {
	    free ((caddr_t) ents);
	    ents = (Color *) Xalloc(count * sizeof(Color));
	    entcnt = count;
	}
	for (ptr = ents, n = count; --n >= 0; entries++, ptr++) {
	    ptr->pixel = npix(entries->pixel);
	    ptr->red = entries->red;
	    ptr->green = entries->green;
	    ptr->blue = entries->blue;
	}
	XStoreColors(count, ents);
}

FONT *GetFont (name)
	char *name;			/* font or file name */
{
	register FontInfo *info;
	register FONT *fd;
	char *fname;

	info = XOpenFont(name);
	if (info == NULL) {
	    errno = EINVAL;
	    return(NULL);
	}
	fd = (FONT *) Xalloc(sizeof(FONT));
	fname = Xalloc(strlen(name) + 1);
	strcpy(fname, name);
	fd->name = fname;
	fd->first = info->firstchar;
	fd->last = info->lastchar;
	fd->space = ' ';
	fd->height = info->height;
	fd->avg_width = info->width;
	fd->fixed = info->fixedwidth;
	fd->base = info->baseline;
	fd->refcnt = 1;
	fd->data = (caddr_t) info;
	return(fd);
}

FreeFont (font)
	FONT *font;
{
	XCloseFont(Xfinf(font));
	free(font->name);
	free((caddr_t) font);
}

CharWidth (c, font)
	unsigned c;			/* character */
	register FONT *font;		/* font */
{
	if (c < font->first || c > font->last)
	    return(0);
	else if (font->fixed)
	    return(font->avg_width);
	else
	    return(Xfinf(font)->widths[c - font->first]);
}

TextWidth (string, strlen, spacepad, font)
	char *string;			/* character string */
	int strlen;			/* string length */
	int spacepad;			/* space-character pad */
	register FONT *font;		/* font */
{
	register u_char *strptr = (u_char *) string;
	short c;
	register short *widths;
	int width = 0;

	if (font->fixed) {
	    width = strlen * font->avg_width;
	    if (spacepad) {
		while (--strlen >= 0)
		    if (*strptr++ == font->space)
		        width += spacepad;
	    }
	} else {
	    widths = Xfinf(font)->widths;
	    while (--strlen >= 0) {
		c = *strptr++;
		if (c >= font->first && c <= font->last) {
		    if (c == font->space)
		        width += spacepad;
		    width += widths[c - font->first];
		}
	    }
	}
	return (width);
}

BITMAP *StoreBitmap (width, height, data)
	int width;			/* bitmap width */
	int height;			/* bitmap height */
	char *data;			/* bitmap data */
{
	Bitmap bitmap;
	register BITMAP *bm;

	bitmap = XStoreBitmap(width, height, data);
	if (bitmap == NULL)
	    return(NULL);
	bm = (BITMAP *) Xalloc(sizeof(BITMAP));
	bm->width = width;
	bm->height = height;
	bm->refcnt = 1;
	bm->data = (caddr_t) bitmap;
	return(bm);
}

FreeBitmap (bitmap)
	BITMAP *bitmap;
{
	XFreeBitmap(Xbitmap(bitmap));
	free((caddr_t) bitmap);
}

BITMAP *CharBitmap (c, font)
	unsigned c;			/* character */
	register FONT *font;		/* font */
{
	Bitmap bitmap;
	register BITMAP *bm;

	bitmap = XCharBitmap(Xfont(font), (int) c);
	if (bitmap == NULL)
	    return(NULL);
	bm = (BITMAP *) Xalloc(sizeof(BITMAP));
	bm->width = CharWidth(c, font);
	bm->height = Xfinf(font)->height;
	bm->refcnt = 1;
	bm->data = (caddr_t) bitmap;
	return(bm);
}

PIXMAP *StorePixmap (width, height, format, data)
	int width;			/* pixmap width */
	int height;			/* pixmap height */
	int format;			/* 0: XY-format, 1: Z-format */
	char *data;			/* pixmap data */
{
	Pixmap pix;
	register PIXMAP *pm;

	ToPixmap(width, height, format, data);
	if (format == XYFormat)
	    pix = XStorePixmapXY(width, height, data);
	else
	    pix = XStorePixmapZ(width, height, data);
	if (pix == NULL)
	    return(NULL);
	pm = (PIXMAP *) Xalloc (sizeof (PIXMAP));
	pm->width = width;
	pm->height = height;
	pm->refcnt = 1;
	pm->data = (caddr_t) pix;
	IsTile(pm);
	return(pm);
}

FreePixmap (pixmap)
	PIXMAP *pixmap;
{
	XFreePixmap(Xpixmap(pixmap));
	free((caddr_t) pixmap);
}

ToPixmap (width, height, format, data)
	int width, height, format;
	char *data;
{
	register int n;
	int shift;
	register u_short *sptr;
	register u_char *cptr;
	register char *ptr;

	if (dev->entries == 0)
	    return;
	if (format == XYFormat) {
	    n = BitmapSize(width, height);
	    if (pixshift) {
		ptr = data + XYPixmapSize(width, height, dev->planes);
		shift = XYPixmapSize(width, height, pixplanes);
		bcopy(ptr - shift,
		      ptr - (shift + XYPixmapSize(width, height, pixshift)),
		      shift);
		for (shift = 0, ptr -= n;
		     shift < pixshift;
		     shift++, ptr -= n) {
		    if (basepix & (1 << shift))
			AllOnes(ptr, width, height);
		    else
			bzero(ptr, n);
		}
	    }
	    for (shift = dev->planes, ptr = data;
		 --shift >= pixshift + pixplanes;
		 ptr += n) {
		if (basepix & (1 << shift))
		    AllOnes(ptr, width, height);
	    }
	} else if (dev->planes > 8) {
	    for (n = WZPixmapSize(width, height) >> 1, sptr = (u_short *) data;
		 --n >= 0;
		 sptr++)	    
		*sptr = npix(*sptr);
	} else {
	    for (n = BZPixmapSize(width, height), cptr = (u_char *) data;
		 --n >= 0;
		 cptr++)
		*cptr = npix(*cptr);
	}
}

AllOnes (data, width, height)
	register char *data;
	int width, height;
{
	register int n, cnt;
	register char *ptr;

	n = BitmapSize(width, 1);
	for (ptr = data, cnt = n; --cnt >= 0; )
	    *ptr++ = ~0;
	for (ptr = data + n, cnt = height; --cnt > 0; ptr += n)
	    bcopy(data, ptr, n);
}

FromPixmap (width, height, format, data)
	int width, height, format;
	register char *data;
{
	register int n;
	register u_short *sptr;
	register u_char *cptr;
	register char *ptr;
	int cnt, shift;

	if (dev->entries == 0)
	    return;
	if (format == XYFormat) {
	    if (pixshift) {
		cnt = BitmapSize(width, height);
		shift = XYPixmapSize(width, height, pixshift);
		for (ptr = data + XYPixmapSize(width, height, dev->planes) - cnt,
		     n = pixshift;
		     --n >= 0;
		     ptr -= cnt)
		    bcopy(ptr - shift, ptr, cnt);
	    }
	    bzero(data, XYPixmapSize(width, height, dev->planes - pixplanes));
	} else if (dev->planes > 8) {
	    for (n = WZPixmapSize(width, height) >> 1, sptr = (u_short *) data;
		 --n >= 0;
		 sptr++)
		*sptr = opix(*sptr);
	} else {
	    for (n = BZPixmapSize(width, height), cptr = (u_char *) data;
		 --n >= 0;
		 cptr++)
		*cptr = opix(*cptr);
	}
}

PIXMAP *MakePixmap (xymask, fore, back)
	BITMAP *xymask;			/* mask or NULL */
	int fore;			/* foreground pixel */
	int back;			/* background pixel */
{
	Pixmap pix;
	register PIXMAP *pm;

	pix = XMakePixmap(xymask ? Xbitmap(xymask) : NULL,
			  npix(fore), npix(back));
	if (pix == NULL)
	    return(NULL);
	pm = (PIXMAP *) Xalloc (sizeof (PIXMAP));
	pm->refcnt = 1;
	pm->data = (caddr_t) pix;
	if (xymask) {
	    pm->width = xymask->width;
	    pm->height = xymask->height;
	    IsTile(pm);
	} else {
	    pm->width = 0;
	    pm->height = 0;
	    pm->tile = 1;
	}
	return(pm);
}

IsTile (pm)
	PIXMAP *pm;
{
	int width, height;

	XQueryTileShape(pm->width, pm->height, &width, &height);
	if (pm->width == width && pm->height == height)
	    pm->tile = 1;
	else
	    pm->tile = 0;
}

QueryShape (shape, width, height)
	int shape;			/* 0: cursor, 1: tile, 2: brush */
	short *width, *height;		/* update in place */
{
	int rwidth, rheight;

	switch (shape) {
	case CursorShape:
	    XQueryCursorShape(*width, *height, &rwidth, &rheight);
	    break;
	case TileShape:
	    XQueryTileShape(*width, *height, &rwidth, &rheight);
	    break;
	case BrushShape:
	    XQueryBrushShape(*width, *height, &rwidth, &rheight);
	    break;
	}
	*width = rwidth;
	*height = rheight;
}

CURSOR *StoreCursor (func, image, fore, back, mask, xoff, yoff)
	int func;			/* GX display function */
	BITMAP *image;			/* cursor image */
	int fore;			/* foreground pixel */
	int back;			/* background pixel */
	BITMAP *mask;			/* cursor mask or NULL */
	int xoff, yoff;			/* tip */
{
	Cursor cursor;
	register CURSOR *cr;
	int width, height;

	cursor = XStoreCursor(Xbitmap(image), mask ? Xbitmap(mask) : NULL,
			      xoff, yoff, npix(fore), npix(back), func);
	if (cursor == NULL)
	    return(NULL);
	XQueryCursorShape(image->width, image->height, &width, &height);
	cr = (CURSOR *) Xalloc(sizeof(CURSOR));
	cr->width = width;
	cr->height = height;
	cr->xoff = xoff;
	cr->yoff = yoff;
	cr->xmin = xoff;
	cr->ymin = yoff;
	cr->xmax = screen_width - (width - xoff);
	cr->ymax = screen_height - (height - yoff);
	cr->refcnt = 1;
	cr->data = (caddr_t) cursor;
	return(cr);
}

FreeCursor (cursor)
	CURSOR *cursor;
{
	XFreeCursor(Xcursor(cursor));
	free((caddr_t) cursor);
}

LoadCursor (cursor)
	CURSOR *cursor;
{
	if (cursor->xoff != cursor_x || cursor->yoff != cursor_y) {
	    XCondWarpMouse(w, mouse.x + cursor->xoff, mouse.y + cursor->yoff,
			   w, mouse.x + cursor_x, mouse.y + cursor_y, 1, 1);
	    cursor_x = cursor->xoff;
	    cursor_y = cursor->yoff;
	}
	XDefineCursor(w, Xcursor(cursor));
}

/*ARGSUSED*/
SetMouseCharacteristics (threshold, acceleration)
	int threshold, acceleration;
{
}

SetCursorPosition (pos)
	vsCursor *pos;
{
	XCondWarpMouse(w, pos->x + cursor_x, pos->y + cursor_y, w, 0, 0, 0, 0);
	mouse = *pos;
}

/*ARGSUSED*/
SetAutoRepeat (onoff)
	int onoff;			/* 0: off, 1: on */
{
}

/*ARGSUSED*/
SetKeyClick (volume)
	int volume;			/* 0: off, 1-8: on */
{
}

SoundBell (volume)
	int volume;			/* 0-7 */
{
	XFeep(volume);
}

/*ARGSUSED*/
SetLockLED (onoff)
	int onoff;			/* 0: off, 1: on */
{
}

SetVideo (onoff)
	int onoff;			/* 0: off, 1: on */
{
	return(onoff - 1);
}
