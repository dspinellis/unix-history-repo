#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/
/* $Header: Xdev.h,v 10.5 86/02/01 15:15:22 tony Rel $ */

/* Definitions for device-dependent interfaces */

typedef struct _device {
	short id;		/* 1=VS100, 2=QVSS, 3=QDSS */
	short width;		/* screen width */
	short height;		/* screen height */
	short planes;		/* number of bit planes */
	unsigned short entries;	/* number of usable color map entries */
	vsCursor *mouse;	/* mouse position */
	vsBox *mbox;		/* mouse motion box */
	vsEventQueue *queue;	/* event queue header */
} DEVICE;

typedef struct _clip {		/* component order optimized for WGA */
	short left;
	short top;
	short width;
	short height;
} CLIP;

typedef struct _bitmap {
	short width;		/* in pixels */
	short height;		/* in pixels */
	short refcnt;		/* reference count */
	char kind;		/* private to device */
	caddr_t data;		/* private to device */
} BITMAP;

typedef struct _pixmap {
	short width;		/* in pixels */
	short height;		/* in pixels */
	short refcnt;		/* reference count */
	char tile;		/* 0: not a tile, 1: tile */
	char kind;		/* private to device */
	caddr_t data;		/* private to device */
} PIXMAP;

typedef struct _font {
	char *name;		/* file name */
	short first;		/* first defined character */
	short last;		/* last defined character */
	short space;		/* space character */
	short height;		/* in pixels */
	short avg_width;	/* average of all defined characters */
	short fixed;		/* 1: all characters are defined same width */
	short base;		/* offset of descenders from bottom */
	short refcnt;		/* reference count */
	caddr_t data;		/* private to device */
} FONT;

typedef struct _cursor {
	short width;		/* in pixels */
	short height;		/* in pixels */
	short xoff;		/* tip, as offsets from upper left */
	short yoff;
	short xmin;		/* minimum position of tip */
	short ymin;
	short xmax;		/* maximum position of tip */
	short ymax;
	short refcnt;		/* reference count */
	caddr_t data;		/* private to device */
} CURSOR;

/* The minimum and maximum positions depend on the hardware and your desires
 * as to what part of the cursor should remain on screen.  If the full
 * cursor rectangle must always be on screen, then:
 *	xmin = xoff
 *	ymin = yoff
 *	xmax = screen_width - (width - xoff)
 *	ymax = screen_height - (height - yoff)
 * If both the tip and the upper left corner must remain on screen, then:
 *	xmin = xoff
 *	ymin = yoff
 *	xmax = screen_width - 1
 *	ymax = screen_height - 1
 * If only the tip must remain on screen, then:
 *	xmin = 0
 *	ymin = 0
 *	xmax = screen_width - 1
 *	ymax = screen_height - 1
 * If only the upper left corner must remain on screen, then:
 *	xmin = xoff
 *	ymin = yoff
 *	xmax = screen_width + xoff - 1
 *	ymax = screen_height + yoff - 1
 */
