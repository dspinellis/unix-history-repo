
#include <X/mit-copyright.h>

/* $Header: Xlib.h,v 10.20 86/12/16 17:17:31 tony Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 *	Xlib.h - Header definition and support file for the C subroutine
 *	interface library (Xlib) to the X Window System Protocol.
 *
 */

extern char *malloc(), *calloc(), *realloc(), *alloca();
#ifdef notdef
#include <sys/types.h>
#endif
#include <X/X.h>

#define Status int
#define XId long
#define XClearVertexFlag() (_XlibCurrentDisplay->lastdraw = NULL)
#define XMakePattern(pattern, patlen, patmul)\
	((Pattern)(((patmul) << 20) | (((patlen) - 1) << 16) | (pattern) ))
#define dpyno() (_XlibCurrentDisplay->fd)
#define RootWindow (_XlibCurrentDisplay->root)
#define BlackPixmap (_XlibCurrentDisplay->black)
#define WhitePixmap (_XlibCurrentDisplay->white)
#define AllPlanes (~0)
#define QLength() (_XlibCurrentDisplay->qlen)
#define DisplayType() (_XlibCurrentDisplay->dtype)
#define DisplayPlanes() (_XlibCurrentDisplay->dplanes)
#define DisplayCells() (_XlibCurrentDisplay->dcells)
#define ProtocolVersion() (_XlibCurrentDisplay->vnumber)
#define DisplayName() (_XlibCurrentDisplay->displayname)

/* Bitmask returned by XParseGeometry().  Each bit tells if the corresponding
   value (x, y, width, height) was found in the parsed string. */

#define NoValue	0x0000
#define XValue  0x0001
#define YValue	0x0002
#define WidthValue  0x0004
#define HeightValue  0x0008
#define AllValues 0x000F
#define XNegative 0x0010
#define YNegative 0x0020

/* Definition of a generic event.  It must be cast to a specific event
 * type before one can read event-specific data */

typedef struct _XEvent {
    	unsigned long type;   /* of event (KeyPressed, ExposeWindow, etc.) */
	Window window;	      /* which selected this event */
	long pad_l1, pad_l2;  /* event-specific data */
	Window subwindow;     /* child window (if any) event actually happened in */
	long pad_l4; 	      /* event-specific data */
} XEvent;


/*
 * _QEvent datatype for use in input queueing.
 */
typedef struct _qevent {
	struct _qevent *next;
	XEvent event;
} _QEvent;


/*
 * Display datatype maintaining display specific data.
 */
typedef struct _display {
	int fd;			/* Network socket. */
	Window root;		/* Root window id. */
	int vnumber;		/* X protocol version number. */
	int dtype;		/* X server display device type. */
	int dplanes;		/* Number of display bit planes. */
	int dcells;		/* Number of display color map cells. */
	_QEvent *head, *tail;	/* Input event queue. */
	int qlen;		/* Length of input event queue */
	int request;		/* Id of last request. */
	char * lastdraw;	/* Last draw request. */
	char *buffer;		/* Output buffer starting address. */
	char *bufptr;		/* Output buffer index pointer. */
	char *bufmax;		/* Output buffer maximum+1 address. */
	int squish;		/* Squish MouseMoved events? */
	Pixmap black, white;    /* Constant tile pixmaps */
	char *displayname;	/* "host:display" string used on this connect*/
	int width, height;	/* width and height of display */
} Display;


/*
 * XAssoc - Association elements used in the XAssocTable data structure.
 * XAssoc's are used as bucket entries in the association table.
 */
typedef struct _x_assoc {
    struct _x_assoc *next;	/* Next object in this bucket. */
    struct _x_assoc *prev;	/* Previous obejct in this bucket. */
    Display *display;		/* Display which ownes the id. */
    XId x_id;			/* X Window System id. */
    char *data;			/* Pointer to untyped memory. */
} XAssoc;

/* 
 * XAssocTable - X Window System id to data structure pointer association
 * table.  An XAssocTable is a hash table who's buckets are circular
 * queue's of XAssoc's.  The XAssocTable is constructed from an array of
 * XAssoc's which are the circular queue headers (bucket headers).  
 * An XAssocTable consists an XAssoc pointer that points to the first
 * bucket in the bucket array and an integer that indicates the number
 * of buckets in the array.
 */
typedef struct _x_assoc_table {
    struct _x_assoc *buckets;	/* Pointer to first bucket in bucket array.*/
    int size;			/* Table size (number of buckets). */
} XAssocTable;

/*
 * Declare the XAssocTable routines that don't return int.
 */
extern char *XLookUpAssoc();
XAssocTable *XCreateAssocTable();


/* 
 * Data returned by XQueryWindow.
 */
typedef struct _WindowInfo {
	short width, height;	/* Width and height. */
	short x, y;		/* X and y coordinates. */
	short bdrwidth;		/* Border width. */
	short mapped;		/* IsUnmapped, IsMapped or IsInvisible.*/
	short type;		/* IsTransparent, IsOpaque or IsIcon. */
	Window assoc_wind;	/* Associated icon or opaque Window. */
} WindowInfo;


/* 
 * Data returned by XQueryFont.
 */
typedef struct _FontInfo {
	Font id;
	short height, width, baseline, fixedwidth;
	unsigned char firstchar, lastchar;
	short *widths;		/* pointer to width array in OpenFont */
} FontInfo;


/*
 * Data structure used by color operations; ints rather than shorts
 * to keep 16 bit protocol limitation out of the library.
 */
typedef struct _Color {
	int pixel;
	unsigned short red, green, blue;
} Color;


/*
 * Data structure use by XCreateTiles.
 */
typedef struct _TileFrame {
	int pixel;		/* Pixel color for constructing the tile. */
	Pixmap pixmap;		/* Pixmap id of the pixmap, filled in later. */
} TileFrame;


/*
 * Data structures used by XCreateWindows XCreateTransparencies and
 * XCreateWindowBatch.
 */
typedef struct _OpaqueFrame {
	Window self;		/* window id of the window, filled in later */
	short x, y;		/* where to create the window */
	short width, height;	/* width and height */
	short bdrwidth;		/* border width */
	Pixmap border;		/* border pixmap */
	Pixmap background;	/* background */
} OpaqueFrame;

typedef struct _TransparentFrame {
	Window self;		/* window id of the window, filled in later */
	short x, y;		/* where to create the window */
	short width, height;	/* width and height */
} TransparentFrame;

typedef struct _BatchFrame {
	short type;		/* One of (IsOpaque, IsTransparent). */
	Window parent;		/* Window if of the window's parent. */
	Window self;		/* Window id of the window, filled in later. */
	short x, y;		/* Where to create the window. */
	short width, height;	/* Window width and height. */
	short bdrwidth;		/* Window border width. */
	Pixmap border;		/* Window border pixmap */
	Pixmap background;	/* Window background pixmap. */
} BatchFrame;


/*
 * Definitions of specific events
 * In all of the following, fields whose names begin with "pad" contain
 * no meaningful value.
 */

struct _XKeyOrButtonEvent {
	unsigned long type;	/* of event (KeyPressed, ButtonReleased, etc.) */
	Window window;		/* which selected this event */
	unsigned short time B16;  /* in 10 millisecond ticks */
	short detail B16;	/* event-dependent data (key state, etc.) */
	short x B16;		/* mouse x coordinate within event window */
	short y B16;		/* mouse y coordinate within event window */
	Window subwindow;	/* child window (if any) mouse was in */
	Locator location;	/* absolute coordinates of mouse */
};

typedef struct _XKeyOrButtonEvent XKeyOrButtonEvent;

typedef struct _XKeyOrButtonEvent XKeyEvent;
typedef struct _XKeyOrButtonEvent XKeyPressedEvent;
typedef struct _XKeyOrButtonEvent XKeyReleasedEvent;

typedef struct _XKeyOrButtonEvent XButtonEvent;
typedef struct _XKeyOrButtonEvent XButtonPressedEvent;
typedef struct _XKeyOrButtonEvent XButtonReleasedEvent;

struct _XMouseOrCrossingEvent {
	unsigned long type;	/* EnterWindow, LeaveWindow, or MouseMoved */
	Window window;		/* which selected this event */
	short pad_s2 B16; 	      
	short detail B16;	/* event-dependent data (key state, etc. ) */
	short x B16;		/* mouse x coordinate within event window */
	short y B16;		/* mouse y coordinate within event window */
	Window subwindow;	/* child window (if any) mouse was in */
	Locator location;	/* absolute coordinates of mouse */
};

typedef struct _XMouseOrCrossingEvent XMouseOrCrossingEvent;

typedef struct _XMouseOrCrossingEvent XMouseEvent;
typedef struct _XMouseOrCrossingEvent XMouseMovedEvent;

typedef struct _XMouseOrCrossingEvent XCrossingEvent;
typedef struct _XMouseOrCrossingEvent XEnterWindowEvent;
typedef struct _XMouseOrCrossingEvent XLeaveWindowEvent;

struct _XExposeEvent {
	unsigned long type;	/* ExposeWindow or ExposeRegion */
	Window window;		/* that selected this event */
	short pad_s2 B16; 	      
	short detail B16;	/* 0 or ExposeCopy */
	short width B16;	/* width of exposed area */
	short height B16;	/* height of exposed area */
	Window subwindow;	/* child window (if any) actually exposed */
	short y B16;		/* top of exposed area (0 for ExposeWindow) */
	short x B16;		/* left edge of exposed area (0 for ExposeWindow) */
};

typedef struct _XExposeEvent XExposeEvent;
typedef struct _XExposeEvent XExposeWindowEvent;
typedef struct _XExposeEvent XExposeRegionEvent;

typedef struct _XExposeCopyEvent {
    	unsigned long type;   /* ExposeCopy */
	Window window;	      /* that selected this event */
	long pad_l1;
	long pad_l2;	      
	Window subwindow;     /* child window (if any) actually exposed */
	long pad_l4;	      
} XExposeCopyEvent;
	
typedef struct _XUnmapEvent {
	unsigned long type;   /* UnmapWindow */
	Window window;	      /* that selected this event */
	long pad_l1;
	long pad_l2;	      
	Window subwindow;     /* child window (if any) actually unmapped */
	long pad_l4;	      
} XUnmapEvent;

typedef struct _XFocusChangeEvent {
	unsigned long type;   /* FocusChange */
	Window window;	      /* that selected this event */
	short pad_s2 B16;
	short detail B16;     /* EnterWindow or LeaveWindow */
	long pad_l2;	      
	Window subwindow;     /* child window (if any) of actual focus change*/
	long pad_l4;	      
} XFocusChangeEvent;

typedef struct _XErrorEvent {
	long pad;
	long serial;		/* serial number of failed request */
	char error_code;    	/* error code of failed request */
	char request_code;	/* request code of failed request */
	char func;  	        /* function field of failed request */
	char pad_b7;
	Window window;	    	/* Window of failed request */
	long pad_l3;
	long pad_l4;
} XErrorEvent;

/*
 * Line pattern related definitions for the library.
 */
typedef long Pattern;

#define DashedLine XMakePattern(0xf0f0, 16, 1)
#define DottedLine XMakePattern(0xaaaa, 16, 1)
#define DotDashLine XMakePattern(0xf4f4, 16, 1)
#define SolidLine  XMakePattern(1,1,1)

typedef short KeyMapEntry [8];

/* define values for keyboard map table */
/* these values will vanish in the next version; DO NOT USE THEM! */
#define SHFT	(short) -2
#define CNTL	(short) -3
#define LOCK	(short) -4
#define SYMBOL	(short) -5
#define KEYPAD	(short) -6
#define CURSOR	(short) -7
#define PFX	(short) -8
#define FUNC1	(short) -9
#define FUNC2	(short) -10
#define FUNC3	(short) -11
#define FUNC4	(short) -12
#define FUNC5	(short) -13
#define FUNC6	(short) -14
#define FUNC7	(short) -15
#define FUNC8	(short) -16
#define FUNC9	(short) -17
#define FUNC10	(short) -18
#define FUNC11	(short) -19
#define FUNC12	(short) -20
#define FUNC13	(short) -21
#define FUNC14	(short) -22
#define FUNC15	(short) -23
#define FUNC16	(short) -24
#define FUNC17	(short) -25
#define FUNC18	(short) -26
#define FUNC19	(short) -27
#define FUNC20	(short) -28
#define E1	(short) -29
#define E2	(short) -30
#define E3	(short) -31
#define E4	(short) -32
#define E5	(short) -33
#define E6	(short) -34


/* 
 * X function declarations.
 */
Display *XOpenDisplay();

char *XFetchBytes();
char * XFetchBuffer();

char *XErrDescrip();
char *XLookupMapping();

short *XFontWidths();
FontInfo *XOpenFont();
extern Display *_XlibCurrentDisplay;
char *XGetDefault();
Bitmap XCharBitmap(), XStoreBitmap();
Pixmap XMakePixmap(), XMakeTile(), XStorePixmapXY(), XStorePixmapZ();
Pixmap XPixmapSave();
Cursor XCreateCursor(), XStoreCursor();
Window XCreate(), XCreateTerm(), XCreateTransparency(), XCreateWindow();
Window XGetIconWindow();
Font XGetFont();
Status XFetchName(), XGetColorCells(), XGetColor(), XGetHardwareColor();
Status XGetResizeHint(), XGrabButton(), XGrabMouse(), XInterpretLocator();
Status XParseColor(), XPixmapGetXY(), XPixmapGetZ(), XQueryMouseButtons();
Status XQueryFont(), XQueryMouse(), XQueryTree(), XQueryWindow();
Status XReadBitmapFile(), XUpdateMouse();
XAssocTable *XCreateAssocTable();
