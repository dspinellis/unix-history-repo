#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985, 1986	*/
/* $Header: X.h,v 10.18 86/11/25 14:54:58 jg Rel $ */

/* Definitions for the X window system likely to be used by applications */

#define X_PROTOCOL	10		/* current protocol version */
#ifdef titan
#define BIGSHORTS
typedef unsigned char * caddr_t;
#endif

#ifdef BIGSHORTS
#define B16 :16
#else
#define B16
#endif

/* Resources */

typedef long Window;
typedef long Font;
typedef long Bitmap;
typedef long Pixmap;
typedef long Cursor;
typedef long Locator;

/* Input Event Codes */

#define NoEvent		 0x0000
#define KeyPressed	 0x0001		/* keyboard key pressed */
#define KeyReleased	 0x0002		/* keyboard key released */
#define ButtonPressed	 0x0004		/* mouse button pressed */
#define ButtonReleased	 0x0008		/* mouse button released */
#define EnterWindow	 0x0010		/* mouse entering window */
#define LeaveWindow	 0x0020		/* mouse leaving window */
#define MouseMoved	 0x0040		/* mouse moves within window */
#define ExposeWindow	 0x0080		/* full window changed and/or exposed */
#define ExposeRegion	 0x0100		/* region of window exposed */
#define ExposeCopy	 0x0200		/* region exposed by X_CopyArea */
#define RightDownMotion	 0x0400		/* mouse moves with right button down */
#define MiddleDownMotion 0x0800		/* mouse moves with middle button down */
#define LeftDownMotion	 0x1000		/* mouse moves with left button down */
#define UnmapWindow	 0x2000		/* window is unmapped */
#define FocusChange	 0x4000		/* keyboard focus changed */

/* Event detail bits */

#define ControlMask	0x4000		/* Control key */
#define MetaMask	0x2000		/* Meta (Symbol) key */
#define ShiftMask	0x1000		/* Shift key */
#define ShiftLockMask	0x0800		/* ShiftLock key */
#define LeftMask	0x0400		/* Left button */
#define MiddleMask	0x0200		/* Middle button */
#define RightMask	0x0100		/* Right button */
#define ValueMask	0x00ff		/* Key/button code */

#define KeyState(x) (((x) & (ControlMask|MetaMask|ShiftMask)) >> 12)
#define FullKeyState(x) (((x) & (ControlMask|MetaMask|ShiftMask|ShiftLockMask)) >> 11)
#define ButtonState(x) (((x) & (LeftMask|MiddleMask|RightMask)) >> 8)

/* Button event detail codes */

#define RightButton	0
#define MiddleButton	1
#define LeftButton	2

/* Enter/Leave event detail codes */

#define IntoOrFromSubwindow	1
#define VirtualCrossing		2

/* These are the error codes */

#define BadRequest	1		/* bad request code */
#define BadValue	2		/* int parameter out of range */
#define BadWindow	3		/* parameter not a Window */
#define BadPixmap	4		/* parameter not a Pixmap */
#define BadBitmap	5		/* parameter not a Bitmap */
#define BadCursor	6		/* parameter not a Cursor */
#define BadFont		7		/* parameter not a Font */
#define BadMatch	8		/* parameter mismatch */
#define BadTile		9		/* Pixmap shape invalid for tiling */
#define BadGrab		10		/* mouse/button already grabbed */
#define BadAccess	11		/* access control violation */
#define BadAlloc	12		/* insufficient resources */
#define BadColor	13		/* no such color */

/* for monochrome applications */

#define BlackPixel		0		/* may not actually be black */
#define WhitePixel		1		/* may not actually be white */

/* graphics functions */

#define	GXclear			0x0		/* 0 */
#define GXand			0x1		/* src AND dst */
#define GXandReverse		0x2		/* src AND NOT dst */
#define GXcopy			0x3		/* src */
#define GXandInverted		0x4		/* NOT src AND dst */
#define	GXnoop			0x5		/* dst */
#define GXxor			0x6		/* src XOR dst */
#define GXor			0x7		/* src OR dst */
#define GXnor			0x8		/* NOT src AND NOT dst */
#define GXequiv			0x9		/* NOT src XOR dst */
#define GXinvert		0xa		/* NOT dst */
#define GXorReverse		0xb		/* src OR NOT dst */
#define GXcopyInverted		0xc		/* NOT src */
#define GXorInverted		0xd		/* NOT src OR dst */
#define GXnand			0xe		/* NOT src OR NOT dst */
#define GXset			0xf		/* 1 */

/* Used in X_TileMode */

#define TileModeAbsolute	0
#define TileModeRelative	1

/* Used in X_ClipMode */

#define ClipModeClipped		0
#define ClipModeDrawThru	1

/* Used in X_QueryWindow reply */

#define IsUnmapped		0
#define IsMapped		1
#define IsInvisible		2

#define IsTransparent		0
#define IsOpaque		1
#define IsIcon			2

/* Used in X_Draw */

#define	DrawSolidLine		0
#define DrawDashedLine		1
#define DrawPatternedLine	2

/* Used in X_Draw and X_DrawFilled */

typedef struct _Vertex {
	short x, y;
	unsigned short flags;
} Vertex;

/* The meanings of the flag bits.  If the bit is 1 the predicate is true */

#define VertexRelative		0x0001		/* else absolute */
#define VertexDontDraw		0x0002		/* else draw */
#define VertexCurved		0x0004		/* else straight */
#define VertexStartClosed	0x0008		/* else not */
#define VertexEndClosed		0x0010		/* else not */
#define VertexDrawLastPoint	0x0020		/* else don't */

/* Device identifiers returned by X_SetUp */

#define XDEV_XNEST		0	/* X in an X window */

/* DEC address space 1-99 */
#define XDEV_VS100		1	/* DEC VS100			*/
#define XDEV_QVSS		2	/* DEC QVSS (VS1 and VS2)	*/
#define XDEV_QDSS		3	/* DEC QDSS display		*/
#define XDEV_DECXXX		4	/* reserved for future use	*/
#define XDEV_DECYYY		5	/* reserved for future use	*/
#define XDEV_DECZZZ		6	/* reserved for future use	*/
#define XDEV_DECLLL		7	/* reserved for future use	*/

/* Cognition address space 100-199 */
#define XDEV_LEX90		100	/* Lexidata 90, Cognition       */

/* IBM address space 200-299 */
#define XDEV_IBMAED             200     /* IBM ACIS Experimental Display */
#define XDEV_IBMAPA8            201     /* IBM APA8 Display              */
#define XDEV_IBMAPA16           202     /* IBM APA16 Display             */
#define XDEV_IBMAPA8C           203     /* IBM APA8 Color Display        */
#define XDEV_IBMPQD             204     /* IBM Print Quality Display     */

/* SMI address space 300-399 */
#define SUN_BASE		300	/* base of SMI displays		*/
#ifndef FBTYPE_SUN1BW
/* from /usr/include/sun/fbio.h */
#define FBTYPE_SUN1BW		0
#define FBTYPE_SUN1COLOR	1
#define FBTYPE_SUN2BW		2
#define FBTYPE_SUN2COLOR	3
#define FBTYPE_SUN2GP		4	/* reserved for future Sun use	*/
#define FBTYPE_SUN3BW		5	/* reserved for future Sun use	*/
#define FBTYPE_SUN3COLOR	6	/* reserved for future Sun use	*/
#define FBTYPE_SUN4BW		7	/* reserved for future Sun use	*/
#define FBTYPE_SUN4COLOR	8	/* reserved for future Sun use	*/
#define FBTYPE_NOTSUN1		9	/* reserved for Sun customer	*/
#define FBTYPE_NOTSUN2		10	/* reserved for Sun customer	*/
#define FBTYPE_NOTSUN3		11	/* reserved for Sun customer	*/
#endif
#define	XDEV_SUN1BW		FBTYPE_SUN1BW+SUN_BASE
#define	XDEV_SUN1COLOR		FBTYPE_SUN1COLOR+SUN_BASE
#define	XDEV_SUN2BW		FBTYPE_SUN2BW+SUN_BASE
#define	XDEV_SUN2COLOR		FBTYPE_SUN2COLOR+SUN_BASE
#define	XDEV_SUN2GP		FBTYPE_SUN2GP+SUN_BASE
#define	XDEV_SUN3BW		FBTYPE_SUN3BW+SUN_BASE
#define	XDEV_SUN3COLOR		FBTYPE_SUN3COLOR+SUN_BASE
#define	XDEV_SUN4BW		FBTYPE_SUN4BW+SUN_BASE
#define	XDEV_SUN4COLOR		FBTYPE_SUN4COLOR+SUN_BASE
#define	XDEV_NOTSUN1		FBTYPE_NOTSUN1+SUN_BASE
#define	XDEV_NOTSUN2		FBTYPE_NOTSUN2+SUN_BASE
#define	XDEV_NOTSUN3		FBTYPE_NOTSUN3+SUN_BASE

/* MASSCOMP address space 400-499 */
#define XDEV_MC1		401	/* Masscomp, in progress	*/
#define XDEV_MC2		402	/* Masscomp, (not implemented)  */
#define XDEV_MC3		403	/* Masscomp, (not implemented)  */

/* Jupiter Systems address space 500-599 */
#define XDEV_PGP20		501	/* 24 bit deep frame buffer	*/

/* Data General address space 600-699 */
#define XDEV_DS4000		601	/* Data General DS4000 display  */
#define XDEV_DS4200		602	/* Data General DS4200 display  */

/* Apollo Computer address space 700-799 */
#define XDEV_APOLLO

/* Hewlett Packard address space 800-899 */
#define XDEV_HP9000S300		800

/* Integrated Solutions address space 900-999 */
#define XDEV_ISI		900		/* ISI base address */
#define XDEV_ISIBW		XDEV_ISI+1	/* Optimum V monochrome */
#define XDEV_ISICOLOR4		XDEV_ISI+2	/* Optimum V 4 bit color */

/* Periphere Computer Systeme address space 1000-1099 */
#define XDEV_PCS                1000            /* Periphere base address */


/* Used in X_StoreColors */

typedef struct _ColorDef {
	unsigned short pixel B16;
	unsigned short red B16, green B16, blue B16;
} ColorDef;

/* Used in X_PixmapBitsPut and X_StorePixmap */

#define XYFormat		0
#define ZFormat			1

#define UBPS (sizeof(short)/2) /* useful bytes per short */
/* size in bytes of a bitmap */
#define BitmapSize(width, height) (((((width) + 15) >> 3) &~ 1) * (height) * UBPS)
/* size in bytes of a pixmap in XYFormat */
#define XYPixmapSize(width, height, planes) (BitmapSize(width, height) * (planes))
/* size in bytes of a pizmap in ZFormat for 9 to 16 bit planes */
#define WZPixmapSize(width, height) (((width) * (height)) << 1)
/* size in bytes of a pixmap in ZFormat for 2 to 8 bit planes */
#define BZPixmapSize(width, height) ((width) * (height))

/* Used in X_QueryShape */

#define CursorShape		0
#define TileShape		1
#define BrushShape		2

/* Used in X_ShiftLock */

#define LockUpDownMode		0
#define LockToggleMode		1

/* Used in X_AddHost, X_RemoveHost, and X_GetHosts */

#define XAF_INET		2
#define XAF_DECnet		12
