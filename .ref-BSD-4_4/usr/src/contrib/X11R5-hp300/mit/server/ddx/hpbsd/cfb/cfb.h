/************************************************************
Copyright 1987 by Sun Microsystems, Inc. Mountain View, CA.

                    All Rights Reserved

Permission  to  use,  copy,  modify,  and  distribute   this
software  and  its documentation for any purpose and without
fee is hereby granted, provided that the above copyright no-
tice  appear  in all copies and that both that copyright no-
tice and this permission notice appear in  supporting  docu-
mentation,  and  that the names of Sun or MIT not be used in
advertising or publicity pertaining to distribution  of  the
software  without specific prior written permission. Sun and
M.I.T. make no representations about the suitability of this
software for any purpose. It is provided "as is" without any
express or implied warranty.

SUN DISCLAIMS ALL WARRANTIES WITH REGARD TO  THIS  SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FIT-
NESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SUN BE  LI-
ABLE  FOR  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,  DATA  OR
PROFITS,  WHETHER  IN  AN  ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
THE USE OR PERFORMANCE OF THIS SOFTWARE.

********************************************************/

#include "pixmap.h"
#include "region.h"
#include "gc.h"
#include "colormap.h"
#include "miscstruct.h"
#include "servermd.h"
#include "mfb.h"

extern Bool cfbScreenInit();
extern void cfbQueryBestSize();
extern Bool cfbCreateWindow();
extern Bool cfbPositionWindow();
extern Bool cfbChangeWindowAttributes();
extern Bool cfbMapWindow();
extern Bool cfbUnmapWindow();
extern Bool cfbDestroyWindow();

extern Bool cfbRealizeFont();
extern Bool cfbUnrealizeFont();
extern Bool cfbRealizeCursor();
extern Bool cfbUnrealizeCursor();
extern Bool cfbScreenSaver();
extern Bool cfbCreateGC();

extern PixmapPtr cfbCreatePixmap();
extern Bool cfbDestroyPixmap();

extern void cfbCopyWindow();
extern void cfbPaintWindow();

extern void miPolyFillRect();
extern void cfbPolyFillRect();
extern void miPolyFillArc();
extern void cfbZeroPolyArcSS8Copy(), cfbZeroPolyArcSS8Xor();
extern void cfbZeroPolyArcSS8General();
extern void cfbLineSS(), cfbLineSD(), cfbSegmentSS(), cfbSegmentSD();
extern void cfb8LineSS1Rect(), cfb8SegmentSS1Rect ();
extern RegionPtr cfbCopyPlane();
extern void cfbPolyFillArcSolidCopy(),cfbPolyFillArcSolidXor();
extern void cfbPolyFillArcSolidGeneral();
extern RegionPtr cfbCopyArea();
extern void cfbFillPoly1RectCopy(), cfbFillPoly1RectGeneral();

extern void cfbPushPixels8();
extern void cfbSetSpans();
extern void cfbGetSpans();
extern void cfbSolidSpansCopy(), cfbSolidSpansXor(), cfbSolidSpansGeneral();
extern void cfbUnnaturalTileFS();
extern void cfbUnnaturalStippleFS();
extern void cfbTile32FSCopy(), cfbTile32FSGeneral();
extern void cfb8Stipple32FS(), cfb8OpaqueStipple32FS();
extern void cfbFillBoxTileOdd();
extern void cfbFillBoxTile32();
extern void cfbFillBoxSolid();

extern void cfbTEGlyphBlt();
extern void cfbTEGlyphBlt8();
extern void cfbPolyGlyphBlt8();
extern void cfbPolyGlyphRop8();
extern void cfbImageGlyphBlt8();

extern void cfbSaveAreas();
extern void cfbRestoreAreas();

/* included from mfb.h; we can't include mfb.h directly because of other 
 * conflicts */
extern void mfbPushPixels();
extern void mfbSetSpans();
extern void mfbGetSpans();
extern void mfbUnnaturalTileFS();
extern void mfbUnnaturalStippleFS();
extern Bool mfbRealizeFont();
extern Bool mfbUnrealizeFont();
extern void mfbQueryBestSize();
extern RegionPtr mfbPixmapToRegion();
extern void mfbCopyRotatePixmap();

extern PixmapPtr cfbCopyPixmap();
extern void  cfbConvertRects();
extern void  miPolyArc();
extern void  miFillPolyArc();

extern void cfbPutImage();
extern void cfbGetImage();
extern RegionPtr miCopyArea();
extern RegionPtr miCopyPlane();
extern void cfbPolyPoint();
extern void miPushPixels();

#ifdef	STATIC_COLOR
extern void cfbInstallColormap();
extern void cfbUninstallColormap();
extern int cfbListInstalledColormaps();
#endif
extern void cfbResolveColor();
extern Bool cfbInitializeColormap();
extern Bool cfbCreateDefColormap();

extern void cfbCopyRotatePixmap();
extern void cfbYRotatePixmap();
extern void cfbXRotatePixmap();
extern void cfbPadPixmap();

/*
   private filed of pixmap
   pixmap.devPrivate = (unsigned int *)pointer_to_bits
   pixmap.devKind = width_of_pixmap_in_bytes
*/

extern int  cfbGCPrivateIndex;
extern int  cfbWindowPrivateIndex;

/* private field of GC */
typedef struct {
    unsigned char       rop;            /* special case rop values */
    /* next two values unused in cfb, included for compatibility with mfb */
    unsigned char       ropOpStip;      /* rop for opaque stipple */
    /* this value is ropFillArea in mfb, usurped for cfb */
    unsigned char       oneRect;	/*  drawable has one clip rect */
    unsigned		fExpose:1;	/* callexposure handling ? */
    unsigned		freeCompClip:1;
    PixmapPtr		pRotatedPixmap;
    RegionPtr		pCompositeClip; /* FREE_CC or REPLACE_CC */
    unsigned long	xor, and;	/* reduced rop values */
    } cfbPrivGC;

typedef cfbPrivGC	*cfbPrivGCPtr;

#define cfbGetGCPrivate(pGC)	((cfbPrivGCPtr)\
	(pGC)->devPrivates[cfbGCPrivateIndex].ptr)

/* way to carry RROP info around */
typedef struct {
    unsigned char	rop;
    unsigned long	xor, and;
} cfbRRopRec, *cfbRRopPtr;

/* private field of window */
typedef struct {
    unsigned	char fastBorder; /* non-zero if border is 32 bits wide */
    unsigned	char fastBackground;
    unsigned short unused; /* pad for alignment with Sun compiler */
    DDXPointRec	oldRotate;
    PixmapPtr	pRotatedBackground;
    PixmapPtr	pRotatedBorder;
    } cfbPrivWin;

/* Common macros for extracting drawing information */

#define cfbGetTypedWidth(pDrawable,wtype) (\
    (((pDrawable)->type == DRAWABLE_WINDOW) ? \
     (int) getPrivScreenPtr((pDrawable)->pScreen)->stride : \
     (int) getPrivPixmapPtr(pDrawable)->stride / sizeof (wtype))

#define cfbGetByteWidth(pDrawable) cfbGetTypedWidth(pDrawable, unsigned char)

#define cfbGetLongWidth(pDrawable) cfbGetTypedWidth(pDrawable, unsigned long)
    
#define cfbGetTypedWidthAndPointer(pDrawable, width, pointer, wtype, ptype) {\
    if ((pDrawable)->type == DRAWABLE_WINDOW) \
    {\
	(pointer) = (ptype *) getPrivScreenPtr((pDrawable)->pScreen)->bits; \
	(width) = ((int) getPrivScreenPtr((pDrawable)->pScreen)->stride) / sizeof (wtype); \
    }\
    else \
    {\
	(pointer) = (ptype *) getPrivPixmapPtr(pDrawable)->bits; \
	(width) = ((int) getPrivPixmapPtr(pDrawable)->stride) / sizeof (wtype); \
    }\
}

#define cfbGetByteWidthAndPointer(pDrawable, width, pointer) \
    cfbGetTypedWidthAndPointer(pDrawable, width, pointer, unsigned char, unsigned char)

#define cfbGetLongWidthAndPointer(pDrawable, width, pointer) \
    cfbGetTypedWidthAndPointer(pDrawable, width, pointer, unsigned long, unsigned long)

#define cfbGetWindowTypedWidthAndPointer(pWin, width, pointer, wtype, ptype) {\
    hpPrivScreenPtr	_pPix = (hpPrivScreenPtr) (pWin)->drawable.pScreen->devPrivate; \
    (pointer) = (ptype *) _pPix->bits; \
    (width) = ((int) _pPix->stride) / sizeof (wtype); \
}

#define cfbGetWindowLongWidthAndPointer(pWin, width, pointer) \
    cfbGetWindowTypedWidthAndPointer(pWin, width, pointer, unsigned long, unsigned long)

#define cfbGetWindowByteWidthAndPointer(pWin, width, pointer) \
    cfbGetWindowTypedWidthAndPointer(pWin, width, pointer, unsigned char, unsigned char)

/* Macros which handle a coordinate in a single register */

/* Most compilers will convert divide by 65536 into a shift, if signed
 * shifts exist.  If your machine does arithmetic shifts and your compiler
 * can't get it right, add to this line.
 */

/* mips compiler - what a joke - it CSEs the 65536 constant into a reg
 * forcing as to use div instead of shift.  Let's be explicit.
 */

#if defined(mips) || defined(sparc)
#define GetHighWord(x) (((int) (x)) >> 16)
#else
#define GetHighWord(x) (((int) (x)) / 65536)
#endif

#if IMAGE_BYTE_ORDER == MSBFirst
#define intToCoord(i,x,y)   (((x) = GetHighWord(i)), ((y) = (int) ((short) (i))))
#define coordToInt(x,y)	(((x) << 16) | (y))
#define intToX(i)	(GetHighWord(i))
#define intToY(i)	((int) ((short) i))
#else
#define intToCoord(i,x,y)   (((x) = (int) ((short) (i))), ((y) = GetHighWord(i)))
#define coordToInt(x,y)	(((y) << 16) | (x))
#define intToX(i)	((int) ((short) (i)))
#define intToY(i)	(GetHighWord(i))
#endif
