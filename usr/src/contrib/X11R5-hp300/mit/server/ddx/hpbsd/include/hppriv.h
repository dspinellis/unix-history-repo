/*
 * hp specific additions to standard mfb.h
 */

#ifndef _HPPRIV_H_
#define _HPPRIV_H_

#include "hpOScrnBuf.h"

/* Pixmap stuff */

#define PIXMAP_HOST_MEMORY  1
#define PIXMAP_FRAME_BUFFER 2

        /* private field of pixmap */
typedef struct {
    pointer bits;
    short stride;	/* width of the pixmap in bytes */
    hpChunk *pChunk;	/* description of off-screen memory (if used) */
} hpPrivPixmap;
typedef hpPrivPixmap *hpPrivPixmapPtr;

#define getPrivPixmapPtr(pSrc) \
    ((hpPrivPixmapPtr)(((PixmapPtr)pSrc)->devPrivate.ptr))

/* private field of screen. */
/*
 * Parameters for the two functions in the structure are as
 * follows:
 *   (*MaskConfig)(pScreen, writeEnableMask, replacementRule);
 *   (*MoveBits)(pScreen, planeMask, replacementRule, sourceX, sourceY,
 *   		destX, destY, width, height);
 */

typedef struct {
    pointer 	bits;
    short	stride;
    DrawablePtr	pDrawable;
    pointer	pHardwareScreen;
    void	(*MoveBits)();
    void	(*MaskConfig)();
	/* cursor stuff */
    void (*MoveMouse)(), (*CursorOff)();
    short int
      hoffX, hoffY,	/* offset of hot spot in cursor rectangle */
      width, height,	/* of cursor rectangle */
      ssaveX, ssaveY,	/* where to save screen covered by cursor */
      srcX, srcY,	/* cursor source */
      maskX, maskY,	/* cursor mask */
      w,h;		/* chunk of cursor thats on screen */
    BoxRec saved;	/* coords of cursor rectangle */
    unsigned char
      cstate;		/* cursor state */
#define CURSOR_OFF 0
#define CURSOR_ON  1
    Bool	    (*CloseScreen)();
    void	    (*GetImage)();
    void	    (*GetSpans)();
    void	    (*SourceValidate)();
    Bool	    (*CreateGC)();
    void	    (*InstallColormap)();
    void	    (*StoreColors)();

    void	    (* PaintWindowBackground)();
    void	    (* PaintWindowBorder)();
    void	    (* CopyWindow)();
    void	    (* ClearToBackground)();

    void	    (* SaveDoomedAreas)();
    RegionPtr	    (* RestoreAreas)();
    ColormapPtr     pInstalledMap;
    /* end of cursor stuff */
    unsigned char planesMask;
    /* allow two heads to work on same hardware (e.g. da Vinci) */
    void (*ChangeScreen)();
    Bool isBlank,isSaved;

    pointer     pBufAllocInfo;
    pointer     pTmpPixmap; /* scratch off-screen Pixmap used by cfb code */
    void	(* WholeGlyph)();  /* output entire glyph to offscreen mem */
    short	memHeight;
    short	memWidth;
    short	fd;
    short	gcid;
    unsigned long minor_num;
    int		fbOffset;
    Bool	screenBlanked;
} hpPrivScreen;
typedef hpPrivScreen *hpPrivScreenPtr;

	/* size of scratch pixmap in off-screen memory. */
#define PRIV_PIX_WIDTH	32
#define PRIV_PIX_HEIGHT	32

#define getPrivScreenPtr(pScreen) \
    ((hpPrivScreenPtr)((pScreen)->devPrivate))

#define getPlanesMask(pScreen) (getPrivScreenPtr(pScreen)->planesMask)

#ifndef WAIT_READY_TO_RENDER
#define WAIT_READY_TO_RENDER(pScreen) \
    (*(getPrivScreenPtr(pScreen)->MoveBits)) \
        (pScreen, 0, GXnoop, 0, 0, 0, 0, 0, 0)
#endif

#ifndef SET_REGISTERS_FOR_WRITING
#define SET_REGISTERS_FOR_WRITING(pScreen, writeEnableMask, replacementRule) \
    (*(getPrivScreenPtr(pScreen)->MaskConfig)) \
        ((pScreen), (writeEnableMask), (replacementRule))
#endif

#endif /* _HPPRIV_H_ */
