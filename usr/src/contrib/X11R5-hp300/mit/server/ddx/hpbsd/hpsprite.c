/*
 * hpsprite.c
 *
 * hp screen independent software sprite routines
 */

/* $XConsortium: misprite.c,v 5.28 90/01/13 17:33:32 rws Exp $ */

/*
Copyright 1989 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission.  M.I.T. makes no
representations about the suitability of this software for any
purpose.  It is provided "as is" without express or implied warranty.
*/

# include   "X.h"
# include   "Xproto.h"
# include   "misc.h"
# include   "pixmapstr.h"
# include   "input.h"
# include   "mi.h"
# include   "cursorstr.h"
# include   "font.h"
# include   "scrnintstr.h"
# include   "colormapst.h"
# include   "windowstr.h"
# include   "gcstruct.h"
# include   "hpspritest.h"
# include   "dixfontstr.h"
# include   "fontstruct.h"
# include   "hppriv.h"

/*
 * screen wrappers
 */

static Bool	    hpSpriteCloseScreen();
static void	    hpSpriteGetImage();
static void	    hpSpriteGetSpans();
static void	    hpSpriteSourceValidate();
static Bool	    hpSpriteCreateGC();
static void	    hpSpriteInstallColormap();
static void	    hpSpriteStoreColors();

static void	    hpSpritePaintWindowBackground();
static void	    hpSpritePaintWindowBorder();
static void	    hpSpriteCopyWindow();
static void	    hpSpriteClearToBackground();

static void	    hpSpriteSaveDoomedAreas();
static RegionPtr    hpSpriteRestoreAreas();

extern Bool	    hpCursorLoc();

#define SCREEN_PROLOGUE(pScreen, field)\
  ((pScreen)->field = getPrivScreenPtr(pScreen)->field)

#define SCREEN_EPILOGUE(pScreen, field, wrapper)\
    ((pScreen)->field = wrapper)

/*
 * GC func wrappers
 */

static unsigned long hpSpriteGeneration = 0;
static int  hpSpriteGCIndex;

static void hpSpriteValidateGC (),  hpSpriteCopyGC ();
static void hpSpriteDestroyGC(),    hpSpriteChangeGC();
static void hpSpriteChangeClip(),   hpSpriteDestroyClip();
static void hpSpriteCopyClip();

static GCFuncs	hpSpriteGCFuncs = {
    hpSpriteValidateGC,
    hpSpriteChangeGC,
    hpSpriteCopyGC,
    hpSpriteDestroyGC,
    hpSpriteChangeClip,
    hpSpriteDestroyClip,
    hpSpriteCopyClip,
};

#define GC_FUNC_PROLOGUE(pGC)					\
    hpSpriteGCPtr   pGCPriv =					\
	(hpSpriteGCPtr) (pGC)->devPrivates[hpSpriteGCIndex].ptr;\
    (pGC)->funcs = pGCPriv->wrapFuncs;				\
    if (pGCPriv->wrapOps)					\
	(pGC)->ops = pGCPriv->wrapOps;

#define GC_FUNC_EPILOGUE(pGC)					\
    pGCPriv->wrapFuncs = (pGC)->funcs;				\
    (pGC)->funcs = &hpSpriteGCFuncs;				\
    if (pGCPriv->wrapOps)					\
    {								\
	pGCPriv->wrapOps = (pGC)->ops;				\
	(pGC)->ops = &hpSpriteGCOps;				\
    }

/*
 * GC op wrappers
 */

static void	    hpSpriteFillSpans(),	hpSpriteSetSpans();
static void	    hpSpritePutImage();
static RegionPtr    hpSpriteCopyArea(),		hpSpriteCopyPlane();
static void	    hpSpritePolyPoint(),	hpSpritePolylines();
static void	    hpSpritePolySegment(),	hpSpritePolyRectangle();
static void	    hpSpritePolyArc(),		hpSpriteFillPolygon();
static void	    hpSpritePolyFillRect(),	hpSpritePolyFillArc();
static int	    hpSpritePolyText8(),	hpSpritePolyText16();
static void	    hpSpriteImageText8(),	hpSpriteImageText16();
static void	    hpSpriteImageGlyphBlt(),	hpSpritePolyGlyphBlt();
static void	    hpSpritePushPixels(),	hpSpriteLineHelper();
static void	    hpSpriteChangeClip(),	hpSpriteDestroyClip();
static void	    hpSpriteCopyClip();

static GCOps hpSpriteGCOps = {
    hpSpriteFillSpans,	    hpSpriteSetSpans,	    hpSpritePutImage,	
    hpSpriteCopyArea,	    hpSpriteCopyPlane,	    hpSpritePolyPoint,
    hpSpritePolylines,	    hpSpritePolySegment,    hpSpritePolyRectangle,
    hpSpritePolyArc,	    hpSpriteFillPolygon,    hpSpritePolyFillRect,
    hpSpritePolyFillArc,    hpSpritePolyText8,	    hpSpritePolyText16,
    hpSpriteImageText8,	    hpSpriteImageText16,    hpSpriteImageGlyphBlt,
    hpSpritePolyGlyphBlt,   hpSpritePushPixels,	    hpSpriteLineHelper,
};

/*
 * testing only -- remove cursor for every draw.  Eventually,
 * each draw operation will perform a bounding box check against
 * the saved cursor area
 */

#define GC_SETUP_CHEAP(pDrawable)				    \
    hpPrivScreenPtr	pScreenPriv = getPrivScreenPtr((pDrawable)->pScreen);

#define GC_SETUP(pDrawable, pGC)				    \
    GC_SETUP_CHEAP(pDrawable)					    \
    hpSpriteGCPtr	pGCPrivate = (hpSpriteGCPtr)		    \
	(pGC)->devPrivates[hpSpriteGCIndex].ptr;		    \
    GCFuncs *oldFuncs = pGC->funcs;

#define GC_SETUP_AND_CHECK(pDrawable, pGC)			    \
    GC_SETUP(pDrawable, pGC);					    \
    if (GC_CHECK((WindowPtr)pDrawable))				    \
	(*pScreenPriv->CursorOff) (pDrawable->pScreen);
    
#define GC_CHECK(pWin)						    \
    (pScreenPriv->cstate == CURSOR_ON &&			    \
	(pWin)->drawable.x < pScreenPriv->saved.x2 &&		    \
	pScreenPriv->saved.x1 < (pWin)->drawable.x + (int) (pWin)->drawable.width && \
	(pWin)->drawable.y < pScreenPriv->saved.y2 &&		    \
	pScreenPriv->saved.y1 < (pWin)->drawable.y + (int) (pWin)->drawable.height)

#define GC_OP_PROLOGUE(pGC) { \
    (pGC)->funcs = pGCPrivate->wrapFuncs; \
    (pGC)->ops = pGCPrivate->wrapOps; \
    }

#define GC_OP_EPILOGUE(pGC) { \
    pGCPrivate->wrapOps = (pGC)->ops; \
    (pGC)->funcs = oldFuncs; \
    (pGC)->ops = &hpSpriteGCOps; \
    }

/*
 * hpSpriteInitialize -- called from device-dependent screen
 * initialization proc after all of the function pointers have
 * been stored in the screen structure.
 */

Bool
hpSpriteInitialize (pScreen)
     ScreenPtr pScreen;
{
    hpPrivScreenPtr	pPriv;
    
    if (hpSpriteGeneration != serverGeneration)
    {
        hpSpriteGeneration = serverGeneration;
        hpSpriteGCIndex = AllocateGCPrivateIndex ();
    }
    if (!AllocateGCPrivate(pScreen, hpSpriteGCIndex, sizeof(hpSpriteGCRec)))
	return FALSE;
    pPriv = getPrivScreenPtr(pScreen);
    pPriv->pInstalledMap = NULL;
    pPriv->CloseScreen = pScreen->CloseScreen;
    pPriv->GetImage = pScreen->GetImage;
    pPriv->GetSpans = pScreen->GetSpans;
    pPriv->SourceValidate = pScreen->SourceValidate;
    pPriv->CreateGC = pScreen->CreateGC;
    pPriv->InstallColormap = pScreen->InstallColormap;
    pPriv->StoreColors = pScreen->StoreColors;

    pPriv->PaintWindowBackground = pScreen->PaintWindowBackground;
    pPriv->PaintWindowBorder = pScreen->PaintWindowBorder;
    pPriv->CopyWindow = pScreen->CopyWindow;
    pPriv->ClearToBackground = pScreen->ClearToBackground;

    pPriv->SaveDoomedAreas = pScreen->SaveDoomedAreas;
    pPriv->RestoreAreas = pScreen->RestoreAreas;

    pScreen->CloseScreen = hpSpriteCloseScreen;
    pScreen->GetImage = hpSpriteGetImage;
    pScreen->GetSpans = hpSpriteGetSpans;
    pScreen->SourceValidate = hpSpriteSourceValidate;
    pScreen->CreateGC = hpSpriteCreateGC;
    pScreen->InstallColormap = hpSpriteInstallColormap;
    pScreen->StoreColors = hpSpriteStoreColors;

    pScreen->PaintWindowBackground = hpSpritePaintWindowBackground;
    pScreen->PaintWindowBorder = hpSpritePaintWindowBorder;
    pScreen->CopyWindow = hpSpriteCopyWindow;
    pScreen->ClearToBackground = hpSpriteClearToBackground;

    pScreen->SaveDoomedAreas = hpSpriteSaveDoomedAreas;
    pScreen->RestoreAreas = hpSpriteRestoreAreas;

    return TRUE;
}

/*
 * Screen wrappers
 */

/*
 * CloseScreen wrapper -- unwrap everything, free the private data
 * and call the wrapped function
 */

static Bool
hpSpriteCloseScreen (i, pScreen)
     int i;
     ScreenPtr pScreen;
{
    hpPrivScreenPtr pScreenPriv = getPrivScreenPtr(pScreen);

    pScreen->CloseScreen = pScreenPriv->CloseScreen;
    pScreen->GetImage = pScreenPriv->GetImage;
    pScreen->GetSpans = pScreenPriv->GetSpans;
    pScreen->SourceValidate = pScreenPriv->SourceValidate;
    pScreen->CreateGC = pScreenPriv->CreateGC;
    pScreen->InstallColormap = pScreenPriv->InstallColormap;
    pScreen->StoreColors = pScreenPriv->StoreColors;

    pScreen->PaintWindowBackground = pScreenPriv->PaintWindowBackground;
    pScreen->PaintWindowBorder = pScreenPriv->PaintWindowBorder;
    pScreen->CopyWindow = pScreenPriv->CopyWindow;
    pScreen->ClearToBackground = pScreenPriv->ClearToBackground;

    pScreen->SaveDoomedAreas = pScreenPriv->SaveDoomedAreas;
    pScreen->RestoreAreas = pScreenPriv->RestoreAreas;

    return (*pScreen->CloseScreen) (i, pScreen);
}

static void
hpSpriteGetImage (pDrawable, sx, sy, w, h, format, planemask, pdstLine)
    DrawablePtr	    pDrawable;
    int		    sx, sy, w, h;
    unsigned int    format;
    unsigned long   planemask;
    pointer	    pdstLine;
{
    ScreenPtr	    pScreen = pDrawable->pScreen;
    hpPrivScreenPtr pScreenPriv;
    
    SCREEN_PROLOGUE (pScreen, GetImage);

    pScreenPriv = getPrivScreenPtr(pScreen);

    if (pDrawable->type == DRAWABLE_WINDOW &&
        pScreenPriv->cstate == CURSOR_ON &&
	ORG_OVERLAP(&pScreenPriv->saved, pDrawable->x, pDrawable->y, sx, sy, w, h))
	    (*pScreenPriv->CursorOff) (pScreen);

    (*pScreen->GetImage) (pDrawable, sx, sy, w, h,
			  format, planemask, pdstLine);

    SCREEN_EPILOGUE (pScreen, GetImage, hpSpriteGetImage);
}

static void
hpSpriteGetSpans (pDrawable, wMax, ppt, pwidth, nspans, pdstStart)
    DrawablePtr	pDrawable;
    int		wMax;
    DDXPointPtr	ppt;
    int		*pwidth;
    int		nspans;
    unsigned int *pdstStart;
{
    ScreenPtr		pScreen = pDrawable->pScreen;
    hpPrivScreenPtr	pScreenPriv;
    
    SCREEN_PROLOGUE (pScreen, GetSpans);

    pScreenPriv = getPrivScreenPtr(pScreen);

    if (pDrawable->type == DRAWABLE_WINDOW && pScreenPriv->cstate == CURSOR_ON)
    {
	register DDXPointPtr    pts;
	register int    	*widths;
	register int    	nPts;
	register int    	xorg,
				yorg;

	xorg = pDrawable->x;
	yorg = pDrawable->y;

	for (pts = ppt, widths = pwidth, nPts = nspans;
	     nPts--;
	     pts++, widths++)
 	{
	    if (SPN_OVERLAP(&pScreenPriv->saved, pts->y+yorg,
			     pts->x+xorg,*widths))
	    {
		(*pScreenPriv->CursorOff) (pScreen);
		break;
	    }
	}
    }

    (*pScreen->GetSpans) (pDrawable, wMax, ppt, pwidth, nspans, pdstStart);

    SCREEN_EPILOGUE (pScreen, GetSpans, hpSpriteGetSpans);
}

static void
hpSpriteSourceValidate (pDrawable, x, y, width, height)
    DrawablePtr	pDrawable;
    int		x, y, width, height;
{
    ScreenPtr		pScreen = pDrawable->pScreen;
    hpPrivScreenPtr	pScreenPriv;
    
    SCREEN_PROLOGUE (pScreen, SourceValidate);

    pScreenPriv = getPrivScreenPtr(pScreen);

    if (pDrawable->type == DRAWABLE_WINDOW && 
	pScreenPriv->cstate == CURSOR_ON &&
	ORG_OVERLAP(&pScreenPriv->saved, pDrawable->x, pDrawable->y,
		    x, y, width, height))
    {
	(*pScreenPriv->CursorOff) (pScreen);
    }

    if (pScreen->SourceValidate)
	(*pScreen->SourceValidate) (pDrawable, x, y, width, height);

    SCREEN_EPILOGUE (pScreen, SourceValidate, hpSpriteSourceValidate);
}

static Bool
hpSpriteCreateGC (pGC)
    GCPtr   pGC;
{
    ScreenPtr	    pScreen = pGC->pScreen;
    Bool	    ret;
    hpSpriteGCPtr   pPriv;

    SCREEN_PROLOGUE (pScreen, CreateGC);
    
    pPriv = (hpSpriteGCPtr)pGC->devPrivates[hpSpriteGCIndex].ptr;

    ret = (*pScreen->CreateGC) (pGC);

    pPriv->wrapOps = NULL;
    pPriv->wrapFuncs = pGC->funcs;
    pGC->funcs = &hpSpriteGCFuncs;

    SCREEN_EPILOGUE (pScreen, CreateGC, hpSpriteCreateGC);

    return ret;
}

static void
hpSpriteInstallColormap (pMap)
    ColormapPtr	pMap;
{
    ScreenPtr		pScreen = pMap->pScreen;
    hpPrivScreenPtr	pPriv;

    pPriv = getPrivScreenPtr(pScreen);

    SCREEN_PROLOGUE(pScreen, InstallColormap);
    
    (*pScreen->InstallColormap) (pMap);

    SCREEN_EPILOGUE(pScreen, InstallColormap, hpSpriteInstallColormap);

    if (pPriv->pInstalledMap != pMap)
    {
    	pPriv->pInstalledMap = pMap;
	(*pPriv->CursorOff) (pScreen);
    }
}

static void
hpSpriteStoreColors (pMap, ndef, pdef)
    ColormapPtr	pMap;
    int		ndef;
    xColorItem	*pdef;
{
    ScreenPtr		pScreen = pMap->pScreen;
    hpPrivScreenPtr	pPriv;
#if 0
    int			i;
#endif

    pPriv = getPrivScreenPtr(pScreen);

    SCREEN_PROLOGUE(pScreen, StoreColors);

    (*pScreen->StoreColors) (pMap, ndef, pdef);

    SCREEN_EPILOGUE(pScreen, StoreColors, hpSpriteStoreColors);

    if (pPriv->pInstalledMap == pMap)
    {
#if 0
	for (i = 0; i < ndef; i++)
	    /*
	     * XXX direct color will affect pixels other than
	     * pdef[i].pixel -- this will be more difficult...
	     */
	    if (pdef[i].pixel == pPriv->colors[SOURCE_COLOR].pixel ||
	        pdef[i].pixel == pPriv->colors[MASK_COLOR].pixel)
	    {
		pPriv->checkPixels = TRUE;
		if (pPriv->isUp && pPriv->shouldBeUp)
#endif
		    (*pPriv->CursorOff) (pScreen);
#if 0
		break;
	    }
#endif
    }
}

void
hpSpriteFindColors(pScreen, pCursor, fgpix, bgpix)
     ScreenPtr pScreen;
     CursorPtr pCursor;
     Pixel *fgpix, *bgpix;
{
    ColormapPtr	cmap = getPrivScreenPtr(pScreen)->pInstalledMap;
    xColorItem fgitem, bgitem;

    fgitem.red = pCursor->foreRed;
    fgitem.green = pCursor->foreGreen;
    fgitem.blue = pCursor->foreBlue;
    FakeAllocColor(cmap, &fgitem);
    bgitem.red = pCursor->backRed;
    bgitem.green = pCursor->backGreen;
    bgitem.blue = pCursor->backBlue;
    FakeAllocColor(cmap, &bgitem);
    *fgpix = fgitem.pixel;
    *bgpix = bgitem.pixel;
    /* "free" the pixels right away, don't let this confuse you */
    FakeFreeColor(cmap, fgitem.pixel);
    FakeFreeColor(cmap, bgitem.pixel);
}

/*
 * BackingStore wrappers
 */

static void
hpSpriteSaveDoomedAreas (pWin, pObscured, dx, dy)
    WindowPtr	pWin;
    RegionPtr	pObscured;
    int		dx, dy;
{
    ScreenPtr		pScreen;
    hpPrivScreenPtr	pScreenPriv;
    BoxRec		cursorBox;

    pScreen = pWin->drawable.pScreen;
    
    SCREEN_PROLOGUE (pScreen, SaveDoomedAreas);

    pScreenPriv = getPrivScreenPtr(pScreen);
    if (pScreenPriv->cstate == CURSOR_ON)
    {
	cursorBox = pScreenPriv->saved;

	if (dx || dy)
 	{
	    cursorBox.x1 += dx;
	    cursorBox.y1 += dy;
	    cursorBox.x2 += dx;
	    cursorBox.y2 += dy;
	}
	if ((* pScreen->RectIn) (pObscured, &cursorBox) != rgnOUT)
	    (*pScreenPriv->CursorOff) (pScreen);
    }

    (*pScreen->SaveDoomedAreas) (pWin, pObscured, dx, dy);

    SCREEN_EPILOGUE (pScreen, SaveDoomedAreas, hpSpriteSaveDoomedAreas);
}

static RegionPtr
hpSpriteRestoreAreas (pWin, prgnExposed)
    WindowPtr	pWin;
    RegionPtr	prgnExposed;
{
    ScreenPtr		pScreen;
    hpPrivScreenPtr	pScreenPriv;
    RegionPtr		result;
    
    pScreen = pWin->drawable.pScreen;
    
    SCREEN_PROLOGUE (pScreen, RestoreAreas);

    pScreenPriv = getPrivScreenPtr(pScreen);
    if (pScreenPriv->cstate == CURSOR_ON)
    {
	if ((* pScreen->RectIn) (prgnExposed, &pScreenPriv->saved) != rgnOUT)
	    (*pScreenPriv->CursorOff) (pScreen);
    }

    result = (*pScreen->RestoreAreas) (pWin, prgnExposed);

    SCREEN_EPILOGUE (pScreen, RestoreAreas, hpSpriteRestoreAreas);

    return result;
}

/*
 * Window wrappers
 */

static void
hpSpritePaintWindowBackground (pWin, pRegion, what)
    WindowPtr	pWin;
    RegionPtr	pRegion;
    int		what;
{
    ScreenPtr	    pScreen;
    hpPrivScreenPtr pScreenPriv;

    pScreen = pWin->drawable.pScreen;

    SCREEN_PROLOGUE (pScreen, PaintWindowBackground);

    pScreenPriv = getPrivScreenPtr(pScreen);
    if (pScreenPriv->cstate == CURSOR_ON)
    {
	/*
	 * If the cursor is on the same screen as the window, check the
	 * region to paint for the cursor and remove it as necessary
	 */
	if ((* pScreen->RectIn) (pRegion, &pScreenPriv->saved) != rgnOUT)
	    (*pScreenPriv->CursorOff) (pScreen);
    }

    (*pScreen->PaintWindowBackground) (pWin, pRegion, what);

    SCREEN_EPILOGUE (pScreen, PaintWindowBackground, hpSpritePaintWindowBackground);
}

static void
hpSpritePaintWindowBorder (pWin, pRegion, what)
    WindowPtr	pWin;
    RegionPtr	pRegion;
    int		what;
{
    ScreenPtr	    pScreen;
    hpPrivScreenPtr pScreenPriv;
    
    pScreen = pWin->drawable.pScreen;

    SCREEN_PROLOGUE (pScreen, PaintWindowBorder);

    pScreenPriv = getPrivScreenPtr(pScreen);
    if (pScreenPriv->cstate == CURSOR_ON)
    {
	/*
	 * If the cursor is on the same screen as the window, check the
	 * region to paint for the cursor and remove it as necessary
	 */
	if ((* pScreen->RectIn) (pRegion, &pScreenPriv->saved) != rgnOUT)
	    (*pScreenPriv->CursorOff) (pScreen);
    }

    (*pScreen->PaintWindowBorder) (pWin, pRegion, what);

    SCREEN_EPILOGUE (pScreen, PaintWindowBorder, hpSpritePaintWindowBorder);
}

static void
hpSpriteCopyWindow (pWin, ptOldOrg, pRegion)
    WindowPtr	pWin;
    DDXPointRec	ptOldOrg;
    RegionPtr	pRegion;
{
    ScreenPtr	    pScreen;
    hpPrivScreenPtr pScreenPriv;
    BoxRec	    cursorBox;
    int		    dx, dy;

    pScreen = pWin->drawable.pScreen;

    SCREEN_PROLOGUE (pScreen, CopyWindow);

    pScreenPriv = getPrivScreenPtr(pScreen);
    if (pScreenPriv->cstate == CURSOR_ON)
    {
	/*
	 * check both the source and the destination areas.  The given
	 * region is source relative, so offset the cursor box by
	 * the delta position
	 */
	cursorBox = pScreenPriv->saved;
	dx = pWin->drawable.x - ptOldOrg.x;
	dy = pWin->drawable.y - ptOldOrg.y;
	cursorBox.x1 -= dx;
	cursorBox.x2 -= dx;
	cursorBox.y1 -= dy;
	cursorBox.y2 -= dy;
	if ((* pScreen->RectIn) (pRegion, &pScreenPriv->saved) != rgnOUT ||
	    (* pScreen->RectIn) (pRegion, &cursorBox) != rgnOUT)
	    (*pScreenPriv->CursorOff) (pScreen);
    }

    (*pScreen->CopyWindow) (pWin, ptOldOrg, pRegion);

    SCREEN_EPILOGUE (pScreen, CopyWindow, hpSpriteCopyWindow);
}

static void
hpSpriteClearToBackground (pWin, x, y, w, h, generateExposures)
    WindowPtr pWin;
    short x,y;
    unsigned short w,h;
    Bool generateExposures;
{
    ScreenPtr		pScreen;
    hpPrivScreenPtr	pScreenPriv;
    int			realw, realh;

    pScreen = pWin->drawable.pScreen;

    SCREEN_PROLOGUE (pScreen, ClearToBackground);

    pScreenPriv = getPrivScreenPtr(pScreen);
    if (pScreenPriv->cstate == CURSOR_ON)
    {
	if (!(realw = w))
	    realw = (int) pWin->drawable.width - x;
	if (!(realh = h))
	    realh = (int) pWin->drawable.height - y;
	if (ORG_OVERLAP(&pScreenPriv->saved, pWin->drawable.x, pWin->drawable.y,
			x, y, realw, realh))
	{
	    (*pScreenPriv->CursorOff) (pScreen);
	}
    }

    (*pScreen->ClearToBackground) (pWin, x, y, w, h, generateExposures);

    SCREEN_EPILOGUE (pScreen, ClearToBackground, hpSpriteClearToBackground);
}

/*
 * GC Func wrappers
 */

static void
hpSpriteValidateGC (pGC, changes, pDrawable)
    GCPtr	pGC;
    Mask	changes;
    DrawablePtr	pDrawable;
{
    GC_FUNC_PROLOGUE (pGC);

    (*pGC->funcs->ValidateGC) (pGC, changes, pDrawable);
    
    pGCPriv->wrapOps = NULL;
    if (pDrawable->type == DRAWABLE_WINDOW && ((WindowPtr) pDrawable)->viewable)
    {
	WindowPtr   pWin;
	RegionPtr   pRegion;

	pWin = (WindowPtr) pDrawable;
	pRegion = &pWin->clipList;
	if (pGC->subWindowMode == IncludeInferiors)
	    pRegion = &pWin->borderClip;
	if ((*pDrawable->pScreen->RegionNotEmpty) (pRegion))
	    pGCPriv->wrapOps = pGC->ops;
    }

    GC_FUNC_EPILOGUE (pGC);
}

static void
hpSpriteChangeGC (pGC, mask)
    GCPtr	    pGC;
    unsigned long   mask;
{
    GC_FUNC_PROLOGUE (pGC);

    (*pGC->funcs->ChangeGC) (pGC, mask);
    
    GC_FUNC_EPILOGUE (pGC);
}

static void
hpSpriteCopyGC (pGCSrc, mask, pGCDst)
    GCPtr	    pGCSrc, pGCDst;
    unsigned long   mask;
{
    GC_FUNC_PROLOGUE (pGCDst);

    (*pGCDst->funcs->CopyGC) (pGCSrc, mask, pGCDst);
    
    GC_FUNC_EPILOGUE (pGCDst);
}

static void
hpSpriteDestroyGC (pGC)
    GCPtr   pGC;
{
    GC_FUNC_PROLOGUE (pGC);

    (*pGC->funcs->DestroyGC) (pGC);
    
    GC_FUNC_EPILOGUE (pGC);
}

static void
hpSpriteChangeClip (pGC, type, pvalue, nrects)
    GCPtr   pGC;
    int		type;
    pointer	pvalue;
    int		nrects;
{
    GC_FUNC_PROLOGUE (pGC);

    (*pGC->funcs->ChangeClip) (pGC, type, pvalue, nrects);

    GC_FUNC_EPILOGUE (pGC);
}

static void
hpSpriteCopyClip(pgcDst, pgcSrc)
    GCPtr pgcDst, pgcSrc;
{
    GC_FUNC_PROLOGUE (pgcDst);

    (* pgcDst->funcs->CopyClip)(pgcDst, pgcSrc);

    GC_FUNC_EPILOGUE (pgcDst);
}

static void
hpSpriteDestroyClip(pGC)
    GCPtr	pGC;
{
    GC_FUNC_PROLOGUE (pGC);

    (* pGC->funcs->DestroyClip)(pGC);

    GC_FUNC_EPILOGUE (pGC);
}

/*
 * GC Op wrappers
 */

static void
hpSpriteFillSpans(pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		nInit;			/* number of spans to fill */
    DDXPointPtr pptInit;		/* pointer to list of start points */
    int		*pwidthInit;		/* pointer to list of n widths */
    int 	fSorted;
{
    GC_SETUP(pDrawable, pGC);

    if (GC_CHECK((WindowPtr) pDrawable))
    {
	register DDXPointPtr    pts;
	register int    	*widths;
	register int    	nPts;

	for (pts = pptInit, widths = pwidthInit, nPts = nInit;
	     nPts--;
	     pts++, widths++)
 	{
	     if (SPN_OVERLAP(&pScreenPriv->saved,pts->y,pts->x,*widths))
	     {
		 (*pScreenPriv->CursorOff) (pDrawable->pScreen);
		 break;
	     }
	}
    }

    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->FillSpans) (pDrawable, pGC, nInit, pptInit, pwidthInit, fSorted);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpriteSetSpans(pDrawable, pGC, psrc, ppt, pwidth, nspans, fSorted)
    DrawablePtr		pDrawable;
    GCPtr		pGC;
    int			*psrc;
    register DDXPointPtr ppt;
    int			*pwidth;
    int			nspans;
    int			fSorted;
{
    GC_SETUP(pDrawable, pGC);

    if (GC_CHECK((WindowPtr) pDrawable))
    {
	register DDXPointPtr    pts;
	register int    	*widths;
	register int    	nPts;

	for (pts = ppt, widths = pwidth, nPts = nspans;
	     nPts--;
	     pts++, widths++)
 	{
	     if (SPN_OVERLAP(&pScreenPriv->saved,pts->y,pts->x,*widths))
	     {
		 (*pScreenPriv->CursorOff) (pDrawable->pScreen);
		 break;
	     }
	}
    }

    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->SetSpans) (pDrawable, pGC, psrc, ppt, pwidth, nspans, fSorted);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpritePutImage(pDrawable, pGC, depth, x, y, w, h, leftPad, format, pBits)
    DrawablePtr	  pDrawable;
    GCPtr   	  pGC;
    int		  depth;
    int	    	  x;
    int	    	  y;
    int	    	  w;
    int	    	  h;
    int		  leftPad;
    int	    	  format;
    char    	  *pBits;
{
    GC_SETUP(pDrawable, pGC);

    if (GC_CHECK((WindowPtr) pDrawable))
    {
	if (ORG_OVERLAP(&pScreenPriv->saved,pDrawable->x,pDrawable->y,
			x,y,w,h))
 	{
	    (*pScreenPriv->CursorOff) (pDrawable->pScreen);
	}
    }

    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->PutImage) (pDrawable, pGC, depth, x, y, w, h, leftPad, format, pBits);

    GC_OP_EPILOGUE (pGC);
}

static RegionPtr
hpSpriteCopyArea (pSrc, pDst, pGC, srcx, srcy, w, h, dstx, dsty)
    DrawablePtr	  pSrc;
    DrawablePtr	  pDst;
    GCPtr   	  pGC;
    int	    	  srcx;
    int	    	  srcy;
    int	    	  w;
    int	    	  h;
    int	    	  dstx;
    int	    	  dsty;
{
    RegionPtr rgn;

    GC_SETUP(pDst, pGC);

    /* check destination/source overlap. */
    if (GC_CHECK((WindowPtr) pDst) &&
	 (ORG_OVERLAP(&pScreenPriv->saved,pDst->x,pDst->y,dstx,dsty,w,h) ||
	  ((pDst == pSrc) &&
	   ORG_OVERLAP(&pScreenPriv->saved,pSrc->x,pSrc->y,srcx,srcy,w,h))))
    {
	(*pScreenPriv->CursorOff) (pDst->pScreen);
    }
 
    GC_OP_PROLOGUE (pGC);

    rgn = (*pGC->ops->CopyArea) (pSrc, pDst, pGC, srcx, srcy, w, h,
				 dstx, dsty);

    GC_OP_EPILOGUE (pGC);

    return rgn;
}

static RegionPtr
hpSpriteCopyPlane (pSrc, pDst, pGC, srcx, srcy, w, h, dstx, dsty, plane)
    DrawablePtr	  pSrc;
    DrawablePtr	  pDst;
    register GC   *pGC;
    int     	  srcx,
		  srcy;
    int     	  w,
		  h;
    int     	  dstx,
		  dsty;
    unsigned long  plane;
{
    RegionPtr rgn;

    GC_SETUP(pDst, pGC);

    /*
     * check destination/source for overlap.
     */
    if (GC_CHECK((WindowPtr) pDst) &&
	(ORG_OVERLAP(&pScreenPriv->saved,pDst->x,pDst->y,dstx,dsty,w,h) ||
	 ((pDst == pSrc) &&
	  ORG_OVERLAP(&pScreenPriv->saved,pSrc->x,pSrc->y,srcx,srcy,w,h))))
    {
	(*pScreenPriv->CursorOff) (pDst->pScreen);
    }

    GC_OP_PROLOGUE (pGC);

    rgn = (*pGC->ops->CopyPlane) (pSrc, pDst, pGC, srcx, srcy, w, h,
				  dstx, dsty, plane);

    GC_OP_EPILOGUE (pGC);

    return rgn;
}

static void
hpSpritePolyPoint (pDrawable, pGC, mode, npt, pptInit)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		mode;		/* Origin or Previous */
    int		npt;
    xPoint 	*pptInit;
{
    xPoint	t;
    int		n;
    BoxRec	cursor;
    register xPoint *pts;

    GC_SETUP (pDrawable, pGC);

    if (npt && GC_CHECK((WindowPtr) pDrawable))
    {
	cursor.x1 = pScreenPriv->saved.x1 - pDrawable->x;
	cursor.y1 = pScreenPriv->saved.y1 - pDrawable->y;
	cursor.x2 = pScreenPriv->saved.x2 - pDrawable->x;
	cursor.y2 = pScreenPriv->saved.y2 - pDrawable->y;

	if (mode == CoordModePrevious)
	{
	    t.x = 0;
	    t.y = 0;
	    for (pts = pptInit, n = npt; n--; pts++)
	    {
		t.x += pts->x;
		t.y += pts->y;
		if (cursor.x1 <= t.x && t.x <= cursor.x2 &&
		    cursor.y1 <= t.y && t.y <= cursor.y2)
		{
		    (*pScreenPriv->CursorOff) (pDrawable->pScreen);
		    break;
		}
	    }
	}
	else
	{
	    for (pts = pptInit, n = npt; n--; pts++)
	    {
		if (cursor.x1 <= pts->x && pts->x <= cursor.x2 &&
		    cursor.y1 <= pts->y && pts->y <= cursor.y2)
		{
		    (*pScreenPriv->CursorOff) (pDrawable->pScreen);
		    break;
		}
	    }
	}
    }

    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->PolyPoint) (pDrawable, pGC, mode, npt, pptInit);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpritePolylines (pDrawable, pGC, mode, npt, pptInit)
    DrawablePtr	  pDrawable;
    GCPtr   	  pGC;
    int	    	  mode;
    int	    	  npt;
    DDXPointPtr	  pptInit;
{
    BoxPtr  cursor;
    register DDXPointPtr pts;
    int	    n;
    int	    x, y, x1, y1, x2, y2;
    int	    lw;
    int	    extra;

    GC_SETUP (pDrawable, pGC);

    if (npt && GC_CHECK((WindowPtr) pDrawable))
    {
	cursor = &pScreenPriv->saved;
	lw = pGC->lineWidth;
	x = pptInit->x + pDrawable->x;
	y = pptInit->y + pDrawable->y;

	if (npt == 1)
	{
	    extra = lw >> 1;
	    if (LINE_OVERLAP(cursor, x, y, x, y, extra))
		(*pScreenPriv->CursorOff) (pDrawable->pScreen);
	}
	else
	{
	    extra = lw >> 1;
	    /*
	     * mitered joins can project quite a way from
	     * the line end; the 11 degree miter limit limits
	     * this extension to 10.43 * lw / 2, rounded up
	     * and converted to int yields 6 * lw
	     */
	    if (pGC->joinStyle == JoinMiter)
		extra = 6 * lw;
	    else if (pGC->capStyle == CapProjecting)
		extra = lw;
	    for (pts = pptInit + 1, n = npt - 1; n--; pts++)
	    {
		x1 = x;
		y1 = y;
		if (mode == CoordModeOrigin)
		{
		    x2 = pDrawable->x + pts->x;
		    y2 = pDrawable->y + pts->y;
		}
		else
		{
		    x2 = x + pts->x;
		    y2 = y + pts->y;
		}
		x = x2;
		y = y2;
		LINE_SORT(x1, y1, x2, y2);
		if (LINE_OVERLAP(cursor, x1, y1, x2, y2, extra))
		{
		    (*pScreenPriv->CursorOff) (pDrawable->pScreen);
		    break;
		}
	    }
	}
    }
    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->Polylines) (pDrawable, pGC, mode, npt, pptInit);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpritePolySegment(pDrawable, pGC, nseg, pSegs)
    DrawablePtr pDrawable;
    GCPtr 	pGC;
    int		nseg;
    xSegment	*pSegs;
{
    int	    n;
    register xSegment *segs;
    BoxPtr  cursor;
    int	    x1, y1, x2, y2;
    int	    extra;

    GC_SETUP(pDrawable, pGC);

    if (nseg && GC_CHECK((WindowPtr) pDrawable))
    {
	cursor = &pScreenPriv->saved;
	extra = pGC->lineWidth >> 1;
	if (pGC->capStyle == CapProjecting)
	    extra = pGC->lineWidth;
	for (segs = pSegs, n = nseg; n--; segs++)
	{
	    x1 = segs->x1 + pDrawable->x;
	    y1 = segs->y1 + pDrawable->y;
	    x2 = segs->x2 + pDrawable->x;
	    y2 = segs->y2 + pDrawable->y;
	    LINE_SORT(x1, y1, x2, y2);
	    if (LINE_OVERLAP(cursor, x1, y1, x2, y2, extra))
	    {
		(*pScreenPriv->CursorOff) (pDrawable->pScreen);
		break;
	    }
	}
    }

    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->PolySegment) (pDrawable, pGC, nseg, pSegs);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpritePolyRectangle(pDrawable, pGC, nrects, pRects)
    DrawablePtr	pDrawable;
    GCPtr	pGC;
    int		nrects;
    xRectangle	*pRects;
{
    register xRectangle *rects;
    BoxPtr  cursor;
    int	    lw;
    int	    n;
    int     x1, y1, x2, y2;
    
    GC_SETUP (pDrawable, pGC);

    if (GC_CHECK((WindowPtr) pDrawable))
    {
	lw = pGC->lineWidth >> 1;
	cursor = &pScreenPriv->saved;
	for (rects = pRects, n = nrects; n--; rects++)
	{
	    x1 = rects->x + pDrawable->x;
	    y1 = rects->y + pDrawable->y;
	    x2 = x1 + (int)rects->width;
	    y2 = y1 + (int)rects->height;
	    if (LINE_OVERLAP(cursor, x1, y1, x2, y1, lw) ||
		LINE_OVERLAP(cursor, x2, y1, x2, y2, lw) ||
		LINE_OVERLAP(cursor, x1, y2, x2, y2, lw) ||
		LINE_OVERLAP(cursor, x1, y1, x1, y2, lw))
	    {
		(*pScreenPriv->CursorOff) (pDrawable->pScreen);
		break;
	    }
	}
    }

    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->PolyRectangle) (pDrawable, pGC, nrects, pRects);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpritePolyArc(pDrawable, pGC, narcs, parcs)
    DrawablePtr	pDrawable;
    register GCPtr pGC;
    int		narcs;
    xArc	*parcs;
{
    BoxPtr  cursor;
    int	    lw;
    int	    n;
    register xArc *arcs;
    
    GC_SETUP (pDrawable, pGC);

    if (GC_CHECK((WindowPtr) pDrawable))
    {
	lw = pGC->lineWidth >> 1;
	cursor = &pScreenPriv->saved;
	for (arcs = parcs, n = narcs; n--; arcs++)
	{
	    if (ORG_OVERLAP (cursor, pDrawable->x, pDrawable->y,
			     arcs->x - lw, arcs->y - lw,
			     (int) arcs->width + pGC->lineWidth,
 			     (int) arcs->height + pGC->lineWidth))
	    {
		(*pScreenPriv->CursorOff) (pDrawable->pScreen);
		break;
	    }
	}
    }

    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->PolyArc) (pDrawable, pGC, narcs, parcs);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpriteFillPolygon(pDrawable, pGC, shape, mode, count, pPts)
    register DrawablePtr pDrawable;
    register GCPtr	pGC;
    int			shape, mode;
    int			count;
    DDXPointPtr		pPts;
{
    int x, y, minx, miny, maxx, maxy;
    register DDXPointPtr pts;
    int n;

    GC_SETUP (pDrawable, pGC);

    if (count && GC_CHECK((WindowPtr) pDrawable))
    {
	x = pDrawable->x;
	y = pDrawable->y;
	pts = pPts;
	minx = maxx = pts->x;
	miny = maxy = pts->y;
	pts++;
	n = count - 1;

	if (mode == CoordModeOrigin)
	{
	    for (; n--; pts++)
	    {
		if (pts->x < minx)
		    minx = pts->x;
		else if (pts->x > maxx)
		    maxx = pts->x;
		if (pts->y < miny)
		    miny = pts->y;
		else if (pts->y > maxy)
		    maxy = pts->y;
	    }
	    minx += x;
	    miny += y;
	    maxx += x;
	    maxy += y;
	}
	else
	{
	    x += minx;
	    y += miny;
	    minx = maxx = x;
	    miny = maxy = y;
	    for (; n--; pts++)
	    {
		x += pts->x;
		y += pts->y;
		if (x < minx)
		    minx = x;
		else if (x > maxx)
		    maxx = x;
		if (y < miny)
		    miny = y;
		else if (y > maxy)
		    maxy = y;
	    }
	}
	if (BOX_OVERLAP(&pScreenPriv->saved,minx,miny,maxx,maxy))
	    (*pScreenPriv->CursorOff) (pDrawable->pScreen);
    }

    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->FillPolygon) (pDrawable, pGC, shape, mode, count, pPts);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpritePolyFillRect(pDrawable, pGC, nrectFill, prectInit)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		nrectFill; 	/* number of rectangles to fill */
    xRectangle	*prectInit;  	/* Pointer to first rectangle to fill */
{
    GC_SETUP(pDrawable, pGC);

    if (GC_CHECK((WindowPtr) pDrawable))
    {
	register int	    nRect;
	register xRectangle *pRect;
	register int	    xorg, yorg;

	xorg = pDrawable->x;
	yorg = pDrawable->y;

	for (nRect = nrectFill, pRect = prectInit; nRect--; pRect++) {
	    if (ORGRECT_OVERLAP(&pScreenPriv->saved,xorg,yorg,pRect)){
		(*pScreenPriv->CursorOff) (pDrawable->pScreen);
		break;
	    }
	}
    }

    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->PolyFillRect) (pDrawable, pGC, nrectFill, prectInit);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpritePolyFillArc(pDrawable, pGC, narcs, parcs)
    DrawablePtr	pDrawable;
    GCPtr	pGC;
    int		narcs;
    xArc	*parcs;
{
    GC_SETUP(pDrawable, pGC);

    if (GC_CHECK((WindowPtr) pDrawable))
    {
	register int	n;
	BoxPtr		cursor;
	register xArc *arcs;

	cursor = &pScreenPriv->saved;

	for (arcs = parcs, n = narcs; n--; arcs++)
	{
	    if (ORG_OVERLAP(cursor, pDrawable->x, pDrawable->y,
			    arcs->x, arcs->y,
 			    (int) arcs->width, (int) arcs->height))
	    {
		(*pScreenPriv->CursorOff) (pDrawable->pScreen);
		break;
	    }
	}
    }

    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->PolyFillArc) (pDrawable, pGC, narcs, parcs);

    GC_OP_EPILOGUE (pGC);
}

/*
 * general Poly/Image text function.  Extract glyph information,
 * compute bounding box and remove cursor if it is overlapped.
 */

static Bool
hpSpriteTextOverlap (pDraw, font, x, y, n, charinfo, imageblt, w, cursorBox)
    DrawablePtr   pDraw;
    FontPtr	  font;
    int		  x, y;
    unsigned long n;
    CharInfoPtr   *charinfo;
    Bool	  imageblt;
    unsigned int  w;
    BoxPtr	  cursorBox;
{
    ExtentInfoRec extents;

    x += pDraw->x;
    y += pDraw->y;

    if (FONTMINBOUNDS(font,characterWidth) >= 0)
    {
	/* compute an approximate (but covering) bounding box */
	if (!imageblt || (charinfo[0]->metrics.leftSideBearing < 0))
	    extents.overallLeft = charinfo[0]->metrics.leftSideBearing;
	else
	    extents.overallLeft = 0;
	if (w)
	    extents.overallRight = w - charinfo[n-1]->metrics.characterWidth;
	else
	    extents.overallRight = FONTMAXBOUNDS(font,characterWidth)
				    * (n - 1);
	if (imageblt && (charinfo[n-1]->metrics.characterWidth >
			 charinfo[n-1]->metrics.rightSideBearing))
	    extents.overallRight += charinfo[n-1]->metrics.characterWidth;
	else
	    extents.overallRight += charinfo[n-1]->metrics.rightSideBearing;
	if (imageblt && FONTASCENT(font) > FONTMAXBOUNDS(font,ascent))
	    extents.overallAscent = FONTASCENT(font);
	else
	    extents.overallAscent = FONTMAXBOUNDS(font, ascent);
	if (imageblt && FONTDESCENT(font) > FONTMAXBOUNDS(font,descent))
	    extents.overallDescent = FONTDESCENT(font);
	else
	    extents.overallDescent = FONTMAXBOUNDS(font,descent);
	if (!BOX_OVERLAP(cursorBox,
			 x + extents.overallLeft,
			 y - extents.overallAscent,
			 x + extents.overallRight,
			 y + extents.overallDescent))
	    return FALSE;
	else if (imageblt && w)
	    return TRUE;
	/* if it does overlap, fall through and compute exactly, because
	 * taking down the cursor is expensive enough to make this worth it
	 */
    }
    QueryGlyphExtents(font, charinfo, n, &extents);
    if (imageblt)
    {
	if (extents.overallWidth > extents.overallRight)
	    extents.overallRight = extents.overallWidth;
	if (extents.overallWidth < extents.overallLeft)
	    extents.overallLeft = extents.overallWidth;
	if (extents.overallLeft > 0)
	    extents.overallLeft = 0;
	if (extents.fontAscent > extents.overallAscent)
	    extents.overallAscent = extents.fontAscent;
	if (extents.fontDescent > extents.overallDescent)
	    extents.overallDescent = extents.fontDescent;
    }
    return (BOX_OVERLAP(cursorBox,
			x + extents.overallLeft,
			y - extents.overallAscent,
			x + extents.overallRight,
			y + extents.overallDescent));
}

/*
 * values for textType:
 */
#define TT_POLY8   0
#define TT_IMAGE8  1
#define TT_POLY16  2
#define TT_IMAGE16 3

static int 
hpSpriteText (pDraw, pGC, x, y, count, chars, fontEncoding, textType, cursorBox)
    DrawablePtr	    pDraw;
    GCPtr	    pGC;
    int		    x,
		    y;
    unsigned long    count;
    char	    *chars;
    FontEncoding    fontEncoding;
    Bool	    textType;
    BoxPtr	    cursorBox;
{
    CharInfoPtr *charinfo;
    register CharInfoPtr *info;
    unsigned long n, i;
    unsigned int w;
    void    	  (*drawFunc)();

    Bool imageblt;

    imageblt = (textType == TT_IMAGE8) || (textType == TT_IMAGE16);

    charinfo = (CharInfoPtr *) ALLOCATE_LOCAL(count * sizeof(CharInfoPtr));
    if (!charinfo)
	return x;

    GetGlyphs(pGC->font, count, (unsigned char *)chars,
	      fontEncoding, &n, charinfo);
    w = 0;
    if (!imageblt)
	for (info = charinfo, i = n; i--; info++)
	    w += (*info)->metrics.characterWidth;

    if (n != 0) {
	if (hpSpriteTextOverlap(pDraw, pGC->font, x, y, n, charinfo, imageblt, w, cursorBox))
	    (*getPrivScreenPtr(pDraw->pScreen)->CursorOff) (pDraw->pScreen);

#define AVOID_GLYPHBLT
#ifdef AVOID_GLYPHBLT
	/*
	 * On displays like Apollos, which do not optimize the GlyphBlt functions because they
	 * convert fonts to their internal form in RealizeFont and optimize text directly, we
	 * want to invoke the text functions here, not the GlyphBlt functions.
	 */
	switch (textType)
	{
	case TT_POLY8:
	    drawFunc = pGC->ops->PolyText8;
	    break;
	case TT_IMAGE8:
	    drawFunc = pGC->ops->ImageText8;
	    break;
	case TT_POLY16:
	    drawFunc = pGC->ops->PolyText16;
	    break;
	case TT_IMAGE16:
	    drawFunc = pGC->ops->ImageText16;
	    break;
	}
	(*drawFunc) (pDraw, pGC, x, y, (int) count, chars);
#else /* don't AVOID_GLYPHBLT */
	/*
	 * On the other hand, if the device does use GlyphBlt ultimately to do text, we
	 * don't want to slow it down by invoking the text functions and having them call
	 * GetGlyphs all over again, so we go directly to the GlyphBlt functions here.
	 */
	drawFunc = imageblt ? pGC->ops->ImageGlyphBlt : pGC->ops->PolyGlyphBlt;
	(*drawFunc) (pDraw, pGC, x, y, n, charinfo,  FONTGLYPHS(pGC->font));
#endif /* AVOID_GLYPHBLT */
    }
    DEALLOCATE_LOCAL(charinfo);
    return x + w;
}

static int
hpSpritePolyText8(pDrawable, pGC, x, y, count, chars)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		x, y;
    int 	count;
    char	*chars;
{
    int	ret;

    GC_SETUP (pDrawable, pGC);

    GC_OP_PROLOGUE (pGC);

    if (GC_CHECK((WindowPtr) pDrawable))
	ret = hpSpriteText (pDrawable, pGC, x, y, (unsigned long)count, chars,
			    Linear8Bit, TT_POLY8, &pScreenPriv->saved);
    else
	ret = (*pGC->ops->PolyText8) (pDrawable, pGC, x, y, count, chars);

    GC_OP_EPILOGUE (pGC);
    return ret;
}

static int
hpSpritePolyText16(pDrawable, pGC, x, y, count, chars)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		x, y;
    int		count;
    unsigned short *chars;
{
    int	ret;

    GC_SETUP(pDrawable, pGC);

    GC_OP_PROLOGUE (pGC);

    if (GC_CHECK((WindowPtr) pDrawable))
	ret = hpSpriteText (pDrawable, pGC, x, y, (unsigned long)count,
			    (char *)chars,
			    FONTLASTROW(pGC->font) == 0 ?
			    Linear16Bit : TwoD16Bit, TT_POLY16, &pScreenPriv->saved);
    else
	ret = (*pGC->ops->PolyText16) (pDrawable, pGC, x, y, count, chars);

    GC_OP_EPILOGUE (pGC);
    return ret;
}

static void
hpSpriteImageText8(pDrawable, pGC, x, y, count, chars)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		x, y;
    int		count;
    char	*chars;
{
    GC_SETUP(pDrawable, pGC);

    GC_OP_PROLOGUE (pGC);

    if (GC_CHECK((WindowPtr) pDrawable))
	(void) hpSpriteText (pDrawable, pGC, x, y, (unsigned long)count,
			     chars, Linear8Bit, TT_IMAGE8, &pScreenPriv->saved);
    else
	(*pGC->ops->ImageText8) (pDrawable, pGC, x, y, count, chars);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpriteImageText16(pDrawable, pGC, x, y, count, chars)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int		x, y;
    int		count;
    unsigned short *chars;
{
    GC_SETUP(pDrawable, pGC);

    GC_OP_PROLOGUE (pGC);

    if (GC_CHECK((WindowPtr) pDrawable))
	(void) hpSpriteText (pDrawable, pGC, x, y, (unsigned long)count,
			     (char *)chars,
			    FONTLASTROW(pGC->font) == 0 ?
			    Linear16Bit : TwoD16Bit, TT_IMAGE16, &pScreenPriv->saved);
    else
	(*pGC->ops->ImageText16) (pDrawable, pGC, x, y, count, chars);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpriteImageGlyphBlt(pDrawable, pGC, x, y, nglyph, ppci, pglyphBase)
    DrawablePtr pDrawable;
    GC 		*pGC;
    int 	x, y;
    unsigned long nglyph;
    CharInfoPtr *ppci;		/* array of character info */
    pointer 	pglyphBase;	/* start of array of glyphs */
{
    GC_SETUP(pDrawable, pGC);

    GC_OP_PROLOGUE (pGC);

    if (GC_CHECK((WindowPtr) pDrawable) &&
	hpSpriteTextOverlap (pDrawable, pGC->font, x, y, nglyph, ppci, TRUE, 0, &pScreenPriv->saved))
    {
	(*pScreenPriv->CursorOff) (pDrawable->pScreen);
    }
    (*pGC->ops->ImageGlyphBlt) (pDrawable, pGC, x, y, nglyph, ppci, pglyphBase);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpritePolyGlyphBlt(pDrawable, pGC, x, y, nglyph, ppci, pglyphBase)
    DrawablePtr pDrawable;
    GCPtr	pGC;
    int 	x, y;
    unsigned long nglyph;
    CharInfoPtr *ppci;		/* array of character info */
    char 	*pglyphBase;	/* start of array of glyphs */
{
    GC_SETUP (pDrawable, pGC);

    GC_OP_PROLOGUE (pGC);

    if (GC_CHECK((WindowPtr) pDrawable) &&
	hpSpriteTextOverlap (pDrawable, pGC->font, x, y, nglyph, ppci, FALSE, 0, &pScreenPriv->saved))
    {
	(*pScreenPriv->CursorOff) (pDrawable->pScreen);
    }
    (*pGC->ops->PolyGlyphBlt) (pDrawable, pGC, x, y, nglyph, ppci, pglyphBase);

    GC_OP_EPILOGUE (pGC);
}

static void
hpSpritePushPixels(pGC, pBitMap, pDrawable, w, h, x, y)
    GCPtr	pGC;
    PixmapPtr	pBitMap;
    DrawablePtr pDrawable;
    int		w, h, x, y;
{
    GC_SETUP(pDrawable, pGC);

    if (GC_CHECK((WindowPtr) pDrawable) &&
	ORG_OVERLAP(&pScreenPriv->saved,pDrawable->x,pDrawable->y,x,y,w,h))
    {
	(*pScreenPriv->CursorOff) (pDrawable->pScreen);
    }

    GC_OP_PROLOGUE (pGC);

    (*pGC->ops->PushPixels) (pGC, pBitMap, pDrawable, w, h, x, y);

    GC_OP_EPILOGUE (pGC);
}

/*
 * I don't expect this routine will ever be called, as the GC
 * will have been unwrapped for the line drawing
 */

static void
hpSpriteLineHelper()
{
    FatalError("hpSpriteLineHelper called\n");
}
