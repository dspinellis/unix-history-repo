/*

Copyright 1990 by the Massachusetts Institute of Technology

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.

*/

/* $XConsortium: miscrinit.c,v 5.2 91/05/14 11:17:40 rws Exp $ */

#include "X.h"
#include "servermd.h"
#include "misc.h"
#include "mi.h"
#include "scrnintstr.h"
#include "pixmapstr.h"
#include "mibstore.h"
#include "dix.h"
#include "hppriv.h"

/*ARGSUSED*/
static Bool
hpCloseScreen (index, pScreen)
    int		index;
    ScreenPtr	pScreen;
{
    (void) xfree (getPrivScreenPtr(pScreen)->pDrawable);
    return TRUE;
}

/*
 * If you pass in bsfuncs, then you must preinitialize the missing
 * screen procs before calling miScreenInit, so that the backing store
 * code can correctly wrap them.
 */

Bool
hpScreenInit(pScreen, pbits, xsize, ysize, dpix, dpiy, /* width, */
	     rootDepth, numDepths, depths, rootVisual, numVisuals, visuals,
	     bsfuncs)
    register ScreenPtr pScreen;
    pointer pbits;		/* pointer to screen bits */
    int xsize, ysize;		/* in pixels */
    int dpix, dpiy;		/* dots per inch */
#if 0
    int width;			/* pixel width of frame buffer */
#endif
    int rootDepth;		/* depth of root window */
    int numDepths;		/* number of depths supported */
    DepthRec *depths;		/* supported depths */
    VisualID rootVisual;	/* root visual */
    int numVisuals;		/* number of visuals supported */
    VisualRec *visuals;		/* supported visuals */
    miBSFuncPtr	bsfuncs;	/* backing store functions */
{
    pScreen->width = xsize;
    pScreen->height = ysize;
    pScreen->mmWidth = (xsize * 254) / (dpix * 10);
    pScreen->mmHeight = (ysize * 254) / (dpiy * 10);
    pScreen->numDepths = numDepths;
    pScreen->rootDepth = rootDepth;
    pScreen->allowedDepths = depths;
    pScreen->rootVisual = rootVisual;
    /* defColormap */
    pScreen->minInstalledCmaps = 1;
    pScreen->maxInstalledCmaps = 1;
    pScreen->backingStoreSupport = Always;
    pScreen->saveUnderSupport = NotUseful;
    /* whitePixel, blackPixel */
#if 0
    if (width)
#endif
    {
	PixmapPtr pPixmap;
	hpPrivPixmapPtr pPrivPixmap;
	hpPrivScreenPtr pPrivScreen;

	pPixmap = (PixmapPtr) xalloc(sizeof(PixmapRec) + sizeof(hpPrivPixmap));
	if (!pPixmap)
	    return FALSE;
	pPrivPixmap = (hpPrivPixmapPtr) (pPixmap+1);

	pPrivScreen = getPrivScreenPtr(pScreen);
	pPrivScreen->bits = pbits;
	pPrivScreen->stride = PixmapBytePad(pPrivScreen->memWidth,
					    rootDepth);
	pPrivScreen->pDrawable = (DrawablePtr) pPixmap;
	pPrivScreen->isBlank = FALSE;
	pScreen->devPrivate = (pointer)pPrivScreen;

	pPixmap->drawable.type = DRAWABLE_PIXMAP;
	pPixmap->drawable.depth = rootDepth;
	pPixmap->drawable.bitsPerPixel = rootDepth;
	pPixmap->drawable.pScreen = pScreen;
	pPixmap->drawable.serialNumber = 0;
	pPixmap->drawable.x = 0;
	pPixmap->drawable.y = 0;
	pPixmap->drawable.width = xsize;
	pPixmap->drawable.height = ysize;
	pPixmap->refcnt = 1;
	pPixmap->devPrivate.ptr = (pointer) pPrivPixmap;
	pPixmap->devKind = PIXMAP_FRAME_BUFFER;

	hpBufAllocInit(pScreen, pPrivScreen->memWidth, pPrivScreen->memHeight,
		       xsize, ysize); /* set up for off-screen memory use */

	pPrivPixmap->pChunk = hpBufAlloc(pScreen, xsize, ysize);
	pPrivPixmap->bits = pbits;
	pPrivPixmap->stride = pPrivScreen->stride;

#ifdef MITSHM
	ShmRegisterFbFuncs(pScreen);
#endif
    }
#if 0
    else
	pScreen->devPrivate = pbits;
#endif
    pScreen->numVisuals = numVisuals;
    pScreen->visuals = visuals;
#if 0
    if (width)
#endif
	pScreen->CloseScreen = hpCloseScreen;
    /* else CloseScreen */
    /* QueryBestSize, SaveScreen, GetImage, GetSpans */
    pScreen->PointerNonInterestBox = (void (*)()) 0;
    pScreen->SourceValidate = (void (*)()) 0;
    /* CreateWindow, DestroyWindow, PositionWindow, ChangeWindowAttributes */
    /* RealizeWindow, UnrealizeWindow */
    pScreen->ValidateTree = miValidateTree;
    pScreen->PostValidateTree = (void (*)()) 0;
    pScreen->WindowExposures = miWindowExposures;
    /* PaintWindowBackground, PaintWindowBorder, CopyWindow */
    pScreen->ClearToBackground = miClearToBackground;
    pScreen->ClipNotify = (void (*)()) 0;
    /* CreatePixmap, DestroyPixmap */
    /* RealizeFont, UnrealizeFont */
    /* CreateGC */
    /* CreateColormap, DestroyColormap, InstallColormap, UninstallColormap */
    /* ListInstalledColormaps, StoreColors, ResolveColor */
    pScreen->RegionCreate = miRegionCreate;
    pScreen->RegionInit = miRegionInit;
    pScreen->RegionCopy = miRegionCopy;
    pScreen->RegionDestroy = miRegionDestroy;
    pScreen->RegionUninit = miRegionUninit;
    pScreen->Intersect = miIntersect;
    pScreen->Union = miUnion;
    pScreen->Subtract = miSubtract;
    pScreen->Inverse = miInverse;
    pScreen->RegionReset = miRegionReset;
    pScreen->TranslateRegion = miTranslateRegion;
    pScreen->RectIn = miRectIn;
    pScreen->PointInRegion = miPointInRegion;
    pScreen->RegionNotEmpty = miRegionNotEmpty;
    pScreen->RegionEmpty = miRegionEmpty;
    pScreen->RegionExtents = miRegionExtents;
    pScreen->RegionAppend = miRegionAppend;
    pScreen->RegionValidate = miRegionValidate;
    /* BitmapToRegion */
    pScreen->RectsToRegion = miRectsToRegion;
    pScreen->SendGraphicsExpose = miSendGraphicsExpose;
    pScreen->BlockHandler = NoopDDA;
    pScreen->WakeupHandler = NoopDDA;
    pScreen->blockData = (pointer)0;
    pScreen->wakeupData = (pointer)0;
    if (bsfuncs)
	miInitializeBackingStore (pScreen, bsfuncs);
    return TRUE;
}
