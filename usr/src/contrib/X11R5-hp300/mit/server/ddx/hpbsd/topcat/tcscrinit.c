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

#include "X.h"
#include "Xmd.h"
#include "servermd.h"
#include "scrnintstr.h"
#include "pixmapstr.h"
#include "resource.h"
#include "colormap.h"
#include "colormapst.h"
#include "../cfb/cfb.h"
#include "mi.h"
#include "mistruct.h"
#include "dix.h"
#include "../cfb/cfbmskbits.h"
#include "mibstore.h"
#include "topcat.h"

extern RegionPtr mfbPixmapToRegion();
extern Bool mfbAllocatePrivates();

extern Bool hpRealizeFont(), hpUnrealizeFont();
extern void hpGetByteImage();

extern int defaultColorVisualClass;

#define _BP 8
#define _RZ ((PSZ + 2) / 3)
#define _RS 0
#define _RM ((1 << _RZ) - 1)
#define _GZ ((PSZ - _RZ + 1) / 2)
#define _GS _RZ
#define _GM (((1 << _GZ) - 1) << _GS)
#define _BZ (PSZ - _RZ - _GZ)
#define _BS (_RZ + _GZ)
#define _BM (((1 << _BZ) - 1) << _BS)
#define _CE (1 << _RZ)

#if 0
static VisualRec visuals[] = {
/* vid  class        bpRGB cmpE nplan rMask gMask bMask oRed oGreen oBlue */
#ifndef STATIC_COLOR
    0,  PseudoColor, _BP,  1<<PSZ,   PSZ,  0,   0,   0,   0,   0,   0,
    0,  DirectColor, _BP, _CE,       PSZ,  _RM, _GM, _BM, _RS, _GS, _BS,
    0,  GrayScale,   _BP,  1<<PSZ,   PSZ,  0,   0,   0,   0,   0,   0,
    0,  StaticGray,  _BP,  1<<PSZ,   PSZ,  0,   0,   0,   0,   0,   0,
#endif
    0,  StaticColor, _BP,  1<<PSZ,   PSZ,  _RM, _GM, _BM, _RS, _GS, _BS,
    0,  TrueColor,   _BP, _CE,       PSZ,  _RM, _GM, _BM, _RS, _GS, _BS
};

#define	NUMVISUALS	((sizeof visuals)/(sizeof visuals[0]))

static  VisualID VIDs[NUMVISUALS];

static DepthRec depths[] = {
/* depth	numVid		vids */
    1,		0,		NULL,
    8,		NUMVISUALS,	VIDs
};

#define NUMDEPTHS	((sizeof depths)/(sizeof depths[0]))

#else

/* vid  class        bpRGB cmpE nplan rMask gMask bMask oRed oGreen oBlue */
static VisualRec PseudoColorVisual = {
    0,  PseudoColor, _BP,  1<<PSZ,   PSZ,  0,   0,   0,   0,   0,   0,
};

static VisualRec StaticGrayVisual = {
    0,  StaticGray,  1,  2,   1,  0,   0,   0,   0,   0,   0,
};

static DepthRec depth1 = {
/* depth        numVid          vids */
    1,          0,              NULL
};

static DepthRec depth8 = {
/* depth        numVid          vids */
    8,          1,              NULL
};

#endif

int cfbWindowPrivateIndex;
int cfbGCPrivateIndex;
static unsigned long hpfbGeneration = 0;

extern void hpSaveAreas(), hpRestoreAreas();

static miBSFuncRec tcBSFuncRec = {
    hpSaveAreas,
    hpRestoreAreas,
    (void (*)()) 0,
    (PixmapPtr (*)()) 0,
    (PixmapPtr (*)()) 0,
};

/*ARGSUSED*/
Bool
tcCloseScreen (index, pScreen)
    int		index;
    ScreenPtr	pScreen;
{
    xfree (pScreen->visuals);
    xfree (pScreen->allowedDepths);
    (void) cfbDestroyPixmap (getPrivScreenPtr(pScreen)->pTmpPixmap);
    (void) cfbDestroyPixmap (getPrivScreenPtr(pScreen)->pDrawable);
    return TRUE;
}

/* dts * (inch/dot) * (25.4 mm / inch) = mm */
Bool
tcScreenInit(pScreen, pbits, xsize, ysize, dpix, dpiy, numPlanes)
    register ScreenPtr pScreen;
    pointer pbits;		/* pointer to screen bitmap */
    int xsize, ysize;		/* in pixels */
    int dpix, dpiy;		/* dots per inch */
    int numPlanes;		/* number of planes in frame buffer */
{
    int	i;
    int numVisuals;
    int numDepths;
    VisualRec *visuals;
    DepthRec *depths;
    
    if (hpfbGeneration != serverGeneration)
    {
	numVisuals = 1;
	visuals = (VisualRec *) xalloc(numVisuals*sizeof(VisualRec));
	if (numPlanes == 1)
	    bcopy(&StaticGrayVisual, &(visuals[0]), sizeof(VisualRec));
	else {
	    bcopy(&PseudoColorVisual, &(visuals[0]), sizeof(VisualRec));
	    if (numPlanes != PSZ) {
		visuals[0].nplanes = numPlanes;
		visuals[0].ColormapEntries = 1 << numPlanes;
		if (numPlanes < 4)
		    visuals[0].class = StaticGray;
	    }
	}

	/*  Set up the visual IDs */
	visuals[0].vid = FakeClientID(0);

        numDepths = 2;
        depths =  (DepthRec *) xalloc(numDepths*sizeof(DepthRec));
        bcopy(&depth8, &(depths[0]), sizeof(DepthRec));
        bcopy(&depth1, &(depths[1]), sizeof(DepthRec));

	/*  Set up the remaining fields in the depths[] array */
	for (i = 0; i < numDepths; i++)
	{
	    if (depths[i].numVids > 0)
	    {
		depths[i].vids = (VisualID *)
		    xalloc(sizeof (VisualID) * depths[i].numVids);
		/* XXX - here we offer only the 8-bit visual */
		depths[i].vids[0] = visuals[0].vid;
	    }
	}
	hpfbGeneration = serverGeneration;
    }
    if (!mfbAllocatePrivates(pScreen,
			     &cfbWindowPrivateIndex, &cfbGCPrivateIndex))
	return FALSE;
    if (!AllocateWindowPrivate(pScreen, cfbWindowPrivateIndex,
			       sizeof(cfbPrivWin)) ||
	!AllocateGCPrivate(pScreen, cfbGCPrivateIndex, sizeof(cfbPrivGC)))
	return FALSE;

    pScreen->defColormap = FakeClientID(0);
    /* let CreateDefColormap do whatever it wants */ 
    pScreen->blackPixel = pScreen->whitePixel = (Pixel) 0;
    pScreen->QueryBestSize = mfbQueryBestSize;
    /* SaveScreen */
    pScreen->GetImage = hpGetByteImage;
    pScreen->GetSpans = cfbGetSpans;
    pScreen->CreateWindow = cfbCreateWindow;
    pScreen->DestroyWindow = cfbDestroyWindow;
    pScreen->PositionWindow = cfbPositionWindow;
    pScreen->ChangeWindowAttributes = cfbChangeWindowAttributes;
    pScreen->RealizeWindow = cfbMapWindow;
    pScreen->UnrealizeWindow = cfbUnmapWindow;
    pScreen->PaintWindowBackground = tcPaintWindow;
    pScreen->PaintWindowBorder = tcPaintWindow;
    pScreen->CopyWindow = cfbCopyWindow;
    pScreen->CreatePixmap = cfbCreatePixmap;
    pScreen->DestroyPixmap = cfbDestroyPixmap;
    pScreen->RealizeFont = hpRealizeFont;
    pScreen->UnrealizeFont = hpUnrealizeFont;
    pScreen->CreateGC = topcatCreateGC;
    pScreen->CreateColormap = cfbInitializeColormap;
    pScreen->DestroyColormap = NoopDDA;
#ifdef	STATIC_COLOR
    pScreen->InstallColormap = cfbInstallColormap;
    pScreen->UninstallColormap = cfbUninstallColormap;
    pScreen->ListInstalledColormaps = cfbListInstalledColormaps;
    pScreen->StoreColors = NoopDDA;
    pScreen->ResolveColor = cfbResolveColor;
#else
    pScreen->InstallColormap = topcatInstallColormap;
    pScreen->UninstallColormap = topcatUninstallColormap;
    pScreen->ListInstalledColormaps = topcatListInstalledColormaps;
    pScreen->StoreColors = topcatStoreColors;
    pScreen->ResolveColor = topcatResolvePseudoColor;
#endif
    pScreen->BitmapToRegion = mfbPixmapToRegion;

    if (!hpScreenInit(pScreen, pbits, xsize, ysize, dpix, dpiy, /*width,*/
		      depths[0].depth, numDepths, depths,
		      visuals[0].vid, numVisuals, visuals,
		      &tcBSFuncRec))
	return FALSE;

    getPrivScreenPtr(pScreen)->pTmpPixmap =
	(pointer) cfbCreatePixmap(pScreen, PRIV_PIX_WIDTH, PRIV_PIX_HEIGHT,
				  pScreen->rootDepth);
    if (!getPrivScreenPtr(pScreen)->pTmpPixmap)
    {
	(void) cfbDestroyPixmap (getPrivScreenPtr(pScreen)->pDrawable);
	return FALSE;
    }
    return TRUE;
}
