/*

*/
#include "X.h"
#define  NEED_EVENTS
#include "Xproto.h"
#include "scrnintstr.h"
#include "pixmapstr.h"
#include "inputstr.h"
#include "regionstr.h"

#include "hppriv.h"
#include "topcat.h"
#include "windowstr.h"
#include "colormapst.h"
#include "resource.h"


/*-
 *-----------------------------------------------------------------------
 * topcatInstallColormap --
 *	Install given colormap.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	All clients requesting ColormapNotify are notified
 *
 *-----------------------------------------------------------------------
 */
void
topcatInstallColormap(cmap)
    ColormapPtr	cmap;
{
    register int i;
    Entry *pent = cmap->red;
    u_char	  rmap[256], gmap[256], bmap[256];
    ColormapPtr	  topcatInstalledMap;
    topcatPrivPtr topcat =
      ((topcatPrivPtr) getPrivScreenPtr(cmap->pScreen)->pHardwareScreen);
    extern int TellLostMap(), TellGainedMap();

    topcatInstalledMap = topcat->InstalledMap;
    if (cmap == topcatInstalledMap)
	return;
    if (topcatInstalledMap)
	WalkTree(topcatInstalledMap->pScreen, TellLostMap,
		 (char *) &(topcatInstalledMap->mid));
    for (i = 0; i < cmap->pVisual->ColormapEntries; i++)
    {
	if (pent->fShared)
	{
	    rmap[i] = pent->co.shco.red->color >> 8;
	    gmap[i] = pent->co.shco.green->color >> 8;
	    bmap[i] = pent->co.shco.blue->color >> 8;
	}
	else
	{
	    rmap[i] = pent->co.local.red >> 8;
	    gmap[i] = pent->co.local.green >> 8;
	    bmap[i] = pent->co.local.blue >> 8;
	}
	pent++;
    }
    topcat->InstalledMap = cmap;
    (*topcat->UpdateColormap)(cmap->pScreen,
			 0, cmap->pVisual->ColormapEntries, rmap, gmap, bmap);
    WalkTree(cmap->pScreen, TellGainedMap, (char *) &(cmap->mid));
}

/*-
 *-----------------------------------------------------------------------
 * topcatUninstallColormap --
 *	Uninstall given colormap.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	All clients requesting ColormapNotify are notified
 *
 *-----------------------------------------------------------------------
 */
void
topcatUninstallColormap(cmap)
    ColormapPtr	cmap;
{
    ScreenPtr	pScreen = cmap->pScreen;

    if (cmap == ((topcatPrivPtr) getPrivScreenPtr(pScreen)->pHardwareScreen)
	                                ->InstalledMap)
    {
	Colormap defMapID = pScreen->defColormap;

	if (cmap->mid != defMapID)
	{
	    ColormapPtr defMap = (ColormapPtr)
		LookupIDByType(defMapID, RT_COLORMAP);
	    (*pScreen->InstallColormap)(defMap);
	}
    }
}

/*-
 *-----------------------------------------------------------------------
 * topcatListInstalledColormaps --
 *	Fills in the list with the IDs of the installed maps
 *
 * Results:
 *	Returns the number of IDs in the list
 *
 * Side Effects:
 *	None
 *
 *-----------------------------------------------------------------------
 */
int
topcatListInstalledColormaps(pScreen, pCmapList)
    ScreenPtr	pScreen;
    Colormap	*pCmapList;
{
    *pCmapList =
      ((topcatPrivPtr) getPrivScreenPtr(pScreen)->pHardwareScreen)->
	  InstalledMap->mid;
    return (1);
}


/*-
 *-----------------------------------------------------------------------
 * topcatStoreColors --
 *	Sets the pixels in pdefs into the specified map.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	None
 *
 *-----------------------------------------------------------------------
 */
void
topcatStoreColors(pmap, ndef, pdefs)
    ColormapPtr	pmap;
    int		ndef;
    xColorItem	*pdefs;
{
    topcatPrivPtr topcat = (topcatPrivPtr) 
	getPrivScreenPtr(pmap->pScreen)->pHardwareScreen;

    switch (pmap->class)
    {
      case PseudoColor:
	if (pmap == topcat->InstalledMap)
	{
	    /* We only have a single colormap */
	    u_char	rmap[256], gmap[256], bmap[256];

	    while (ndef--)
	    {
		register unsigned index = pdefs->pixel&0xff;
		EntryPtr pEntry = pmap->red + index;

		if (pEntry->fShared)
		{
		    rmap[index] = pEntry->co.shco.red->color >> 8;
		    gmap[index] = pEntry->co.shco.green->color >> 8;
		    bmap[index] = pEntry->co.shco.blue->color >> 8;
		}
		else
		{
		    rmap[index] = pEntry->co.local.red >> 8;
		    gmap[index] = pEntry->co.local.green >> 8;
		    bmap[index] = pEntry->co.local.blue >> 8;
		}

	 	(*topcat->UpdateColormap)(pmap->pScreen,
					  index, 1, rmap, gmap, bmap);
		pdefs++;
	    }
	}
	break;
      case DirectColor:
      default:
	ErrorF("topcatStoreColors: bad class %d\n", pmap->class);
	break;
    }
}

/*-
 *-----------------------------------------------------------------------
 * topcatResolvePseudoColor --
 *	Adjust specified RGB values to closest values hardware can do.
 *
 * Results:
 *	Args are modified.
 *
 * Side Effects:
 *	None
 *
 *-----------------------------------------------------------------------
 */
void
topcatResolvePseudoColor(pRed, pGreen, pBlue, pVisual)
     CARD16	*pRed, *pGreen, *pBlue;
     VisualPtr	pVisual;
{
    register int mask;

    if (pVisual->nplanes!=1)
	mask = 0xFF00;		/* 8 bit color */
    else
	mask = 0x8000;		/* monochrome */

    *pRed &= mask; *pGreen &= mask; *pBlue &= mask;
}
