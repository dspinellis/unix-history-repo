/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.
*/

#include "X.h"
#define  NEED_EVENTS
#include "Xproto.h"
#include "scrnintstr.h"
#include "pixmapstr.h"
#include "inputstr.h"
#include "regionstr.h"
#include "windowstr.h"

#include "hppriv.h"
#include "mi.h"
#include "hpBlock.h"

/*
  maintain a local pointer to the screen->devPrivate structure. When the
  server restarts, this will be used to restore the screen pointer lost
  when the screen structure is reallocated.
*/
hpPrivScreenPtr hp_screens[MAXSCREENS] = {(hpPrivScreenPtr)NULL};


extern WindowRec WindowTable[];

static void BlankScreen();
static void UnblankScreen();

void hpDoNothing() {  }	/* one of the more exciting routines */

void
hpPlaceCursorInScreen(pScreen,x,y)
     ScreenPtr pScreen;
     int x,y;
{
    register WindowPtr pOuterWin,pInnerWin;
    CursorPtr pCurs;

    pOuterWin = &WindowTable[pScreen->myNum]; /* get root window for screen */
    pInnerWin = pOuterWin->firstChild; /* check top windows for containment */
    pCurs = wCursor(pOuterWin);
#ifdef NOT_DONE_YET
    while (pInnerWin)
    {
	if ((pInnerWin->mapped) &&
	    (x >= pInnerWin->drawable.x - pInnerWin->borderWidth) &&
	    (x < pInnerWin->drawable.x + (int)pInnerWin->clientWinSize.width +
	     pInnerWin->borderWidth) &&
	    (y >= pInnerWin->drawable.y - pInnerWin->borderWidth) &&
	    (y < pInnerWin->drawable.y + (int)pInnerWin->clientWinSize.height +
	     pInnerWin->borderWidth))
	{
	    if (pInnerWin->cursor) pCurs = pInnerWin->cursor;
	    pOuterWin = pInnerWin;	   /* sprite is contained here */
	    pInnerWin = pOuterWin->firstChild; /* any sub-windows? */
	}
	else pInnerWin = pInnerWin->nextSib;  /* not in here, continue */
    }
#endif
    pScreen->DisplayCursor(pScreen,pCurs);
}

void
hpChangeScreens(pNewScreen)
     register ScreenPtr pNewScreen; /* screen being entered */
{
    register hpPrivScreenPtr pNewPrivScreen,pPrivScreen;
    register ScreenPtr pScreen;
    register int i;

    pNewPrivScreen = getPrivScreenPtr(pNewScreen);

    for (i = 0; i < screenInfo.numScreens; i++)
    {
	pScreen = screenInfo.screens[i];
	pPrivScreen = getPrivScreenPtr(pScreen);
	if (pPrivScreen->CursorOff == hpDoNothing)
	{
	    ((HpRegisterStatePtr)(pScreen->blockData))->cursorOff(pScreen);
	    ((HpRegisterStatePtr)
	     (pScreen->wakeupData))->save(pScreen,pScreen->wakeupData);
	    ((HpRegisterStatePtr)
	     (pScreen->blockData))->save(pScreen,pScreen->blockData);
	}
	if (pScreen == pNewScreen)
	    UnblankScreen(pScreen);
	else if ((pPrivScreen->gcid == pNewPrivScreen->gcid) &&
		 (pPrivScreen->minor_num == pNewPrivScreen->minor_num))
	    BlankScreen(pScreen); /* overlay ? clear : black */
    }
}

static void
BlankScreen(pScreen)
     ScreenPtr pScreen;
{
    hpPrivScreenPtr pPrivScreen = getPrivScreenPtr(pScreen);

    if (pPrivScreen->isBlank) return;
    if (!pPrivScreen->isSaved)
    {
	pScreen->SaveScreen(pScreen, SCREEN_SAVER_ON);
	pPrivScreen->isSaved = FALSE; /* this didn't change */
    }
    pPrivScreen->isBlank = TRUE;	  /* but this did	   */

    /* if we get to here, we must be running with two heads (or we would
       never ask to be blanked, and would never have called this) */
    ((HpRegisterStatePtr)
     (pScreen->wakeupData))->save(pScreen, pScreen->wakeupData);
    ((HpRegisterStatePtr)
     (pScreen->blockData))->save(pScreen, pScreen->blockData);
}

static void
UnblankScreen(pScreen)
     ScreenPtr pScreen;
{
    hpPrivScreenPtr pPrivScreen = getPrivScreenPtr(pScreen);

    if (!pPrivScreen->isBlank) return;
    pPrivScreen->isBlank = FALSE; /* this will be true either way */
    if (!pPrivScreen->isSaved)	/* must have been blanked only  */
	pScreen->SaveScreen(pScreen, SCREEN_SAVER_OFF);
    /* if we get to here, we must be running with two heads (or we would
       never have been blanked, and so have taken the early return) */
    ((HpRegisterStatePtr)
     (pScreen->wakeupData))->save(pScreen, pScreen->wakeupData);
    ((HpRegisterStatePtr)
     (pScreen->blockData))->save(pScreen, pScreen->blockData);
}
