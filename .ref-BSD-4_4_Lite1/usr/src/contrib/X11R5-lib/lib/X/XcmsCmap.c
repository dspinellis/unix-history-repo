/* $XConsortium: XcmsCmap.c,v 1.11 91/07/25 01:08:28 rws Exp $" */

/*
 * Code and supporting documentation (c) Copyright 1990 1991 Tektronix, Inc.
 * 	All Rights Reserved
 * 
 * This file is a component of an X Window System-specific implementation
 * of Xcms based on the TekColor Color Management System.  Permission is
 * hereby granted to use, copy, modify, sell, and otherwise distribute this
 * software and its documentation for any purpose and without fee, provided
 * that this copyright, permission, and disclaimer notice is reproduced in
 * all copies of this software and in supporting documentation.  TekColor
 * is a trademark of Tektronix, Inc.
 * 
 * Tektronix makes no representation about the suitability of this software
 * for any purpose.  It is provided "as is" and with all faults.
 * 
 * TEKTRONIX DISCLAIMS ALL WARRANTIES APPLICABLE TO THIS SOFTWARE,
 * INCLUDING THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE.  IN NO EVENT SHALL TEKTRONIX BE LIABLE FOR ANY
 * SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA, OR PROFITS, WHETHER IN AN ACTION OF
 * CONTRACT, NEGLIGENCE, OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR THE PERFORMANCE OF THIS SOFTWARE.
 *
 *
 *	NAME
 *		XcmsCmap.c - Client Colormap Management Routines
 *
 *	DESCRIPTION
 *		Routines that store additional information about
 *		colormaps being used by the X Client.
 *
 *
 */

#define NEED_EVENTS
#include "Xlibint.h"
#include "Xcmsint.h"
#include "Xutil.h"

/*
 *      FORWARD DECLARATIONS
 */
XcmsCmapRec *_XcmsAddCmapRec();
static void _XcmsFreeClientCmaps();

/*
 *      LOCAL VARIABLES
 */
static unsigned char CreateWindowStatus;



/************************************************************************
 *									*
 *			PRIVATE INTERFACES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		CreateWindowErrorHandler
 *
 *	SYNOPSIS
 */
/* ARGSUSED */
static int
CreateWindowErrorHandler(dpy, errorp)
    Display *dpy;
    XErrorEvent *errorp;
/*
 *	DESCRIPTION
 *		Error Hander used in CmapRecForColormap() to catch
 *		errors occuring when creating a window with a foreign
 *		colormap.
 *
 *	RETURNS
 *		1
 *
 */
{
    if (errorp->request_code == X_CreateWindow) {
	CreateWindowStatus = errorp->error_code;
    }
    return(1);
}	


/*
 *	NAME
 *		CmapRecForColormap
 *
 *	SYNOPSIS
 */
static XcmsCmapRec *
CmapRecForColormap(dpy, cmap)
    Display *dpy;
    Colormap cmap;
/*
 *	DESCRIPTION
 *		Find the corresponding XcmsCmapRec for cmap.  In not found
 *		this routines attempts to create one.
 *
 *	RETURNS
 *		Returns NULL if failed; otherwise the address to
 *		the corresponding XcmsCmapRec.
 *
 */
{
    XcmsCmapRec *pRec;
    int nScrn;
    int i, j;
    XVisualInfo visualTemplate;	/* Template of the visual we want */
    XVisualInfo *visualList;	/* List for visuals that match */
    int nVisualsMatched;	/* Number of visuals that match */
    XSetWindowAttributes windowAttr;
    Window tmpWindow;
    int (*oldErrorHandler)();

    for (pRec = (XcmsCmapRec *)dpy->cms.clientCmaps; pRec != NULL;
	    pRec = pRec->pNext) {
	if (pRec->cmapID == cmap) {
	    return(pRec);
	}
    }

    /*
     * Can't find an XcmsCmapRec associated with cmap in our records.
     * Let's try to see if its a default colormap
     */
    nScrn = ScreenCount(dpy);
    for (i = 0; i < nScrn; i++) {
	if (cmap == DefaultColormap(dpy, i)) {
	    /* It is ... lets go ahead and store that info */
	    if ((pRec = _XcmsAddCmapRec(dpy, cmap, RootWindow(dpy, i),
		    DefaultVisual(dpy, i))) == NULL) {
		return((XcmsCmapRec *)NULL);
	    }
	    pRec->ccc = XcmsCreateCCC(
		    dpy,
		    i,			/* screenNumber */
		    DefaultVisual(dpy, i),
		    (XcmsColor *)NULL,	/* clientWhitePt */
		    (XcmsCompressionProc)NULL,  /* gamutCompProc */
		    (XPointer)NULL,	/* gamutCompClientData */
		    (XcmsWhiteAdjustProc)NULL,  /* whitePtAdjProc */
		    (XPointer)NULL	/* whitePtAdjClientData */
		    );
	    return(pRec);
	}
    }

    /*
     * Nope, its not a default colormap, so it's probably a foreign color map
     * of which we have no specific details.  Let's go through the
     * rigorous process of finding this colormap:
     *        for each screen
     *            for each screen's visual types
     *                create a window
     *                attempt to set the window's colormap to cmap
     *                if successful
     *                    Add a CmapRec
     *                    Create an XcmsCCC
     *                    return the CmapRec
     *                else
     *                    continue
     */

    /*
     * Before we start into this ugly process, let's sync up.
     */
    XSync(dpy, False);

    /*
     * Setup Temporary Error Handler
     */
    oldErrorHandler = XSetErrorHandler(CreateWindowErrorHandler);

    for (i = 0; i < nScrn; i++) {
	visualTemplate.screen = i;
	visualList = XGetVisualInfo(dpy, VisualScreenMask, &visualTemplate,
	    &nVisualsMatched);
	if (nVisualsMatched == 0) {
	    continue;
	}

	j = 0;
	windowAttr.colormap = cmap;

	/*
	 * Call routine that attempts to create a window with cmap
	 */
	do {
	    CreateWindowStatus = Success;
	    tmpWindow = XCreateWindow(dpy, RootWindow(dpy, i),
		    0, 0, 5, 5, 1,
		    (visualList+j)->depth, 
		    CopyFromParent,
		    (visualList+j)->visual, 
		    CWColormap,
		    &windowAttr);
	    XSync(dpy, False);
	} while (CreateWindowStatus != Success && ++j < nVisualsMatched);

	/*
	 * if successful
	 */
	if (CreateWindowStatus == Success) {
	    if ((pRec = _XcmsAddCmapRec(dpy, cmap, tmpWindow,
		    (visualList+j)->visual)) == NULL) {
		return((XcmsCmapRec *)NULL);
	    }
	    pRec->ccc = XcmsCreateCCC(
		    dpy,
		    i,			/* screenNumber */
		    (visualList+j)->visual,
		    (XcmsColor *)NULL,	/* clientWhitePt */
		    (XcmsCompressionProc)NULL,  /* gamutCompProc */
		    (XPointer)NULL,	/* gamutCompClientData */
		    (XcmsWhiteAdjustProc)NULL,  /* whitePtAdjProc */
		    (XPointer)NULL	/* whitePtAdjClientData */
		    );
	    XSetErrorHandler(oldErrorHandler);
	    XDestroyWindow(dpy, tmpWindow);
	    XFree((XPointer)visualList);
	    return(pRec);
	}

	/*
	 * Otherwise continue ....
	 */
	XFree((XPointer)visualList);
    }

    /*
     * Unsetup Temporary Error Handler
     */
    XSetErrorHandler(oldErrorHandler);

    return(NULL);
}



/************************************************************************
 *									*
 *			API PRIVATE INTERFACES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		_XcmsAddCmapRec
 *
 *	SYNOPSIS
 */
XcmsCmapRec *
_XcmsAddCmapRec(dpy, cmap, windowID, visual)
    Display *dpy;
    Colormap cmap;
    Window windowID;
    Visual *visual;
/*
 *	DESCRIPTION
 *		Create an XcmsCmapRec for the specified cmap, windowID,
 *		and visual, then adds it to its list of CmapRec's.
 *
 *	RETURNS
 *		Returns NULL if failed; otherwise the address to
 *		the added XcmsCmapRec.
 *
 */
{
    XcmsCmapRec *pNew;

    if ((pNew = (XcmsCmapRec *) Xcalloc(1, (unsigned) sizeof(XcmsCmapRec)))
	    == NULL) {
	return((XcmsCmapRec *)NULL);
    }

    pNew->cmapID = cmap;
    pNew->dpy = dpy;
    pNew->windowID = windowID;
    pNew->visual = visual;
    pNew->pNext = (XcmsCmapRec *)dpy->cms.clientCmaps;
    dpy->cms.clientCmaps = (XPointer)pNew;
    dpy->free_funcs->clientCmaps = _XcmsFreeClientCmaps;

    /*
     * Note, we don't create the XcmsCCC for pNew->ccc here because
     * it may require the use of XGetWindowAttributes (a round trip request)
     * to determine the screen.
     */
    return(pNew);
}


/*
 *	NAME
 *		_XcmsCopyCmapRecAndFree
 *
 *	SYNOPSIS
 */
XcmsCmapRec *
_XcmsCopyCmapRecAndFree(dpy, src_cmap, copy_cmap)
    Display *dpy;
    Colormap src_cmap;
    Colormap copy_cmap;
/*
 *	DESCRIPTION
 *		Augments Xlib's XCopyColormapAndFree() to copy
 *		XcmsCmapRecs.
 *
 *	RETURNS
 *		Returns NULL if failed; otherwise the address to
 *		the copy XcmsCmapRec.
 *
 */
{
    XcmsCmapRec *pRec_src;
    XcmsCmapRec *pRec_copy;

    if ((pRec_src = CmapRecForColormap(dpy, src_cmap)) != NULL) {
	pRec_copy =_XcmsAddCmapRec(dpy, copy_cmap, pRec_src->windowID,
		pRec_src->visual);
	if (pRec_copy != NULL && pRec_src->ccc) {
	    pRec_copy->ccc = (XcmsCCC)Xcalloc(1, (unsigned) sizeof(XcmsCCCRec));
	    bcopy((char *)pRec_src->ccc, (char *)pRec_copy->ccc,
		    sizeof(XcmsCCCRec));
	}
	return(pRec_copy);
    }
    return((XcmsCmapRec *)NULL);
}


/*
 *	NAME
 *		_XcmsDeleteCmapRec
 *
 *	SYNOPSIS
 */
void
_XcmsDeleteCmapRec(dpy, cmap)
    Display *dpy;
    Colormap cmap;
/*
 *	DESCRIPTION
 *		Removes and frees the specified XcmsCmapRec structure
 *		from the linked list of structures.
 *
 *	RETURNS
 *		void
 *
 */
{
    XcmsCmapRec **pPrevPtr;
    XcmsCmapRec *pRec;
    int scr;

    /* If it is the default cmap for a screen, do not delete it,
     * because the server will not actually free it */
    for (scr = ScreenCount(dpy); --scr >= 0; ) {
	if (cmap == DefaultColormap(dpy, scr))
	    return;
    }

    /* search for it in the list */
    pPrevPtr = (XcmsCmapRec **)&dpy->cms.clientCmaps;
    while ((pRec = *pPrevPtr) && (pRec->cmapID != cmap)) {
	pPrevPtr = &pRec->pNext;
    }

    if (pRec) {
	if (pRec->ccc) {
	    XcmsFreeCCC(pRec->ccc);
	}
	*pPrevPtr = pRec->pNext;
	Xfree(pRec);
    }
}


/*
 *	NAME
 *		_XcmsFreeClientCmaps
 *
 *	SYNOPSIS
 */
static void
_XcmsFreeClientCmaps(dpy)
    Display *dpy;
/*
 *	DESCRIPTION
 *		Frees all XcmsCmapRec structures in the linked list
 *		and sets dpy->cms.clientCmaps to NULL.
 *
 *	RETURNS
 *		void
 *
 */
{
    XcmsCmapRec *pRecNext, *pRecFree;

    pRecNext = (XcmsCmapRec *)dpy->cms.clientCmaps;
    while (pRecNext != NULL) {
	pRecFree = pRecNext;
	pRecNext = pRecNext->pNext;
	if (pRecFree->ccc) {
	    /* Free the XcmsCCC structure */
	    XcmsFreeCCC(pRecFree->ccc);
	}
	/* Now free the XcmsCmapRec structure */
	Xfree(pRecFree);
    }
    dpy->cms.clientCmaps = (XPointer)NULL;
}



/************************************************************************
 *									*
 *			PUBLIC INTERFACES				*
 *									*
 ************************************************************************/

/*
 *	NAME
 *		XcmsCCCOfColormap
 *
 *	SYNOPSIS
 */
XcmsCCC 
XcmsCCCOfColormap(dpy, cmap)
    Display *dpy;
    Colormap cmap;
/*
 *	DESCRIPTION
 *		Finds the XcmsCCC associated with the specified colormap.
 *
 *	RETURNS
 *		Returns NULL if failed; otherwise the address to
 *		the associated XcmsCCC structure.
 *
 */
{
    XWindowAttributes windowAttr;
    XcmsCmapRec *pRec;
    int nScrn = ScreenCount(dpy);
    int i;

    if ((pRec = CmapRecForColormap(dpy, cmap)) != NULL) {
	if (pRec->ccc) {
	    /* XcmsCmapRec already has a XcmsCCC */
	    return(pRec->ccc);
	}

	/*
	 * The XcmsCmapRec does not have a XcmsCCC yet, so let's create
	 * one.  But first, we need to know the screen associated with
	 * cmap, so use XGetWindowAttributes() to extract that
	 * information.  Unless, of course there is only one screen!!
	 */
	if (nScrn == 1) {
	    /* Assume screenNumber == 0 */
	    return(pRec->ccc = XcmsCreateCCC(
		    dpy,
		    0,			/* screenNumber */
		    pRec->visual,
		    (XcmsColor *)NULL,	/* clientWhitePt */
		    (XcmsCompressionProc)NULL,  /* gamutCompProc */
		    (XPointer)NULL,	/* gamutCompClientData */
		    (XcmsWhiteAdjustProc)NULL,  /* whitePtAdjProc */
		    (XPointer)NULL	/* whitePtAdjClientData */
		    ));
	} else {
	    if (XGetWindowAttributes(dpy, pRec->windowID, &windowAttr)) {
		for (i = 0; i < nScrn; i++) {
		    if (ScreenOfDisplay(dpy, i) == windowAttr.screen) {
			return(pRec->ccc = XcmsCreateCCC(
				dpy,
				i,		   /* screenNumber */
				pRec->visual,
				(XcmsColor *)NULL, /* clientWhitePt */
				(XcmsCompressionProc)NULL, /* gamutCompProc */
				(XPointer)NULL,	   /* gamutCompClientData */
				(XcmsWhiteAdjustProc)NULL, /* whitePtAdjProc */
				(XPointer)NULL	   /* whitePtAdjClientData */
				));
		    }
		}
	    }
	}
    }

    /*
     * No such cmap
     */
    return(NULL);
}
