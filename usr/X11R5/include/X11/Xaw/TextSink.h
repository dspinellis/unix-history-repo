/*
 * $XConsortium: TextSink.h,v 1.8 91/02/17 13:20:18 rws Exp $
 */

/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#ifndef _XawTextSink_h
#define _XawTextSink_h

/***********************************************************************
 *
 * TextSink Object
 *
 ***********************************************************************/

#include <X11/Object.h>
#include <X11/Xfuncproto.h>

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 font                Font            XFontStruct *      XtDefaultFont
 foreground          Foreground      Pixel              XtDefaultForeground
 background          Background      Pixel              XtDefaultBackground

*/
 
/* Class record constants */

extern WidgetClass textSinkObjectClass;

typedef struct _TextSinkClassRec *TextSinkObjectClass;
typedef struct _TextSinkRec      *TextSinkObject;

typedef enum {XawisOn, XawisOff} XawTextInsertState;

/************************************************************
 *
 * Public Functions.
 *
 ************************************************************/

_XFUNCPROTOBEGIN

/*	Function Name: XawTextSinkDisplayText
 *	Description: Stub function that in subclasses will display text. 
 *	Arguments: w - the TextSink Object.
 *                 x, y - location to start drawing text.
 *                 pos1, pos2 - location of starting and ending points
 *                              in the text buffer.
 *                 highlight - hightlight this text?
 *	Returns: none.
 *
 * This function doesn't actually display anything, it is only a place
 * holder.
 */

extern void XawTextSinkDisplayText(
#if NeedFunctionPrototypes
    Widget		/* w */,
#if NeedWidePrototypes
    /* Position */ int	/* x */,
    /* Position	*/ int	/* y */,
#else
    Position		/* x */,
    Position		/* y */,
#endif
    XawTextPosition	/* pos1 */,
    XawTextPosition	/* pos2 */,
#if NeedWidePrototypes
    /* Boolean */ int	/* highlight */
#else
    Boolean		/* highlight */
#endif
#endif
);

/*	Function Name: XawTextSinkInsertCursor
 *	Description: Places the InsertCursor.
 *	Arguments: w - the TextSink Object.
 *                 x, y - location for the cursor.
 *                 staye - whether to turn the cursor on, or off.
 *	Returns: none.
 *
 * This function doesn't actually display anything, it is only a place
 * holder.
 */

extern void XawTextSinkInsertCursor(
#if NeedFunctionPrototypes
    Widget		/* w */,
#if NeedWidePrototypes
    /* Position */ int	/* x */,
    /* Position	*/ int	/* y */,
    /* XawTextInsertState */ int /* state */
#else
    Position		/* x */,
    Position		/* y */,
    XawTextInsertState	/* state */
#endif
#endif
);

/*	Function Name: XawTextSinkClearToBackground
 *	Description: Clears a region of the sink to the background color.
 *	Arguments: w - the TextSink Object.
 *                 x, y  - location of area to clear.
 *                 width, height - size of area to clear
 *	Returns: void.
 *
 * This function doesn't actually display anything, it is only a place
 * holder.
 */

extern void XawTextSinkClearToBackground(
#if NeedFunctionPrototypes
    Widget		/* w */,
#if NeedWidePrototypes
    /* Position */ int	/* x */,
    /* Position */ int	/* y */,
    /* Dimension */ int	/* width */,
    /* Dimension */ int	/* height */
#else
    Position		/* x */,
    Position		/* y */,
    Dimension		/* width */,
    Dimension		/* height */
#endif
#endif
);

/*	Function Name: XawTextSinkFindPosition
 *	Description: Finds a position in the text.
 *	Arguments: w - the TextSink Object.
 *                 fromPos - reference position.
 *                 fromX   - reference location.
 *                 width,  - width of section to paint text.
 *                 stopAtWordBreak - returned position is a word break?
 *                 resPos - Position to return.      *** RETURNED ***
 *                 resWidth - Width actually used.   *** RETURNED ***
 *                 resHeight - Height actually used. *** RETURNED ***
 *	Returns: none (see above).
 */

extern void XawTextSinkFindPosition(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* fromPos */,
    int			/* fromX */,
    int			/* width */,
#if NeedWidePrototypes
    /* Boolean */ int	/* stopAtWordBreak */,
#else
    Boolean		/* stopAtWordBreak */,
#endif
    XawTextPosition*	/* pos_return */,
    int*		/* width_return */,
    int*		/* height_return */
#endif
);

/*	Function Name: XawTextSinkFindDistance
 *	Description: Find the Pixel Distance between two text Positions.
 *	Arguments: w - the TextSink Object.
 *                 fromPos - starting Position.
 *                 fromX   - x location of starting Position.
 *                 toPos   - end Position.
 *                 resWidth - Distance between fromPos and toPos.
 *                 resPos   - Acutal toPos used.
 *                 resHeight - Height required by this text.
 *	Returns: none.
 */

extern void XawTextSinkFindDistance (
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* fromPos */,
    int			/* fromX */,
    XawTextPosition     /* toPos */,
    int*		/* width_return */,
    XawTextPosition*	/* pos_return */,
    int*		/* height_return */
#endif
);

/*	Function Name: XawTextSinkResolve
 *	Description: Resloves a location to a position.
 *	Arguments: w - the TextSink Object.
 *                 pos - a reference Position.
 *                 fromx - a reference Location.
 *                 width - width to move.
 *                 resPos - the resulting position.
 *	Returns: none
 */

extern void XawTextSinkResolve(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* fromPos */,
    int			/* fromX */,
    int			/* width */,
    XawTextPosition*	/* pos_return */
#endif
);

/*	Function Name: XawTextSinkMaxLines
 *	Description: Finds the Maximum number of lines that will fit in
 *                   a given height.
 *	Arguments: w - the TextSink Object.
 *                 height - height to fit lines into.
 *	Returns: the number of lines that will fit.
 */

extern int XawTextSinkMaxLines(
#if NeedFunctionPrototypes
    Widget		/* w */,
#if NeedWidePrototypes
    /* Dimension */ int	/* height */
#else
    Dimension		/* height */
#endif
#endif
);

/*	Function Name: XawTextSinkMaxHeight
 *	Description: Finds the Minium height that will contain a given number 
 *                   lines.
 *	Arguments: w - the TextSink Object.
 *                 lines - the number of lines.
 *	Returns: the height.
 */

extern int XawTextSinkMaxHeight(
#if NeedFunctionPrototypes
    Widget		/* w */,
    int			/* lines */
#endif
);

/*	Function Name: XawTextSinkSetTabs
 *	Description: Sets the Tab stops.
 *	Arguments: w - the TextSink Object.
 *                 tab_count - the number of tabs in the list.
 *                 tabs - the text positions of the tabs.
 *	Returns: none
 */

extern void XawTextSinkSetTabs(
#if NeedFunctionPrototypes
    Widget		/* w */,
    int			/* tab_count */,
    int*		/* tabs */
#endif
);
						  
/*	Function Name: XawTextSinkGetCursorBounds
 *	Description: Finds the bounding box for the insert curor (caret).
 *	Arguments: w - the TextSinkObject.
 *                 rect - an X rectance containing the cursor bounds.
 *	Returns: none (fills in rect).
 */

extern void XawTextSinkGetCursorBounds(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XRectangle*		/* rect_return */
#endif
);

_XFUNCPROTOEND

#endif /* _XawTextSrc_h -- DON'T ADD STUFF AFTER THIS #endif */
