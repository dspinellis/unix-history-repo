/*
 * $XConsortium: TextSrc.h,v 1.9 91/02/17 13:12:26 rws Exp $
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

#ifndef _XawTextSrc_h
#define _XawTextSrc_h

/***********************************************************************
 *
 * TextSrc Object
 *
 ***********************************************************************/

#include <X11/Object.h>
#include <X11/Xfuncproto.h>

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 editType	     EditType		XawTextEditType	XawtextRead

*/
 
/* Class record constants */

extern WidgetClass textSrcObjectClass;

typedef struct _TextSrcClassRec *TextSrcObjectClass;
typedef struct _TextSrcRec      *TextSrcObject;

typedef enum {XawstPositions, XawstWhiteSpace, XawstEOL, XawstParagraph,
              XawstAll} XawTextScanType;
typedef enum {Normal, Selected }highlightType;
typedef enum {XawsmTextSelect, XawsmTextExtend} XawTextSelectionMode;
typedef enum {XawactionStart, XawactionAdjust, XawactionEnd}
    XawTextSelectionAction;

/*
 * Error Conditions:
 */

#define XawTextReadError -1
#define XawTextScanError -1

/************************************************************
 *
 * Public Functions.
 *
 ************************************************************/

_XFUNCPROTOBEGIN

/*	Function Name: XawTextSourceRead
 *	Description: This function reads the source.
 *	Arguments: w - the TextSrc Object.
 *                 pos - position of the text to retreive.
 * RETURNED        text - text block that will contain returned text.
 *                 length - maximum number of characters to read.
 *	Returns: The number of characters read into the buffer.
 */

extern XawTextPosition XawTextSourceRead(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* pos */,
    XawTextBlock*	/* text_return */,
    int			/* length */
#endif
);

/*	Function Name: XawTextSourceReplace.
 *	Description: Replaces a block of text with new text.
 *	Arguments: src - the Text Source Object.
 *                 startPos, endPos - ends of text that will be removed.
 *                 text - new text to be inserted into buffer at startPos.
 *	Returns: XawEditError or XawEditDone.
 */

extern int XawTextSourceReplace (
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* start */,
    XawTextPosition	/* end */,
    XawTextBlock*	/* text */
#endif
);

/*	Function Name: XawTextSourceScan
 *	Description: Scans the text source for the number and type
 *                   of item specified.
 *	Arguments: w - the TextSrc Object.
 *                 position - the position to start scanning.
 *                 type - type of thing to scan for.
 *                 dir - direction to scan.
 *                 count - which occurance if this thing to search for.
 *                 include - whether or not to include the character found in
 *                           the position that is returned. 
 *	Returns: The position of the text.
 *
 */

extern XawTextPosition XawTextSourceScan(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* position */,
#if NeedWidePrototypes
    /* XawTextScanType */ int		/* type */,
    /* XawTextScanDirection */ int	/* dir */,
#else
    XawTextScanType	/* type */,
    XawTextScanDirection /* dir */,
#endif
    int			/* count */,
#if NeedWidePrototypes
    /* Boolean */ int	/* include */
#else
    Boolean		/* include */
#endif
#endif
);

/*	Function Name: XawTextSourceSearch
 *	Description: Searchs the text source for the text block passed
 *	Arguments: w - the TextSource Object.
 *                 position - the position to start scanning.
 *                 dir - direction to scan.
 *                 text - the text block to search for.
 *	Returns: The position of the text we are searching for or
 *               XawTextSearchError.
 */

extern XawTextPosition XawTextSourceSearch(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* position */,
#if NeedWidePrototypes
    /* XawTextScanDirection */ int	/* dir */,
#else
    XawTextScanDirection /* dir */,
#endif
    XawTextBlock*	/* text */
#endif
);

/*	Function Name: XawTextSourceConvertSelection
 *	Description: Dummy selection converter.
 *	Arguments: w - the TextSrc object.
 *                 selection - the current selection atom.
 *                 target    - the current target atom.
 *                 type      - the type to conver the selection to.
 * RETURNED        value, length - the return value that has been converted.
 * RETURNED        format    - the format of the returned value.
 *	Returns: TRUE if the selection has been converted.
 *
 */

extern Boolean XawTextSourceConvertSelection(
#if NeedFunctionPrototypes
    Widget		/* w */,
    Atom*		/* selection */,
    Atom*		/* target */,
    Atom*		/* type */,
    XtPointer*		/* value_return */,
    unsigned long*	/* length_return */,
    int*		/* format_return */
#endif
);

/*	Function Name: XawTextSourceSetSelection
 *	Description: allows special setting of the selection.
 *	Arguments: w - the TextSrc object.
 *                 left, right - bounds of the selection.
 *                 selection - the selection atom.
 *	Returns: none
 */

extern void XawTextSourceSetSelection(
#if NeedFunctionPrototypes
    Widget		/* w */,
    XawTextPosition	/* start */,
    XawTextPosition	/* end */,
    Atom		/* selection */
#endif
);

_XFUNCPROTOEND

#endif /* _XawTextSrc_h */
/* DON'T ADD STUFF AFTER THIS #endif */
