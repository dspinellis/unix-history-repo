
/* $Header: XMenuInt.h,v 1.1 87/08/04 10:29:03 toddb Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuInternal.h - Internal menu system include file for the
 *			MIT Project Athena XMenu X window system
 *			menu package.
 *			
 *	Author:		Tony Della Fera, DEC
 *			October, 1985
 */

#ifndef _XMenuInternal_h_
#define _XMenuInternal_h_

#include <X11/Xlib.h>
#include <X11/X10.h>
#include <stdio.h>
#include "XMenu.h"

#define min(x, y)	((x) <= (y) ? (x) : (y))
#define max(x, y)	((x) >= (y) ? (x) : (y))
#define abs(a)		((a) < 0 ? -(a) : (a))

#define _X_FAILURE	-1

#define _SUCCESS	1
#define _FAILURE	-1

/*
 * XMenu internal event handler variable.
 */
extern int (*_XMEventHandler)();

#ifndef Pixel
#define Pixel unsigned long
#endif

/*
 * Internal routine declarations.
 */
int _XMWinQueInit();		/* No value actually returned. */
int _XMWinQueAddPane();
int _XMWinQueAddSelection();
int _XMWinQueFlush();
XMPane *_XMGetPanePtr();
XMSelect *_XMGetSelectionPtr();
int _XMRecomputeGlobals();	/* No value actually returned. */
int _XMRecomputePane();
int _XMRecomputeSelection();
int _XMTransToOrigin();		/* No value actually returned. */
int _XMRefreshPane();		/* No value actually returned. */
int _XMRefreshSelections();	/* No value actually returned. */
int _XMHighlightSelection();	/* No value actually returned. */

#endif
/* Don't add stuff after this #endif */
