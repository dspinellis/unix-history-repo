#include <X/mit-copyright.h>

/* $Header: XMenuInternal.h,v 10.9 86/02/12 16:19:41 tony Rel $ */
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

#include <X/Xlib.h>
#include "XMenu.h"
#include <stdio.h>

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

/*
 * Internal boolean datatype.
 */
typedef enum _bool {FALSE, TRUE} Bool;

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
