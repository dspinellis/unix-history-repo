/* $Header: wm.h,v 10.3 86/02/01 16:02:05 tony Rel $ */
#include <errno.h>
#include <X/Xlib.h>
#include <stdio.h>
#include <sys/types.h>

#define TRUE 1
#define FALSE 0

typedef XKeyOrButtonEvent BEvent;

#define MatchUp(e,w) ((e).type == ButtonReleased && (e).detail == (w))
#define MatchDown(e,w) ((e).type == ButtonPressed && (e).detail == (w))

#include "extern.h"
