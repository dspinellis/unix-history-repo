/* $Header: globals.c,v 10.2 86/02/01 16:01:58 tony Rel $ */
#include <X/Xlib.h>

Cursor iconCursor, wmCursor, textCursor;
int mask;
int frameWidth;
Font iconfont, menufont, sizefont;
int screen_width, screen_height;
Pixmap gray;
Window focus;
WindowInfo focusInfo;
Status status;
int freeze;
int popup;
int bgColor, fgColor;
Pixmap bgPixmap, fgPixmap;
int cursorFunc;
int iconifyDelta;
