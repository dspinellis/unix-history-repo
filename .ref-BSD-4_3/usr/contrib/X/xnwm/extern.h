/* $Header: extern.h,v 10.2 86/02/01 16:01:53 tony Rel $ */
extern int errno;

extern Cursor iconCursor, wmCursor, textCursor;
extern int mask;
extern int frameWidth;
extern Font iconfont, sizefont, menufont;
extern int screen_width, screen_height;
extern Pixmap gray;
extern Window focus;
extern WindowInfo focusInfo;
extern Status status;
extern int freeze;
extern int popup;
extern int bgColor, fgColor;
extern Pixmap bgPixmap, fgPixmap;
extern int cursorFunc;
extern int iconifyDelta;

Cursor XCreateCursor();
Bitmap XStoreBitmap();
Pixmap XMakePixmap();
Font XGetFont();
