/* $Header: vs100.h,v 10.3 86/02/01 15:47:56 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include <sys/types.h>
#include <stdio.h>
#include "param.h"
#include "packet.h"
#include "mem.h"
#include "../X/vsinput.h"
#include "../X/Xdev.h"
#include "../X/X.h"

#define HTCROCK		/* Writearound for Halftone alignment problem */

#define VBUFSIZE 2048

#define MAPTYPE(x) (x >> 4)
#define MAPLIT(x) (x & 0xf)

typedef struct _curspriv {
	BITMAP *image;
	BITMAP *mask;
	short map;
} CursPriv;

#define CDATA(x) ((CursPriv *) x->data)

typedef struct _fontpriv {
	short *widths;
	VSArea *remote;
} FontPriv;

#define FDATA(x) ((FontPriv *) x->data)

#define BDATA(x) ((VSArea *) x->data)

#define PDATA(x) ((BITMAP *) x->data)

#ifdef HTCROCK

typedef struct _tilepriv {
	BITMAP *bitmap;
	short data[16];
} TilePriv;

#define TDATA(x) ((TilePriv *) x->data)

#endif
