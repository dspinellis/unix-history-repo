#ifndef lint
static char *rcsid_util_c = "$Header: util.c,v 10.1 86/11/19 10:44:43 jg Exp $";
#endif	lint
/* Copyright 1985 Massachusetts Institute of Technology */

/* util.c - Various utilities
 *
 *      SoundBell       Generate audible bell
 *      SetKeyClick     Control key click
 *      SetAutoRepeat   Control auto repeat
 *      SetLockLED      Control Lock LED
 *      SetVideo        Disable/enable video
 *      QueryShape      Determine shapes
 *      ResolveColors   Does nothing
 *      StoreColors     Does nothing
 *
 *  	Author:
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *      	Providence, RI 02912
 *
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

#include "private.h"
#include "bitblt.h"

/*
 * Sound bell, volume between 0 (quiet) and 7 (loud)
 */

SoundBell (volume)
        int volume;
{
#ifdef TRACE_X
	fprintf(stderr, "In SoundBell\n");
	fflush(stderr);
#endif TRACE_X
	if (volume > 0)
		ioctl (xdev, QIOCBELL, (caddr_t)&volume);
}

/*
 * Set key click, volume between -1 (default), 0 (off) and 8 (loud)
 */

SetKeyClick (volume)
        int volume;
{
#ifdef TRACE_X
	fprintf(stderr, "In SetKeyClick\n");
	fflush(stderr);
#endif TRACE_X
	ioctl (xdev, QIOCCLICK, (caddr_t)&volume);
}

/*
 * Set autorepeat on or off
 */

SetAutoRepeat (onoff)
        int onoff;
{
#ifdef TRACE_X
	fprintf(stderr, "In SetAutoRepeat\n");
	fflush(stderr);
#endif TRACE_X
	ioctl (xdev, QIOCAUTOREP, (caddr_t)&onoff);
}

/*
 * Enable/disable video
 */

/*ARGSUSED*/
SetVideo(onoff)
        int onoff;
{
#ifdef TRACE_X
	fprintf(stderr, "In SetVideo\n");
	fflush(stderr);
#endif TRACE_X
        return(0);
}

/*
 * Determine shapes of cursor and tile
 */

QueryShape (shape, width, height)
        int shape;
        register short *width, *height;
{
#ifdef TRACE_X
	fprintf(stderr, "In QueryShape\n");
	fflush(stderr);
#endif TRACE_X

        switch (shape) {
        case CursorShape:
            if (*width > CURSOR_WIDTH)
                *width = CURSOR_WIDTH;
            if (*height > CURSOR_HEIGHT)
                *height = CURSOR_HEIGHT;
            break;
        case TileShape:
            *width = *height = TILE_SIZE;
            break;
        }
}

/*
 * Turn caps lock on or off
 */

SetLockLED (onoff)
        int onoff;
{
#ifdef TRACE_X
	fprintf(stderr, "In SetLockLED\n");
	fflush(stderr);
#endif TRACE_X
	ioctl (xdev, (onoff) ? QIOCSETCAPSL : QIOCCLRCAPSL, (caddr_t) 0);
}

/*ARGSUSED*/
ResolveColor (red, green, blue)
        u_short *red, *green, *blue;
{
#ifdef TRACE_X
	fprintf(stderr, "In ResolveColor\n");
	fflush(stderr);
#endif TRACE_X
}

/*ARGSUSED*/
StoreColors (count, entries)
        int count;
        ColorDef *entries;
{
#ifdef TRACE_X
	fprintf(stderr, "In StoreColors\n");
	fflush(stderr);
#endif TRACE_X
}
