#ifndef lint
static char *rcsid_util_c = "$Header: util.c,v 10.1 86/11/29 13:53:19 jg Rel $";
#endif	lint
    /*

    Copyright 1986 by the University of Utah

    Permission to use, copy, modify, and distribute this
    software and its documentation for any purpose and without
    fee is hereby granted, provided that the above copyright
    notice appear in all copies and that both that copyright
    notice and this permission notice appear in supporting
    documentation, and that the name of the University of Utah
    not be used in advertising or publicity pertaining to 
    distribution of the software without specific, written 
    prior permission. The University of Utah makes no
    representations about the suitability of this software for
    any purpose.  It is provided "as is" without express or
    implied warranty.

    */


/* util.c		Various utilities
 *
 *	SoundBell	Generate audible bell
 *	SetKeyClick	Control key click
 *	SetAutoRepeat	Control auto repeat
 *	SetLockLED	Control Lock LED
 *	SetVideo	Disable/enable video
 *	QueryShape	Determine shapes
 *	ResolveColors	doesn't do much
 *	StoreColors	sets the color map
 */

/*
 *	ToDo:
 *		Implement SetVideo
 */

#include "Xapollo.h"
#include "/sys/ins/tone.ins.c"

extern int vsdev;

/* Sound bell, volume between 0 (quiet) and 7 (loud) */

SoundBell (volume)
	int volume;
{
	time_$clock_t ToneTime;
	ToneTime.high = 1;
	ToneTime.low = 0;
	tone_$time(ToneTime);
	return (0);
}

/* Set key click, volume between -1 (default), 0 (off) and 8 (loud) */

SetKeyClick (volume)
	int volume;
{
	return (0);
}

/* Set autorepeat */

SetAutoRepeat (onoff)
	int onoff;
{
	return (0);
}

int SetVideo(onoff)
	int onoff;
{
	return(0);
}

QueryShape (shape, width, height)
	int shape;
	short *width, *height;
{
	switch (shape) {
	case CursorShape:
	    if (*width > 16)
		*width = 16;
	    if (*height > 16)
		*height = 16;
	    break;
	case TileShape:
	    *width = *height = 32;
	    break;
	}
}

SetLockLED (onoff)
	int onoff;
{
	return (0);
}

ResolveColor (red, green, blue)
	unsigned short *red, *green, *blue;
{
    *red &= ~0377;
    *green &= ~0377;
    *blue &= ~0377;
}

long
color_entry(red, green, blue)
int red, green, blue;
{
    red = red >> 8;
    green = green >> 8;
    blue = blue >> 8;
    return(( red * 65536) + (green * 256) + blue);
}

StoreColors (count, entries)
	int count;
	ColorDef *entries;

{
    gpr_$color_vector_t values;
    int n;
    status_$t status;

    for (n=0;n<count;n++) {
      values[0] = color_entry(entries->red, entries->green, entries->blue);
      gpr_$set_color_map( (gpr_$pixel_value_t)entries->pixel, (short)1, values, status);
      check_status( status, "StoreColors: ");
      entries++;
      }
}

extern unsigned char InvertedPixelArray[];

InvertPixelOrder(p, n)
	register unsigned short *p;
	register int n;
{
	for (; n--; p++)
	    *p = (unsigned short) ((InvertedPixelArray[*p & 0xFF] << 8) |
				   InvertedPixelArray[*p >> 8]);
}


