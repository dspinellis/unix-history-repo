/*
 *	$Source: /u1/X/libis/RCS/util.c,v $
 *	$Header: util.c,v 1.1 86/11/17 14:34:48 swick Rel $
 */

#ifndef lint
static char *rcsid_util_c = "$Header: util.c,v 1.1 86/11/17 14:34:48 swick Rel $";
#endif	lint

#include "is-copyright.h"

/* util.c		Various utilities
 *
 *	QueryShape	Determine shapes
 *	ResolveColors	Map X colors to hardware colors
 *	StoreColors	Load lookup table values
 *
 *	The rest of these don't do anything right now.
 *
 *	SoundBell	Generate audible bell
 *	SetKeyClick	Control key click
 *	SetAutoRepeat	Control auto repeat
 *	SetLockLED	Control Lock LED
 *	SetVideo	Set video blanking
 *
 *	Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include "Xis.h"

QueryShape(shape, width, height)
int		shape;
register short	*width, *height;
{
    switch (shape) {
    case CursorShape:
    case BrushShape:
	if (*width > 64)
	    *width = 64;
	if (*height > 64)
	    *height = 64;
	break;
    case TileShape:
	*width = *height = 16;
	break;
    }

#ifdef DEBUG
if (debug & D_Misc)
    printf("QueryShape(shape=%d, *width=%d, *height=%d)\n",
	shape, *width, *height);
#endif DEBUG
}

SoundBell(volume)
int	volume;
{
#ifdef DEBUG
if (debug & D_Misc)
    printf("SoundBell(volume=%d)\n", volume);
#endif DEBUG
}

/*ARGSUSED*/
SetKeyClick(volume)
int	volume;
{
#ifdef DEBUG
if (debug & D_Misc)
    printf("SetKeyClick(volume=%d)\n", volume);
#endif DEBUG
}

/*ARGSUSED*/
SetAutoRepeat(onoff)
int	onoff;
{
#ifdef DEBUG
if (debug & D_Misc)
    printf("SetAutoRepeat(onoff=%d)\n", onoff);
#endif DEBUG
}

/*ARGSUSED*/
SetLockLED(onoff)
int	onoff;
{
#ifdef DEBUG
if (debug & D_Misc)
    printf("SetLockLED(onoff=%d)\n", onoff);
#endif DEBUG
}

SetVideo(onoff)
int	onoff;
{
#ifdef DEBUG
if (debug & D_Misc)
    printf("SetVideo(onoff=%d)\n", onoff);
#endif DEBUG

    return (onoff - 1);
}

ResolveColor(red, green, blue)
unsigned short	*red, *green, *blue;
{

#ifdef DEBUG
if (debug & D_Color)
    printf("ResolveColor(%04x, %04x, %04x)", *red, *green, *blue);
#endif DEBUG

    *red	&= (unsigned)~0x0fff;
    *green	&= (unsigned)~0x0fff;
    *blue	&= (unsigned)~0x0fff;

#ifdef DEBUG
if (debug & D_Color)
    printf(" to (%04x, %04x, %04x)\n", *red, *green, *blue);
#endif DEBUG
}

StoreColors(count, entries)
register int		count;
register ColorDef	*entries;
{

#ifdef DEBUG
if (debug & D_Color)
    printf("StoreColors(%d colors)\n", count);
#endif DEBUG

    while (count--) {

#ifdef DEBUG
if (debug & D_Color)
    printf("	color %d = (%04x, %04x, %04x)\n",
	entries->pixel, entries->red, entries->green, entries->blue);
#endif DEBUG

	GIP_set_lookup((short)(entries->pixel),
	     (short)((entries->red >> 12) & 0xf),
	     (short)((entries->green >> 12) & 0xf),
	     (short)((entries->blue >> 12) & 0xf));
	entries++;
    }
}
