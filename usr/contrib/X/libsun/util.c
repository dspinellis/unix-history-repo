#ifndef lint
static char *rcsid_util_c = "$Header: util.c,v 10.2 86/02/01 16:21:36 tony Rel $";
#endif	lint
#ifdef	sun
/*
 * The Sun X drivers are a product of Sun Microsystems, Inc. and are provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify these drivers without charge, but are not authorized
 * to license or distribute them to anyone else except as part of a product or
 * program developed by the user.
 * 
 * THE SUN X DRIVERS ARE PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND
 * INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE
 * PRACTICE.
 *
 * The Sun X Drivers are provided with no support and without any obligation
 * on the part of Sun Microsystems, Inc. to assist in their use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THE SUN X
 * DRIVERS OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifndef	lint
static char sccsid[] = "@(#)util.c 2.1 86/01/28 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */

/* util.c		Various utilities
 *
 *	SoundBell	Generate audible bell
 *	SetKeyClick	Control key click
 *	SetAutoRepeat	Control auto repeat
 *	SetLockLED	Control Lock LED
 *	SetVideo	Disable/enable video
 *	QueryShape	Determine shapes
 *	ResolveColors	does nothing
 *	StoreColors	does nothing
 */

/*
 *	ToDo:
 *		Implement SetVideo
 *		Implement SoundBell(!)
 */

#include "Xsun.h"

extern int vsdev;

/* Sound bell, volume between 0 (quiet) and 7 (loud) */

SoundBell (volume)
	int volume;
{
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
	/* Cursors & tiles unrestricted */
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

StoreColors (count, entries)
	int count;
	ColorDef *entries;

{
    /* XXX - should keep interal shadow of color map and rewrite whole */
    extern struct pixrect *PixRect;
    while (count--) {
	u_char r, g, b;

	r = (u_char) (entries->red>>8);
	g = (u_char) (entries->green>>8);
	b = (u_char) (entries->blue>>8);
	pr_putcolormap(PixRect, entries->pixel, 1, &r, &g, &b);
	entries++;
    }
}

extern u_char InvPix[];

InvertPixelOrder(p, n)
	register unsigned short *p;
	register int n;
{
	for (; n--; p++) {
	    register unsigned short l = (*p & 0xff), h = (*p >> 8)&0xff;
	    unsigned short         old = *p;

	    *p = (unsigned short) ((InvPix[l] << 8) | InvPix[h]);
	}
}
#endif	sun
