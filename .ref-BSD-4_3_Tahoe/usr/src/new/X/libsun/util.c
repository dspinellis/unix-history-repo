#ifndef lint
static char *rcsid_util_c = "$Header: util.c,v 10.4 86/11/29 13:49:21 jg Rel $";
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
 *		SetKeyClick
 */

#include "Xsun.h"
#include <sys/time.h>

extern int vsdev;

/*
 * Ring the bell on a sun 120, volume between 0 (quiet) and 7 (loud).  
 * Need to make the /dev/bell device with
 * the same major device number as tty[ab] but with a new minor number.
 *
 *  # /etc/mknod /dev/bell c 12 2
 *
 *  crw-rw-rw-  1 root      12,   0 Jan  6 18:18 /dev/ttya
 *  crw-rw-rw-  1 root      12,   1 Feb 26  1985 /dev/ttyb
 *  crw-rw-rw-  1 root      12,   2 Jan 14 08:45 /dev/bell
 *
 */


#define RING_ON         0x02    /* Control-B */
#define RING_OFF        0x03    /* Control-C */
#define RING_WAIT       25000 /* microseconds, for volume 1 */
#ifndef NULL
#define NULL            0
#endif

SoundBell (volume)
    int volume;
{
    static int bell = -1;
    int status;
    char outbuf[1];
    struct timeval ring_time;

    if (volume == 0) {
	return(0);
    }
    if (bell < 0) {
	bell = open("/dev/bell",2);
	if (bell < 0) {
	    return(1);
	}
    }
    ring_time.tv_sec = 0;
    ring_time.tv_usec = RING_WAIT * volume;
    outbuf[0] = RING_ON;
    status = write(bell,outbuf,1);
    if (status < 0) {
	return(1);
    }

    select(0, NULL, NULL, NULL, &ring_time);

    outbuf[0] = RING_OFF;
    status = write(bell,outbuf,1);
    if (status < 0) {
	return(1);
    }
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
	extern struct pixrect *PixRect;
        static int have_saved;
        static struct pixrect *saved;

        if (have_saved && onoff) {
                pr_rop (PixRect, 0, 0, PixRect->pr_width, PixRect->pr_height,
                        PIX_SRC | PIX_DONTCLIP, saved, 0, 0);
                have_saved = 0;
        }
        else if (!have_saved && !onoff) {
		if (saved == NULL)
			saved = mem_create(PixRect->pr_width,
				PixRect->pr_height, PixRect->pr_depth);
		if (saved) {
		   pr_rop (saved, 0, 0, PixRect->pr_width, PixRect->pr_height,
			PIX_SRC | PIX_DONTCLIP, PixRect, 0, 0);
		   pr_rop (PixRect, 0, 0, PixRect->pr_width,
			PixRect->pr_height, PIX_SRC | PIX_DONTCLIP, NULL, 0, 0);
		   have_saved = 1;
		}
        }
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
