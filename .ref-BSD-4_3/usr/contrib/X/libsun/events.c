#ifndef lint
static char *rcsid_events_c = "$Header: events.c,v 10.2 86/02/01 16:20:48 tony Rel $";
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
static char sccsid[] = "@(#)events.c 2.1 86/01/28 Copyright 1986 Sun Micro";
#endif

/*-
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 */

/*
 *	ToDo:
 *		Up events
 *		Shiftlock support
 */

#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/time.h>
#include	<sys/errno.h>
#include	"../X/X.h"
#include	"../X/vsinput.h"
#include	"../X/Xdev.h"
#include	<sundev/kbd.h>
#include	<sunwindow/win_input.h>

#ifndef	event_is_ascii
#define	event_is_ascii(e) (e->ie_code >= ASCII_FIRST && e->ie_code <= ASCII_LAST)
#endif
#ifndef	event_is_meta
#define	event_is_meta(e) (e->ie_code >= META_FIRST && e->ie_code <= META_LAST)
#endif
/* Should be qevent.h */
#define	VSE_LEFT_BUTTON	0
#define	VSE_MIDDLE_BUTTON	1
#define	VSE_RIGHT_BUTTON	2

extern int errno;
unsigned state_mask = 0;
extern int vsdev;
extern DEVICE *CurrentDevice;

/*ARGSUSED*/
ProcessInput(ev)
	register vsEvent *ev;
{
	/*NOTREACHED*/
}

#include "lk201.h"

static int
SunToXKeyCode(s)
int s;
{
    register int ret = LK201[s&0177];
    char *c = "^.";
    char *f = ".";

    c[1] = (s&0177)|0100;
    f[0] = (s&0177);

    return(ret);
}

/*
 * Convert from a Sun event to an X event
 */
static int
ConvertEvent(se, xe)
struct inputevent *se;
vsEvent *xe;
{
    /* Map the coordinates */
    xe->vse_x = se->ie_locx;
    xe->vse_y = se->ie_locy;
    /* Map the time stamps */
    xe->vse_time = (se->ie_time.tv_usec/10000 + se->ie_time.tv_sec);
    /* Set direction */
    xe->vse_direction = (win_inputposevent(se) ? VSE_KBTDOWN : VSE_KBTUP);
    /* Sort out the event codes */
    if (event_is_ascii(se)) {
	/* ASCII keystroke */
	int key = SunToXKeyCode(se->ie_code);
	xe->vse_key = (key & 0377);
	xe->vse_device = VSE_DKB;
	xe->vse_type = VSE_BUTTON;
	state_mask = (state_mask & ~(ControlMask|MetaMask|ShiftMask|ShiftLockMask))
	    | (key & (ControlMask|MetaMask|ShiftMask|ShiftLockMask));
    } else if (event_is_meta(se)) {
	/* META keystroke - map to ASCII for now */
	int key = SunToXKeyCode(se->ie_code - META_FIRST + ASCII_FIRST) | MetaMask;
	xe->vse_key = (key & 0377);
	xe->vse_device = VSE_DKB;
	xe->vse_type = VSE_BUTTON;
	state_mask = (state_mask & ~(ControlMask|MetaMask|ShiftMask|ShiftLockMask))
	    | (key & (ControlMask|MetaMask|ShiftMask|ShiftLockMask));
    } else switch (se->ie_code) {
	case LOC_MOVE:
	    xe->vse_device = VSE_MOUSE;		/* XXX - should query shift state here but ... */
	    xe->vse_type = VSE_MMOTION;
	    break;
	case MS_LEFT:
	    xe->vse_key = VSE_LEFT_BUTTON;	/* XXX - should query shift state here */
	    xe->vse_device = VSE_MOUSE;
	    xe->vse_type = VSE_BUTTON;
	    if (xe->vse_direction == VSE_KBTUP)
		state_mask &= ~LeftMask;
	    else
	        state_mask |= LeftMask;
	    goto ShiftKeys;
	case MS_MIDDLE:
	    xe->vse_device = VSE_MOUSE;
	    xe->vse_type = VSE_BUTTON;
	    xe->vse_key = VSE_MIDDLE_BUTTON;
	    if (xe->vse_direction == VSE_KBTUP)
		state_mask &= ~MiddleMask;
	    else
	        state_mask |= MiddleMask;
	    goto ShiftKeys;
	case MS_RIGHT:
	    xe->vse_key = VSE_RIGHT_BUTTON;
	    xe->vse_device = VSE_MOUSE;
	    xe->vse_type = VSE_BUTTON;
	    if (xe->vse_direction == VSE_KBTUP)
		state_mask &= ~RightMask;
	    else
	        state_mask |= RightMask;
ShiftKeys:
	    if (se->ie_shiftmask & CAPSMASK)
		state_mask |= ShiftLockMask;
	    else
	        state_mask &= ~ShiftMask;
	    if (se->ie_shiftmask & SHIFTMASK)
		state_mask |= ShiftMask;
	    else
	    	state_mask &= ~ShiftMask;
	    if (se->ie_shiftmask & CTRLMASK)
		state_mask |= ControlMask;
	    else
		state_mask &= ~ControlMask;
#ifdef	META_SHIFT_MASK
	    if (se->ie_shiftmask & META_SHIFT_MASK)
		state_mask |= MetaMask;
	    else
		state_mask &= ~MetaMask;
#endif
	    break;
	default:
	    /* Ignore it */
	    break;
    }
    return (state_mask);
}

#define	INPBUFSIZE	128

/*
 * Read pending input events
 */
InputReader()
{
    struct inputevent sunevents[INPBUFSIZE];
    int         n, m;
    static int last_mask = 0;

    if ((n = read(vsdev, sunevents, INPBUFSIZE * sizeof sunevents[0])) < 0
	&& errno != EWOULDBLOCK) {
	/*
	 * Error reading events 
	 */
	/* XXX_DO_SOMETHING(); */
	return;
    }
    for (n /= sizeof sunevents[0], m = 0; m < n; m++) {
	vsEvent Xevent, Sevent;
	int mask;

	mask = ConvertEvent(&(sunevents[m]), &Xevent);
	SetCursorPosition((vsCursor *) &Xevent);	/* XXX - tacky */

	if (mask != last_mask) {
	    last_mask ^= mask;
	    Sevent.vse_device = VSE_DKB;
	    Sevent.vse_x = Xevent.vse_x;
	    Sevent.vse_y = Xevent.vse_y;
	    Sevent.vse_time = Xevent.vse_time;
	    Sevent.vse_device = VSE_DKB;
	    if (last_mask & ShiftMask) {
		if (mask & ShiftMask)
		    Sevent.vse_direction = VSE_KBTDOWN;
		else
		    Sevent.vse_direction = VSE_KBTUP;
		Sevent.vse_key = 0256;
		Deal_with_input(&Sevent);
	    }
	    if (last_mask & ControlMask) {
		if (mask & ControlMask)
		    Sevent.vse_direction = VSE_KBTDOWN;
		else
		    Sevent.vse_direction = VSE_KBTUP;
		Sevent.vse_key = 0257;
		Deal_with_input(&Sevent);
	    }
	    if (last_mask & ShiftLockMask) {
		if (mask & ShiftLockMask)
		    Sevent.vse_direction = VSE_KBTDOWN;
		else
		    Sevent.vse_direction = VSE_KBTUP;
		Sevent.vse_key = 0260;
		Deal_with_input(&Sevent);
	    }
	    if (last_mask & MetaMask) {
		if (mask & MetaMask)
		    Sevent.vse_direction = VSE_KBTDOWN;
		else
		    Sevent.vse_direction = VSE_KBTUP;
		Sevent.vse_key = 0261;
		Deal_with_input(&Sevent);
	    }
	    last_mask = mask;
	}
	if (Xevent.vse_type == VSE_MMOTION) {
	    register vsBox *b = CurrentDevice->mbox;
	    register vsCursor *m = CurrentDevice->mouse;
	    /*
	     * Has it left the box? 
	     */
	    if (m->y >= b->bottom || m->y < b->top ||
		m->x >= b->right || m->x < b->left) {
		    b->bottom = 0;
		    Deal_with_movement();
	    }
	} else
	    Deal_with_input(&Xevent);
    }
}
#endif	sun
