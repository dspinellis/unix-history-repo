#ifndef lint
static char *rcsid_events_c = "$Header: events.c,v 10.4 86/12/17 20:36:45 swick Exp $";
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
 *		Up events on regular keys
 */

#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/time.h>
#include	<sys/errno.h>
#include	"../X/X.h"
#include	"../X/vsinput.h"
#include	"../X/Xdev.h"
#include	<sundev/kbd.h>
#ifdef	RAW_KBD
#include	<sundev/kbio.h>
#endif
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
#define	NOT_A_KEY	9

extern int errno;
extern int vsdev;
extern DEVICE *CurrentDevice;

int sunthreshold = 0;
int sunaccel = 0;

#ifdef	RAW_KBD
extern unsigned state_mask;
#endif

/*ARGSUSED*/
ProcessInput(ev)
	register vsEvent *ev;
{
	/*NOTREACHED*/
}

#include "lk201.h"

#ifdef	RAW_KBD
extern struct kiockey sunkeymap[];

/*
 * Convert from a Sun event to an X event
 */
unsigned
ConvertEvent(se, xe)
struct inputevent *se;
vsEvent *xe;
{
    int key;
    unsigned kludgekey;
    /* Map the coordinates */
    xe->vse_x = se->ie_locx;
    xe->vse_y = se->ie_locy;
    /* Map the time stamps */
    xe->vse_time = (se->ie_time.tv_usec/10000 + se->ie_time.tv_sec);
    /* Set direction */
    xe->vse_direction = (win_inputposevent(se) ? VSE_KBTDOWN : VSE_KBTUP);
    /* Sort out the event codes */

    if (se->ie_code >= VKEY_FIRSTSHIFT && se->ie_code <= VKEY_LASTSHIFT) {
	/*
	 * this makes ie_code between 0 and 0200 inclusive.
	 */
	se->ie_code -= VKEY_FIRSTSHIFT;
	key = SHIFTKEYS + se->ie_code;
    } else if (se->ie_code >= 0 && se->ie_code < 0200)
	key = sunkeymap[se->ie_code].kio_entry;
    else
	key = -1;

    kludgekey = 0;
    if (key >= 0) {
	xe->vse_device = VSE_DKB;
	xe->vse_type = VSE_BUTTON;

	/*
	 * First deal with special keycodes
	 */
	if (key & 0x80) {
		switch(key & 0xf0) {
		case SHIFTKEYS:
			/*
			 * The static count is keeping track of how many
			 * keys I have down for the given function.
			 * Only need to do this for shift and meta.
			 * On an up event I decrease the count.  If it is
			 * not the last one up then I convert to a down event
			 * which really won't do anything.  I should ignore
			 * the event, but this works.
			 * At odd times the sun keyboard gets confused and I
			 * miss an UP event.  This may get you stuck in
			 * shift mode.  I assume there is only 2 shift keys
			 * and only two meta keys.  If count ever goes above
			 * 2 I make it 2 again, assuming I have missed an up
			 * event.  If you get stuck in shifted mode, just his
			 * both shift keys and you should be fixed.
			 */
			switch(key & 0x0f) {
			case 11:
			case 14:
			case LEFTSHIFT:
			case RIGHTSHIFT: {
				static count;

				kludgekey = 0256;

				if (win_inputposevent(se)) {
					if (++count > 2)
						count = 2;
				} else if (--count > 0)
					xe->vse_direction = VSE_KBTDOWN;
				else if (count < 0)
					count = 0;
				break;
			    }
			/* LEFT/RIGHT key */
			case 9:
			default: {
				static count;

				kludgekey = 0261;

				if (win_inputposevent(se)) {
					if (++count > 2)
						count = 2;
				} else if (--count > 0)
					xe->vse_direction = VSE_KBTDOWN;
				else if (count < 0)
					count = 0;
				break;
			    }
			case 10:
			case 13:
			case CAPSLOCK:
			case SHIFTLOCK:
				kludgekey = 0260;
				break;
			case 12:
			case 15:
			case LEFTCTRL:
			case RIGHTCTRL:
				kludgekey = 0257;
				break;
			}
			break;
		case STRING:
			switch(key & 0xf) {
			default:
			case HOMEARROW:
				kludgekey = 0206;
				break;
			case UPARROW:
				kludgekey = 0252;
				break;
			case DOWNARROW:
				kludgekey = 0251;
				break;
			case LEFTARROW:
				kludgekey = 0247;
				break;
			case RIGHTARROW:
				kludgekey = 0250;
				break;
			}
			break;
		case RIGHTFUNC:
			kludgekey = RightKeys[key&0xf];
			break;
		case LEFTFUNC:
			kludgekey = LeftKeys[key&0xf];
			break;
		case TOPFUNC:
			kludgekey = TopKeys[key&0xf];
			break;
		case BOTTOMFUNC:
			kludgekey = BotKeys[key&0xf];
			break;
		case BUCKYBITS:
		case FUNNY:
		default:
			kludgekey = 0;
		}
	} else {
	/*
	 * Now deal with regular keys.
	 * Note, I look up the key in the shift/ctrl/caps tables
	 * in case the keys are not quite like a lk201.
	 * I assume that a regular key shifted/ctrled/caped is also a
	 * regular key.  This may be naive.
	 */
		if (key == '\033' /*ESC*/ )
			kludgekey = 0161;
		else if (key == '\b')
			kludgekey = 0162;
		else if (key == '\n')
			kludgekey = 0163;
		else if (key == '\r')
			kludgekey = 0275;
		else if (key == '\t')
			kludgekey = 0276;
		else if (key == '\006' /*ALT*/)
			kludgekey = 0245;
		else if (key == '\177' /*DEL*/)
			kludgekey = 0274;
		else if(state_mask & ControlMask)
			kludgekey = LK201[sunkeymap[se->ie_code+00200].kio_entry];
		else if(state_mask & ShiftMask)
			kludgekey = LK201[sunkeymap[se->ie_code+00600].kio_entry];
		else if(state_mask & ShiftLockMask)
			kludgekey = LK201[sunkeymap[se->ie_code+00400].kio_entry];
		else
			kludgekey = LK201[key];
	}
	xe->vse_key = kludgekey & 0377;
	kludgekey &= (~state_mask)&(ControlMask|ShiftMask);
    } else switch (se->ie_code) {
	case LOC_MOVE:
	    xe->vse_device = VSE_MOUSE;	 /* XXX - should query shift state here but ... */
	    xe->vse_type = VSE_MMOTION;
	    break;
	case MS_LEFT:
	    xe->vse_key = VSE_LEFT_BUTTON;
	    xe->vse_device = VSE_MOUSE;
	    xe->vse_type = VSE_BUTTON;
	    break;
	case MS_MIDDLE:
	    xe->vse_device = VSE_MOUSE;
	    xe->vse_type = VSE_BUTTON;
	    xe->vse_key = VSE_MIDDLE_BUTTON;
	    break;
	case MS_RIGHT:
	    xe->vse_key = VSE_RIGHT_BUTTON;
	    xe->vse_device = VSE_MOUSE;
	    xe->vse_type = VSE_BUTTON;
	    break;
	default:
	    xe->vse_key = NOT_A_KEY;
	    xe->vse_device = VSE_MOUSE;
	    xe->vse_type = VSE_BUTTON;
	    break;
    }
    return(kludgekey);
}

#define	INPBUFSIZE	128

/*
 * Read pending input events
 */
InputReader()
{
    struct inputevent sunevents[INPBUFSIZE];
    int         n, m;

    if ((n = read(vsdev, sunevents, INPBUFSIZE * sizeof sunevents[0])) < 0
	&& errno != EWOULDBLOCK) {
	/*
	 * Error reading events 
	 */
	/* XXX_DO_SOMETHING(); */
	return;
    }
    for (n /= sizeof sunevents[0], m = 0; m < n; m++) {
	vsEvent Xevent;

	unsigned kludgekey = ConvertEvent(&(sunevents[m]), &Xevent);

	if (Xevent.vse_type == VSE_MMOTION) {
	    if (sunthreshold) {
		int dx = Xevent.vse_x - CurrentDevice->mouse->x;
		int dy = Xevent.vse_y - CurrentDevice->mouse->y;

		if (sunthreshold <= (dx > 0 ? dx : -dx) + (dy > 0 ? dy : -dy)) {
			Xevent.vse_x = CurrentDevice->mouse->x + dx * sunaccel;
			Xevent.vse_y = CurrentDevice->mouse->y + dy * sunaccel;
		}
	
	    }
	    SetCursorPosition((vsCursor *) &Xevent);	/* XXX - tacky */
	}

	Xevent.vse_key;

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
	} else if (Xevent.vse_key != NOT_A_KEY ||
			Xevent.vse_device != VSE_MOUSE) {
	    state_mask |= kludgekey;
	    Deal_with_input(&Xevent);
	    state_mask &= ~kludgekey;
	}
    }
}
#else
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
int
ConvertEvent(se, xe)
struct inputevent *se;
vsEvent *xe;
{
    static unsigned lstate_mask;

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
	lstate_mask = (lstate_mask & ~(ControlMask|MetaMask|ShiftMask))
	    | (key & (ControlMask|MetaMask|ShiftMask));
    } else if (event_is_meta(se)) {
	/* META keystroke - map to ASCII for now */
	int key = SunToXKeyCode(se->ie_code - META_FIRST + ASCII_FIRST) | MetaMask;
	xe->vse_key = (key & 0377);
	xe->vse_device = VSE_DKB;
	xe->vse_type = VSE_BUTTON;
	lstate_mask = (lstate_mask & ~(ControlMask|MetaMask|ShiftMask))
	    | (key & (ControlMask|MetaMask|ShiftMask));
    } else switch (se->ie_code) {
	case LOC_MOVE:
	    xe->vse_device = VSE_MOUSE;		/* XXX - should query shift state here but ... */
	    xe->vse_type = VSE_MMOTION;
	    break;
	case MS_LEFT:
	    xe->vse_key = VSE_LEFT_BUTTON;
	    xe->vse_device = VSE_MOUSE;
	    xe->vse_type = VSE_BUTTON;
	    if (xe->vse_direction == VSE_KBTUP)
		lstate_mask &= ~LeftMask;
	    else
	        lstate_mask |= LeftMask;
	    goto ShiftKeys;
	case MS_MIDDLE:
	    xe->vse_device = VSE_MOUSE;
	    xe->vse_type = VSE_BUTTON;
	    xe->vse_key = VSE_MIDDLE_BUTTON;
	    if (xe->vse_direction == VSE_KBTUP)
		lstate_mask &= ~MiddleMask;
	    else
	        lstate_mask |= MiddleMask;
	    goto ShiftKeys;
	case MS_RIGHT:
	    xe->vse_key = VSE_RIGHT_BUTTON;
	    xe->vse_device = VSE_MOUSE;
	    xe->vse_type = VSE_BUTTON;
	    if (xe->vse_direction == VSE_KBTUP)
		lstate_mask &= ~RightMask;
	    else
	        lstate_mask |= RightMask;
	    goto ShiftKeys;
	default:
	    xe->vse_key = NOT_A_KEY;
	    xe->vse_device = VSE_MOUSE;
	    xe->vse_type = VSE_BUTTON;
	    if (xe->vse_direction == VSE_KBTUP)
		lstate_mask &= ~ShiftLockMask;
	    else
	        lstate_mask |= ShiftLockMask;
ShiftKeys:
	    if (se->ie_shiftmask & SHIFTMASK)
		lstate_mask |= ShiftMask;
	    else
	    	lstate_mask &= ~ShiftMask;
	    if (se->ie_shiftmask & CTRLMASK)
		lstate_mask |= ControlMask;
	    else
		lstate_mask &= ~ControlMask;
#ifdef	META_SHIFT_MASK
	    if (se->ie_shiftmask & META_SHIFT_MASK)
		lstate_mask |= MetaMask;
	    else
		lstate_mask &= ~MetaMask;
#endif
	    break;
    }
    return (lstate_mask);
}

#define	INPBUFSIZE	128

/*
 * Read pending input events
 */
InputReader()
{
    struct inputevent sunevents[INPBUFSIZE];
    int         n, m;
    static int last_mask;

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
	if (Xevent.vse_type == VSE_MMOTION) {
	    if (sunthreshold) {
		int dx = Xevent.vse_x - CurrentDevice->mouse->x;
		int dy = Xevent.vse_y - CurrentDevice->mouse->y;

		if (sunthreshold <= (dx > 0 ? dx : -dx) + (dy > 0 ? dy : -dy)) {
			Xevent.vse_x = CurrentDevice->mouse->x + dx * sunaccel;
			Xevent.vse_y = CurrentDevice->mouse->y + dy * sunaccel;
		}
	
	    }
	    SetCursorPosition((vsCursor *) &Xevent);	/* XXX - tacky */
	}

	if (mask != last_mask) {
	    last_mask ^= mask;
	    Sevent.vse_device = VSE_DKB;
	    Sevent.vse_x = Xevent.vse_x;
	    Sevent.vse_y = Xevent.vse_y;
	    Sevent.vse_time = Xevent.vse_time;
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
	} else if (Xevent.vse_key != NOT_A_KEY ||
			Xevent.vse_device != VSE_MOUSE)
	    Deal_with_input(&Xevent);
    }
}
#endif
#endif	sun
