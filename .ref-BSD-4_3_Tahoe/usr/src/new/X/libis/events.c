/*
 *	$Source: /u1/X/libis/RCS/events.c,v $
 *	$Header: events.c,v 1.1 86/11/17 14:33:56 swick Rel $
 */

#ifndef lint
static char *rcsid_events_c = "$Header: events.c,v 1.1 86/11/17 14:33:56 swick Rel $";
#endif	lint

#include "is-copyright.h"

/*      events.c
 *
 *	ProcessInput	stub
 *      InputHandler	Read IS input; convert to X event
 *
 *      Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include "Xis.h"
#include <fcntl.h>
#include "lk201.h"
#include <sys/time.h>

#define IBUFSIZE	512
#define KEY_MASKS	(ControlMask|MetaMask|ShiftMask|ShiftLockMask)

/*
 *	taken from ../X/input.c
 */
#define ShiftKeyCode	0256
#define ControlKeyCode	0257
#define LockKeyCode	0260
#define MetaKeyCode	0261

#define VSE_LEFT_BUTTON		0
#define VSE_MIDDLE_BUTTON	1
#define VSE_RIGHT_BUTTON	2

extern DEVICE	*CurrentDevice;
extern int	indev;
extern vsCursor	last_mouse;

int	mouse_acceleration = 1;
int	mouse_threshold;
int	invalid_mouse;

/*
 *	ProcessInput
 */
/*ARGSUSED*/
ProcessInput(ev)
register vsEvent *ev;
{
    /*NOTREACHED*/
}

/*
 *	InputHandler
 *
 *	It is assumed that read always returns complete sequences.
 *	Uses PF[1-4] to toggle ShiftKey, LockKey, ControlKey, and MetaKey.
 */
int InputHandler()
{
    unsigned char		inbuf[IBUFSIZE];
    register unsigned char	*p = inbuf;
    register int		nread;

#ifdef DEBUG
    fflush(stdout);
#endif

    fcntl(indev, F_SETFL, FNDELAY);
    if ((nread = read(indev, (char *)inbuf, IBUFSIZE)) <= 0) {
	/* no input now */
	fcntl(indev, F_SETFL, 0);
	if (invalid_mouse)
	    --invalid_mouse;
	return;
    }

    while (p < &inbuf[nread]) {
	static unsigned	mask = 0;
	vsEvent	event;

	if (*p == VT_MOUSE) {
	    /* a mouse input */
	    int			warp = 0;

	    event.vse_device	= VSE_MOUSE;
	    p++;

	    /* which button */
	    switch (*p & (VT_MOUSE_LEFT|VT_MOUSE_MIDDLE|VT_MOUSE_RIGHT)) {
	    case VT_MOUSE_LEFT:
		event.vse_key = VSE_LEFT_BUTTON;
		break;
	    case VT_MOUSE_MIDDLE:
		event.vse_key = VSE_MIDDLE_BUTTON;
		break;
	    case VT_MOUSE_RIGHT:
		event.vse_key = VSE_RIGHT_BUTTON;
		break;
	    }

	    /* which direction */
	    switch (*p++ & (VT_MOUSE_DOWN|VT_MOUSE_UP|VT_MOUSE_NOBUTTON)) {
	    case VT_MOUSE_DOWN:
		event.vse_type		= VSE_BUTTON;
		event.vse_direction	= VSE_KBTDOWN;
		break;
	    case VT_MOUSE_UP:
		event.vse_type		= VSE_BUTTON;
		event.vse_direction	= VSE_KBTUP;
		break;
	    case VT_MOUSE_NOBUTTON:
		event.vse_type		= VSE_MMOTION;
		event.vse_direction	= VSE_KBTRAW;
		break;
	    }

	    p++; /* window */
	    p++; /* pane */

	    event.vse_x	= *p++ << 8; event.vse_x |= *p++;  /* x location */
	    event.vse_y	= *p++ << 8; event.vse_y |= *p++;  /* y location */

	    if (invalid_mouse && (event.vse_type == VSE_MMOTION)) {
		/* throw away buffered mouse motion events after warp */
		continue;
	    }

	    /* adjust for mouse acceleration if necessary */
	    if ((mouse_acceleration > 1) && (event.vse_type == VSE_MMOTION)) {
		/* accelerated mouse */
		register int delta;
		delta = event.vse_x - last_mouse.x;
		if ((delta < -mouse_threshold) || (delta > mouse_threshold)) {
		    /* accelerate mouse in x direction */
		    register short new_x;
		    warp = 1;	/* will have to warp the mouse */
		    new_x = last_mouse.x +
			((delta<0?-1:1)*mouse_threshold)*(1-mouse_acceleration)
			+ (mouse_acceleration*delta);
		    /* keep warped mouse on the screen */
		    if (new_x >= ScreenPixmap.width)
			event.vse_x = ScreenPixmap.width - 1;
		    else if (new_x < 0)
			event.vse_x = 0;
		    else
			event.vse_x = (unsigned short) new_x;
		}
		delta = event.vse_y - last_mouse.y;
		if ((delta < -mouse_threshold) || (delta > mouse_threshold)) {
		    /* accelerate mouse in y direction */
		    register short new_y;
		    warp = 1;	/* will have to warp the mouse */
		    new_y = last_mouse.y +
			((delta<0?-1:1)*mouse_threshold)*(1-mouse_acceleration)
			+ (mouse_acceleration*delta);
		    /* keep warped mouse on the screen */
		    if (new_y >= ScreenPixmap.height)
			event.vse_y = ScreenPixmap.height - 1;
		    else if (new_y < 0)
			event.vse_y = 0;
		    else
			event.vse_y = (unsigned short) new_y;
		}
	    }

	    if (invalid_mouse && (event.vse_type != VSE_MMOTION)) {
		/* don't throw away buffered mouse button events after warp */
		/* but set them to warp location */
		event.vse_x = last_mouse.x;
		event.vse_y = last_mouse.y;
	    }

	    if (warp)
		SetCursorPosition((vsCursor *) &event);
	    else
		UpdateCursorPosition((vsCursor *) &event);

	    if (event.vse_type == VSE_MMOTION) {
		register vsBox *b = CurrentDevice->mbox;
		register vsCursor *m = CurrentDevice->mouse;
		/* Has it left the box? */
		if (m->y >= b->bottom || m->y < b->top ||
		    m->x >= b->right || m->x < b->left) {
			b->bottom = 0;
			Deal_with_movement();
		}
	    } else {
		/* a mouse button press or release */
		struct timeval t;
		struct timezone	tz;
		/* this is the best we can do for now */
		gettimeofday(&t, &tz);
		event.vse_time = t.tv_sec*100 + t.tv_usec/10000;
		Deal_with_input(&event);
	    }
	} else {
	    /* keyboard input */
	    struct timeval	t;
	    struct timezone	tz;
	    /* this is the best we can do for now */
	    gettimeofday(&t, &tz);
	    event.vse_time = t.tv_sec*100 + t.tv_usec/10000;
	    event.vse_type	= VSE_BUTTON;
	    event.vse_device	= VSE_DKB;
	    /* check for PF[1-4] (ESC O [PQRS]);  assumes that if a */
	    /* complete PF escape sequence is in the buffer, it was */
	    /* produced by a PF key */
	    if ((&inbuf[nread] - p >= 3) &&
		(*p == '\033') && (*(p+1) == 'O') &&
		(*(p+2) >= 'P') && (*(p+2) <= 'S')) {
		/* one of Shift, Lock, Control, Meta (PF[1-4]) was pressed */
		switch (*(p+2)) {	/* toggle the appropriate key */
		case 'P':	/* PF1 -- ShiftKey */
		    mask ^= ShiftMask;
		    event.vse_key = ShiftKeyCode;
		    event.vse_direction =
			    (mask & ShiftMask) ? VSE_KBTDOWN : VSE_KBTUP;
		    break;
		case 'Q':	/* PF2 -- LockKey */
		    mask ^= ShiftLockMask;
		    event.vse_key = LockKeyCode;
		    event.vse_direction =
			    (mask & ShiftLockMask) ? VSE_KBTDOWN : VSE_KBTUP;
		    break;
		case 'R':	/* PF3 -- ControlKey */
		    mask ^= ControlMask;
		    event.vse_key = ControlKeyCode;
		    event.vse_direction =
			    (mask & ControlMask) ? VSE_KBTDOWN : VSE_KBTUP;
		    break;
		case 'S':	/* PF4 -- MetaKey */
		    mask ^= MetaMask;
		    event.vse_key = MetaKeyCode;
		    event.vse_direction =
			    (mask & MetaMask) ? VSE_KBTDOWN : VSE_KBTUP;
		    break;
		}
		p += 3;
		Deal_with_input(&event);
	    } else {
		/* "normal" key */
		register unsigned short key = LK201[*p++];
		static vsEvent upkey = {0,0,0,VSE_BUTTON,0,VSE_KBTUP,VSE_DKB};
		static vsEvent dnkey = {0,0,0,VSE_BUTTON,0,VSE_KBTDOWN,VSE_DKB};
		upkey.vse_time = dnkey.vse_time = event.vse_time;

		/* send UP for any Shift, Lock, Control, or Meta keys down */
		/* but not if this is a Shift or Control key */
		if ((mask & ShiftMask) && !(key & ShiftMask)) {
		    upkey.vse_key = ShiftKeyCode;
		    Deal_with_input(&upkey);
		}
		if (mask & ShiftLockMask) {
		    upkey.vse_key = LockKeyCode;
		    Deal_with_input(&upkey);
		}
		if ((mask & ControlMask) && !(key & ControlMask)) {
		    upkey.vse_key = ControlKeyCode;
		    Deal_with_input(&upkey);
		}
		if (mask & MetaMask) {
		    upkey.vse_key = MetaKeyCode;
		    Deal_with_input(&upkey);
		}

		/* set up keyboard event */
		event.vse_key		= key & 0377;
		event.vse_direction	= VSE_KBTRAW;

		switch (key & KEY_MASKS) {
		case ShiftMask:
		    /* send Shift down if not down already */
		    if (!(mask & ShiftMask)) {
			dnkey.vse_key	= ShiftKeyCode;
			Deal_with_input(&dnkey);
		    }
		    /* send the key */
		    Deal_with_input(&event);
		    /* send Shift up */
		    upkey.vse_key = ShiftKeyCode;
		    Deal_with_input(&upkey);
		    break;
		case ControlMask:
		    /* send Control down if not down already */
		    if (!(mask & ControlMask)) {
			dnkey.vse_key	= ControlKeyCode;
			Deal_with_input(&dnkey);
		    }
		    /* send the key */
		    Deal_with_input(&event);
		    /* send Control up */
		    upkey.vse_key = ControlKeyCode;
		    Deal_with_input(&upkey);
		    break;
		case ShiftLockMask:
		case MetaMask:
		default:
		    /* send the key */
		    Deal_with_input(&event);
		    break;
		}

		mask = 0;	/* nothing should be down now */
	    }
	}
	fflush(stdout);
    }
    if (invalid_mouse)
	--invalid_mouse;
}
