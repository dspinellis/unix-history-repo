/* $XConsortium: XChKeyCon.c,v 11.12 91/01/26 14:12:12 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	*/

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#include "Xlibint.h"

XChangeKeyboardControl(dpy, mask, value_list)
    register Display *dpy;
    unsigned long mask;
    XKeyboardControl *value_list;
{
    unsigned long values[8];
    register unsigned long *value = values;
    long nvalues;
    register xChangeKeyboardControlReq *req;

    LockDisplay(dpy);
    GetReq(ChangeKeyboardControl, req);
    req->mask = mask;

    /* Warning!  This code assumes that "unsigned long" is 32-bits wide */

    if (mask & KBKeyClickPercent)
	*value++ = value_list->key_click_percent;
	
    if (mask & KBBellPercent)
    	*value++ = value_list->bell_percent;

    if (mask & KBBellPitch)
    	*value++ = value_list->bell_pitch;

    if (mask & KBBellDuration)
    	*value++ = value_list->bell_duration;

    if (mask & KBLed)
    	*value++ = value_list->led;

    if (mask & KBLedMode)
	*value++ = value_list->led_mode;

    if (mask & KBKey)
        *value++ = value_list->key;

    if (mask & KBAutoRepeatMode)
        *value++ = value_list->auto_repeat_mode;


    req->length += (nvalues = value - values);

    /* note: Data is a macro that uses its arguments multiple
       times, so "nvalues" is changed in a separate assignment
       statement */

    nvalues <<= 2;
    Data32 (dpy, (long *) values, nvalues);
    UnlockDisplay(dpy);
    SyncHandle();

    }
