#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

/* input.c */

#ifndef lint
static char *rcsid_input_c = "$Header: input.c,v 10.10 86/02/01 16:06:22 tony Rel $";
#endif	lint

#include <X/Xlib.h>
#include <X/Xkeyboard.h>
#include "ptyx.h"
#include <stdio.h>

static char *kypd_num = "0x.123456,789-";
static char *kypd_apl = "pxnMqrstuvlwxym";
static char *cur = "DCBA";

Input (keyboard, screen, event)
register Keyboard	*keyboard;
register Screen		*screen;
register XKeyPressedEvent *event;
{
	register int keycode = event->detail;
	register char *string;
	int	tx8bit	= screen->tx8bit;
	int	pty	= screen->respond;
	int	nbytes;
	ANSI	reply;

	string = XLookupMapping (event, &nbytes);

	keycode &= ValueMask;	/* no longer need shift bits for anything */

	reply.a_pintro = 0;
	reply.a_final = 0;
	reply.a_nparam = 0;
	reply.a_inters = 0;

	if (nbytes > 0) {
		while (nbytes-- > 0)
			unparseputc(*string++, pty);
	}
	else if (IsKeypadKey(keycode)) {
	  	if (keyboard->flags & KYPD_APL)	{
			reply.a_type   = SS3;
			unparseseq(&reply, tx8bit, pty);
			unparseputc(kypd_apl[keycode-KC_KEYPAD_0], pty);
		}
		else
			unparseputc(kypd_num[keycode-KC_KEYPAD_0], pty);
	}
	else if (IsCursorKey(keycode)) {
       		if (keyboard->flags & CURSOR_APL) {
			reply.a_type = SS3;
			unparseseq(&reply, tx8bit, pty);
			unparseputc(cur[keycode-KC_CURSOR_LEFT], pty);
		}
		else {
			reply.a_type = CSI;
			reply.a_final = cur[keycode-KC_CURSOR_LEFT];
			unparseseq(&reply, tx8bit, pty);
		}
	}
	else if (IsPFKey(keycode)) {
			reply.a_type = SS3;
			unparseseq(&reply, tx8bit, pty);
			unparseputc((char)(keycode-KC_PF1+'P'), pty);
	}
	else if (IsFunctionKey(keycode)) {
			reply.a_type = CSI;
			reply.a_nparam = 1;
			reply.a_param[0] = funcvalue(keycode);
			reply.a_final = '~';
			if (reply.a_param[0] > 0)
				unparseseq(&reply, tx8bit, pty);
	}
#ifdef ENABLE_PRINT
	if (keycode == KC_F2) TekPrint();
#endif
	return;
}

funcvalue(keycode)
{
	switch (keycode) {
		case KC_F1:	return(11);
		case KC_F2:	return(12);
		case KC_F3:	return(13);
		case KC_F4:	return(14);
		case KC_F5:	return(15);
		case KC_F6:	return(17);
		case KC_F7:	return(18);
		case KC_F8:	return(19);
		case KC_F9:	return(20);
		case KC_F10:	return(21);
		case KC_F11:	return(23);
		case KC_F12:	return(24);
		case KC_F13:	return(25);
		case KC_F14:	return(26);
		case KC_F15:	return(28);
		case KC_F16:	return(29);
		case KC_F17:	return(31);
		case KC_F18:	return(32);
		case KC_F19:	return(33);
		case KC_F20:	return(34);
		case KC_E1 :	return(1);
		case KC_E2:	return(2);
		case KC_E3:	return(3);
		case KC_E4:	return(4);
		case KC_E5:	return(5);
		case KC_E6:	return(6);
		default:	return(-1);
	}
}
