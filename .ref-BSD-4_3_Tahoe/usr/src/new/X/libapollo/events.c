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

   /*

    Copyright (C) 1986, Leonard N. Zubkoff   All Rights Reserved	     */


/*
 *	ToDo:
 *		Up events
 *		Shiftlock support
 */

#include	<stdio.h>
#include	<sys/time.h>
#include	<sys/errno.h>
#include	<sys/wait.h>

#define do
#define hidden	    static
#define visible
#define procedure   void

#include    "Xapollo.h"
#include "/sys/ins/ios.ins.c"
#include "/sys/ins/io_traits.ins.c"
#include "/sys/ins/trait.ins.c"
#include "/sys/ins/kbd.ins.c"
#include "/sys/ins/smdu.ins.c"
#include "/sys/ins/tone.ins.c"
#include "/sys/ins/ec2.ins.c"


/* Should be qevent.h */
#define	VSE_LEFT_BUTTON	0
#define	VSE_MIDDLE_BUTTON	1
#define	VSE_RIGHT_BUTTON	2

#define ShiftKeyCode 0256
#define ControlKeyCode 0257
#define LockKeyCode 0260
#define MetaKeyCode 0261

#define SETSIZE (short)256

extern int errno;
unsigned state_mask;
extern DEVICE *CurrentDevice;
extern int borrow_flag;
static vsEvent xes, *xe;
static status_$t stp;
static ec2_$ptr_t ecs[2];
static long ec_vlist[2];
static gpr_$keyset_t KeySet;
static boolean not_in_window;
static gpr_$event_t EventType;
static unsigned char EventData[1];
static gpr_$position_t EventPosition;
static boolean HavePreviewedData;
static gpr_io_get_calls = 0;
static ec2_$ptr_t GPREc;


#include "lk201.h"

ProcessInput()
{
}


/*

  This should be replaced by a tabular implementation of some kind.

 */
int
ApolloToXKey(c)
unsigned char c;
{
    register int ret;

    state_mask = 0;
    switch (c) {
    case KBD_$R1:
    case KBD_$R1U:
        return(MetaKeyCode);
    case KBD_$LD:
    case KBD_$LDU:
        return(ControlKeyCode);
    case KBD_$LE:
    case KBD_$LEU:
        return(MetaKeyCode);
    case KBD_$LF:
    case KBD_$LFU:
        return(ShiftKeyCode);
    case KBD_$CR:
        c = '\015';
        break;
    case KBD_$BS:
        c = '\010';
        break;
    case KBD_$TAB:
    case KBD_$STAB:
    case KBD_$CTAB:
        c = '\011';
        break;
    }
    ret = LK201[c&0177];
	state_mask = (state_mask & ~(ControlMask|MetaMask|ShiftMask|ShiftLockMask))
	    | (ret & (ControlMask|MetaMask|ShiftMask|ShiftLockMask));
    return(ret);
}

InputReader()
{
    gpr_$event_t prior_event;
    int i;
    boolean flag;

    HavePreviewedData = false;

    prior_event = gpr_$no_event;
    while (1) {
    if (!borrow_flag)
        flag = gpr_$acquire_display( stp );
    /* this needs to be replaced with a "real" timestamp */
    xe->vse_time += 1;
    if (EventType != gpr_$no_event) {
        xe->vse_x = EventPosition.x_coord;
        xe->vse_y = EventPosition.y_coord;
        }
    switch (EventType) {
    case gpr_$no_event:
        if ((prior_event == gpr_$locator_stop) ||
            (prior_event == gpr_$locator)) {
            Deal_with_movement(xe);
    	    SetCursorPosition((vsCursor *) xe);
            gpr_$set_cursor_position( EventPosition, stp);
            }
        if (!borrow_flag)
            gpr_$release_display( stp );
        return;
    case gpr_$keystroke:
    	SetCursorPosition((vsCursor *) xe);
        xe->vse_device = VSE_DKB;
        xe->vse_type = VSE_BUTTON;
        if ((EventData[0] == KBD_$R1U) ||
            (EventData[0] == KBD_$LDU) ||
            (EventData[0] == KBD_$LEU) ||
            (EventData[0] == KBD_$LFU))
             xe->vse_direction = VSE_KBTUP;
        else
            xe->vse_direction = VSE_KBTDOWN;
        xe->vse_key = ApolloToXKey(EventData[0]);
        handle_mask_down();
        Deal_with_input(xe);
        handle_mask_up();
        break;
    case gpr_$buttons:
        xe->vse_device = VSE_MOUSE;
        xe->vse_type = VSE_BUTTON;
    	SetCursorPosition((vsCursor *) xe);
        switch (EventData[0]) {
        case 'a':
            xe->vse_key = VSE_LEFT_BUTTON;
            xe->vse_direction = VSE_KBTDOWN;
            break;
        case 'b':
            xe->vse_key = VSE_MIDDLE_BUTTON;
            xe->vse_direction = VSE_KBTDOWN;
            break;
        case 'c':
            xe->vse_key = VSE_RIGHT_BUTTON;
            xe->vse_direction = VSE_KBTDOWN;
            break;
        case 'A':
            xe->vse_key = VSE_LEFT_BUTTON;
            xe->vse_direction = VSE_KBTUP;
            break;
        case 'B':
            xe->vse_key = VSE_MIDDLE_BUTTON;
            xe->vse_direction = VSE_KBTUP;
            break;
        case 'C':
            xe->vse_key = VSE_RIGHT_BUTTON;
            xe->vse_direction = VSE_KBTUP;
            break;                       
        }
        Deal_with_input(xe);
        break;
    case gpr_$left_window:
        gpr_$disable_input(gpr_$buttons, stp);
        gpr_$disable_input(gpr_$keystroke, stp);
        if (!borrow_flag)
            gpr_$disable_input(gpr_$left_window, stp);
        gpr_$disable_input(gpr_$locator_stop, stp);
        gpr_$disable_input(gpr_$locator, stp);
        if (!borrow_flag)
            gpr_$force_release(i, stp);
        break;
    case gpr_$entered_window:
        if (!borrow_flag)
            flag = gpr_$acquire_display(stp);
        gpr_$enable_input(gpr_$buttons, KeySet, stp);
        gpr_$enable_input(gpr_$keystroke, KeySet, stp);
        if (!borrow_flag) {
            gpr_$enable_input(gpr_$left_window, 0L, stp);
            gpr_$enable_input(gpr_$entered_window, 0L, stp);
            }
        gpr_$enable_input(gpr_$locator_stop, 0L, stp);
        gpr_$enable_input(gpr_$locator, 0L, stp);
        break;
    case gpr_$locator:
        xe->vse_device = VSE_MOUSE;
        xe->vse_type = VSE_MMOTION;
        break;      
    case gpr_$locator_stop:
        xe->vse_device = VSE_MOUSE;
        xe->vse_type = VSE_MMOTION;
        break;      
    default:
        fprintf(stderr,"Unrecognizable event--yecccch!!!!\n");
        break;
        }
        prior_event = EventType;
    	gpr_$cond_event_wait(EventType, EventData[0], EventPosition, stp);
    }

}

/* Turn off pseudo-shift/control keys
 *  --destroys data field of event
 */
handle_mask_up()
{
    
    if (state_mask & ControlMask) {
        xe->vse_key = ControlKeyCode;
        xe->vse_direction = VSE_KBTUP;
        Deal_with_input(xe);
        }
    if (state_mask & ShiftMask) {
        xe->vse_key = ShiftKeyCode;
        xe->vse_direction = VSE_KBTUP;
        Deal_with_input(xe);
        }
};

/* Turn on pseudo-shift/control keys
 *  --preserves data field of event
 */
handle_mask_down()
{
    unsigned char data;

    data = xe->vse_key;
    if (state_mask & ControlMask) {
        xe->vse_key = ControlKeyCode;
        xe->vse_direction = VSE_KBTDOWN;
        Deal_with_input(xe);
        }
    if (state_mask & ShiftMask) {
        xe->vse_key = ShiftKeyCode;
        xe->vse_direction = VSE_KBTDOWN;
        Deal_with_input(xe);
        }
    xe->vse_key = data;
};

/* A rudimentary stream interface/type mgr for the keyboard and mouse
 */

hidden int gpr_io_$get(hpp, optp, bp, buf_lenp, stp)
	char **hpp;
	ios_$put_get_opts_t *optp;
	char *bp;
	long *buf_lenp;
	status_$t *stp;
{
	static char Buff[10];
	static int BuffLen;
	static boolean HaveBuff = false;
	boolean NoWait = ((ios_$cond_opt & *optp) != 0);
	boolean Preview = ((ios_$preview_opt & *optp) != 0);
	status_$t Status;
        boolean flag;

	if (! Preview) {
		fprintf(stderr, "Gack!  Not preview in gpr_io_$get\n");
		abort();
	}

	if (HavePreviewedData) {
		stp->all = status_$ok;
		return(1);
	}

	HavePreviewedData = false;

#ifdef PRE_SR9_5
	if (NoWait) {
		long ECValue = (long)ec2_$read(*GPREc);
		static long PrevECValue = -1;
		if (PrevECValue == ECValue)
			stp->all = ios_$get_conditional_failed;
		else { 
                if (!borrow_flag)
                    flag = gpr_$acquire_display(Status);
    			gpr_$cond_event_wait(EventType, EventData[0], EventPosition, *stp);
                if (!borrow_flag)
                    gpr_$release_display( Status );
	    		if (EventType == gpr_$no_event) {
			    	stp->all = ios_$get_conditional_failed;
				    PrevECValue = ECValue;
    			    }
	    		else
		    		PrevECValue = -1;
		        }
	       }
	else {
        if (!borrow_flag)
            flag = gpr_$acquire_display(Status);
    	gpr_$event_wait(EventType, EventData[0], EventPosition, *stp);
        if (!borrow_flag)
            gpr_$release_display( Status );
	    }
#else 
	if (NoWait) {
		gpr_$cond_event_wait(EventType, EventData[0], EventPosition, *stp);
		if (EventType == gpr_$no_event)
			stp->all = ios_$get_conditional_failed;
	    }
	else
		gpr_$event_wait(EventType, EventData[0], EventPosition, *stp);
#endif

	if (stp->all != status_$ok)
		return(0);

	HavePreviewedData = true;
	return(1);
}

int
InitInput()
{
    boolean flag;
    short i;
    short KeyClass[256];

    for (i=0; i<255; i++) do KeyClass[i] = 0;

    KeyClass[KBD_$L_BOX_ARROW] = 1;
    KeyClass[KBD_$L9] = 1;
    KeyClass[KBD_$L9S] = 1;
    KeyClass[KBD_$L9U] = 1;
    KeyClass[KBD_$CMD] = 1;
    lib_$init_set(KeySet,256);
    for (i=0; i<256; i++) do
        if (KeyClass[i] != 1) 
	    lib_$add_to_set(KeySet,256,i);
    gpr_$enable_input(gpr_$keystroke,KeySet, stp);
    gpr_$enable_input(gpr_$buttons, KeySet, stp);
    if (!borrow_flag) {
        gpr_$enable_input(gpr_$left_window, 0L, stp);
        gpr_$enable_input(gpr_$entered_window, 0L, stp);
        }
    gpr_$enable_input(gpr_$locator_stop, 0L, stp);
    gpr_$enable_input(gpr_$locator, 0L, stp);
    smd_$set_quit_char( (char)KBD_$F8S, stp );
    check_status(stp, "InitInput (Set quit char): ");
    xe = &xes;
}

hidden boolean gpr_io_$close(hpp, stp)
	char **hpp;
	status_$t *stp;
{
	stp->all = status_$ok;
	return(false);
}


hidden ios_$conn_flag_set gpr_io_$inq_conn_flags(hpp, stp)
	char **hpp;
	status_$t *stp;
{
	stp->all = status_$ok;
	return(ios_$cf_tty_mask | ios_$cf_vt_mask);
}


hidden procedure gpr_io_$get_ec(hpp, keyp, ecpp, stp)
	char **hpp;
	ios_$ec_key_t *keyp;
	ec2_$ptr_t *ecpp;
	status_$t *stp;
{
    long foo;

    foo = (long) ec2_$read( *GPREc );
	*ecpp = GPREc;
    stp->all = status_$ok;             
}

#ifdef PRE_SR9_5
  hidden long nilp[2] = {0, 0};
#else
#define nilp 0
#endif

io_$epv gpr_io_$epv = {
	nilp,
	nilp,
	nilp,
	nilp,
	gpr_io_$close,
	gpr_io_$get_ec,
	nilp,
	nilp,
	nilp,
	gpr_io_$inq_conn_flags,
	nilp,
	gpr_io_$get,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
	nilp,
};

int
MakeGPRStream()
{
	status_$t Status;
	static uid_$t gpr_io_$uid = {0x2f3e1e7a, 0x10003166};
        boolean flag;

#ifdef PRE_SR9_5
        flag = gpr_$acquire_display(stp);
#endif
	gpr_$get_ec(gpr_$input_ec,GPREc,Status);
	check_status(Status, "MakeGPRStream");
#ifdef PRE_SR9_5
        gpr_$release_display( stp );
#endif
	trait_$mgr_dcl(gpr_io_$uid, io_$trait, trait_$kind_local, &gpr_io_$epv, Status);
	return (ios_$connect("", (short) 0, gpr_io_$uid, (long) 0, &gpr_io_$epv, Status));
}

/*
   Universally used; returns -1 if an status is abnormal/error
 */
int
check_status(status, name)
status_$t status;        
char *name;
{
    if (status.all != status_$ok) {
        fprintf(stderr, "%s", name);
        error_$print(status);   
	return(-1);
    }
    return(0);
}
