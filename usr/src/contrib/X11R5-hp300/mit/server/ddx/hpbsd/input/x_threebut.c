/* $Header: /host/kaukau/disk2/X11R5/R5-hp300/mit/server/ddx/hpbsd/input/RCS/x_threebut.c,v 1.1 1992/09/30 03:14:10 root Exp $ */
/*******************************************************************
 **
 **    *********************************************************
 **    *
 **    *  File:          ddx/hp/hp/x_threebut.c
 **    *
 **    *  Contents:      Routines for processing button presses and
 **    *                 emulating extra buttons when using HIL and serial
 **    *                 pointing devices.
 **    *
 **    *  Created:       4/28/88
 **    *
 **    *  Last Change:   06/07/91
 **    *
 **    *  Last Release:  8.0
 **    *
 **    *  Revision:      A.01.00
 **    *
 **    *  Author:        --gms
 **    *
 **    *  Copyright:     (c) 1988 Hewlett-Packard Company
 **    *
 **    *********************************************************
 ** 
 ********************************************************************/

/********************************************************

Copyright (c) 1988 by Hewlett-Packard Company
Copyright (c) 1988 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this software 
and its documentation for any purpose and without fee is hereby 
granted, provided that the above copyright notice appear in all 
copies and that both that copyright notice and this permission 
notice appear in supporting documentation, and that the names of 
Hewlett-Packard or  M.I.T.  not be used in advertising or publicity 
pertaining to distribution of the software without specific, written 
prior permission.

********************************************************/

#define	 NEED_EVENTS
#include <sys/types.h>
#include <stdio.h>
#if defined(__hp_osf) || defined(__apollo) || defined(hp9000)
#include <sys/time.h>
#else
#include <time.h>
#endif
#ifdef __hp_osf
#include <hp/hilioctl.h>
#endif
#include "X.h"
#include "Xproto.h"
#include "inputstr.h"
#include "hildef.h"
#include "XHPproto.h"
#include "x_serialdrv.h"

#define B1 0x01
#define B2 0x02
#define B3 0x04
#define B4 0x08
#define one_button_down(s) ((s==B1 || s==B2 || s==B3 || s==B4) ? 1 : 0)

extern	u_char		buf[];
extern	u_char		*pkt_ptr;
extern	int		pending_index;
extern	int		pending_bytes;
extern	HPInputDevice	*hpPointer;
extern	int		DeviceButtonPress, DeviceButtonRelease;
extern  InputInfo	inputInfo;

Bool		button_latch_enabled = FALSE;
u_char		*button_map;
u_char		identity_map[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
u_char		swapped_map1[] = {0,2,1,3,4,5,6,7,8,9,10,11,12,13,14,15};
HPInputDevice	*bd;
struct	dev_info   *devinfo;
#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
    struct	timeval	  wait_time = {0, 100000};	/* wait 1/10 second */
#endif /* __hpux */

static int next_device_state ();
static void generate_buttons (), put_button_event ();
static void look_for_next ();


/*************************************************************************
 * 
 * This routine processes buttons from all input devices.
 * Devices with 2 buttons emulate a third by pressing both.
 * Devices with 3 buttons emulate 5 buttons by pressing two buttons at once.
 * Button chording is enabled by default for two button mice, disabled by
 * default for devices with three or more buttons.
 * This can be overridden via the X*pointerkeys file.
 * 
 */

void
process_button (ev, dev, hil_info, code, num_buttons)
    xEvent		*ev;
    DeviceIntPtr dev;
    struct dev_info	*hil_info;
    u_char		code;
    u_char		num_buttons;
    {	
    int		old_state;
    int		curstate;
    extern	u_char button_chording;

    if (dev==inputInfo.pointer)	
	bd = hpPointer;			/* logical device is X pointer */
    else
	bd = hil_info->hil_dev;

    old_state = bd->button_state;
    if (num_buttons == 2 || num_buttons == 4)
	button_map = swapped_map1;
    else
	button_map = identity_map;

    devinfo = hil_info;

    curstate = next_device_state (dev, ev, code);
    if (curstate == ILLEGAL)
	{
	deallocate_event(ev);
	return;
	}
    bd->button_state = curstate;
    if (old_state == 0 && one_button_down(curstate) &&
	((num_buttons == 2 && button_chording != CHORDING_OFF) ||
	 button_chording == CHORDING_ON))
	 look_for_next (&ev, dev, hil_info, num_buttons, &code);

    put_button_event (dev, ev, hil_info, bd, code);
    }

/***********************************************************************
 *
 * look_for_next ()
 *
 * If button chording is enabled, we look for a second button press within
 * 100ms of the first.
 *
 */

#define	CORE_EVENT		0	
#define	EXTENSION_EVENT		1	
#define	FIRST_EXTENSION_EVENT	64
#define	MAXNAMLEN		255
#define	MOTION_BITS		3
#define	NONDATA_BYTES		7

static void
look_for_next (ev, dev, hil_info, num_buttons, code)
    xEvent		**ev;
    DeviceIntPtr	dev;
    struct		dev_info   *hil_info;
    int			num_buttons;
    u_char		*code;
    {
    extern		u_char button_latching;
    extern int		data_cnt;
    u_char		nxt_button;
    int			i, curstate;
    int			button_ds;	/* file descriptor of device. */
    xEvent		*format_ev ();
#ifdef __hp_osf
    extern		HPInputDevice	l_devs[MAX_LOGICAL_DEVS];
    unsigned int	curtime, start;
    char		dev_name[MAXNAMLEN];
    Bool		evflag = FALSE;
    u_char 		*lbuf;
    extern		HILQ *hil_qp;
    int 		j, head, tmp, index;
#endif

    button_ds = hil_info->hil_dev->file_ds;
    if (pending_index<pending_bytes)		/* already have next button */
	nxt_button = hil_info->dev_data[pending_index++];
    else
	{

#if defined(__hp_osf)
	/*
	 * OSF has a shared memory input events queue.  We use select() to
	 * wait 100ms, then check to see if a second button press is in the
	 * queue.  If we find another button event from the same device, we
	 * remove it from the queue, otherwise we return.
	 *
	 */

	head = hil_qp->hil_evqueue.head;
	lbuf = (u_char *) &hil_qp->hil_event[head].tstamp;
	start = ((*lbuf & 0x0ff) << 24) | ((*(lbuf+1) & 0x0ff) << 16) |
		   ((*(lbuf+2) & 0x0ff) << 8) | ( *(lbuf+3) & 0x0ff);

	select (NULL,NULL,NULL,NULL, &wait_time);

	for (tmp = (hil_qp->hil_evqueue.head + 1) % hil_qp->hil_evqueue.size,
	     curtime=start;
	     tmp != hil_qp->hil_evqueue.tail && curtime < start + 10000; 
	     tmp = (tmp + 1) % hil_qp->hil_evqueue.size)

	    {
	    sprintf (dev_name, "/dev/hil%d", hil_qp->hil_event[tmp].dev);

	    for (i=0; i<MAX_LOGICAL_DEVS; i++)
		if (strcmp (l_devs[i].dev_name, dev_name) == 0)
		    break;

	    if (i==MAX_LOGICAL_DEVS)
		FatalError ("Can't find input device %s\n",dev_name);

	    lbuf = (u_char *) &hil_qp->hil_event[tmp].tstamp;
	    curtime = ((*lbuf & 0x0ff) << 24) | ((*(lbuf+1) & 0x0ff) << 16) |
		       ((*(lbuf+2) & 0x0ff) << 8) | ( *(lbuf+3) & 0x0ff);

	    if (&l_devs[i] != hil_info->hil_dev) 	/* wrong device */
		{
		continue;
		}
	    if (!(hil_qp->hil_event[tmp].poll_hdr & SET1_KEY_MASK))
		{
		continue;				/* not a button event */
		}
	    if (! (hil_qp->hil_event[tmp].poll_hdr & MOTION_BITS))
		index = 0;
	    else if (l_devs[i].hil_header.flags & HIL_16_BITS)
		index = (hil_qp->hil_event[tmp].poll_hdr & MOTION_BITS) * 2;
	    else
		index = hil_qp->hil_event[tmp].poll_hdr & MOTION_BITS;
	   
	    if (hil_qp->hil_event[tmp].dev_data[index] >= BUTTON_BASE &&
		 hil_qp->hil_event[tmp].dev_data[index] < PROXIMITY_IN)
		{
		hil_info->poll_hdr = hil_qp->hil_event[tmp].poll_hdr 
						& HIL_POLL_HDR_BITS;
		pending_index = 0;
		pending_bytes = hil_qp->hil_event[tmp].size - NONDATA_BYTES;
		for (j=0; j < pending_bytes; j++)
	            hil_info->dev_data[j] = hil_qp->hil_event[tmp].dev_data[j];

		j = tmp;
		while (j > hil_qp->hil_evqueue.head)
		    {
		    hil_qp->hil_event[j] = hil_qp->hil_event[j-1];
		    j--;
		    }

		hil_qp->hil_evqueue.head = (hil_qp->hil_evqueue.head + 1) %
		    hil_qp->hil_evqueue.size;
		evflag = TRUE;
		break;
		}
	   
	    }
	if (!evflag)
	    return;
#endif /* __hp_osf */
#if defined(__hpux) || defined(hp9000)
	/*
	 * If data_cnt is > 0, the button has been read into a global buffer.
	 * If not, select on the proper device and wait 100ms.
	 *
	 */
        if (data_cnt != 0)
	    {
	    if (hil_info->hil_dev->hpflags & IS_SERIAL_DEVICE)
		{
	        get_serial_event (hil_info);
		if (!(hil_info->poll_hdr & BUTTON_DATA))
		    return;
		hil_info->dev_data[pending_index] += BUTTON_BASE; 
		}
	    else
		{
	        get_hil_event (button_ds, hil_info);
		if (!(hil_info->poll_hdr & SET1_KEY_MASK))
		    return;
		if (hil_info->poll_hdr & MOTION_MASK)
		    {
		    handle_motion_event (dev, hil_info->hil_dev, hil_info);
		    hil_info->poll_hdr &= ~MOTION_MASK;
		    }
		}
	    }
        else
	    {
	    int	mask = 1 << button_ds;

	    if (select (button_ds+1, &mask, NULL, NULL, &wait_time) <= 0)
		return;
	    else
		{
		pkt_ptr = buf;
		data_cnt = read (button_ds, buf, READ_SIZ);
		if (data_cnt <= 0)
		    return;
		if (hil_info->hil_dev->hpflags & IS_SERIAL_DEVICE)
		    {
		    get_serial_event (hil_info);
		    if (!(hil_info->poll_hdr & BUTTON_DATA))
			return;
		    hil_info->dev_data[pending_index] += BUTTON_BASE; 
		    }
		else
		    {
		    get_hil_event (button_ds, hil_info);
		    if (!(hil_info->poll_hdr & SET1_KEY_MASK))
			return;
		    if (hil_info->poll_hdr & MOTION_MASK)
			{
			handle_motion_event (dev, hil_info->hil_dev, hil_info);
			hil_info->poll_hdr &= ~MOTION_MASK;
			}
		    }
		}
    	    }
#endif /* __hpux */
#if defined(__apollo)
	/*
	 * If data_cnt is > 0, the button has been read into a global buffer.
	 * If not, select on the proper device and wait 100ms.
	 *
	 */
	{
	int	mask = 1 << button_ds;
        struct	timeval	  wait_time;			/* wait 1/10 second */

	wait_time.tv_sec = 0;
	wait_time.tv_usec = 100000;

	if (hil_info->hil_dev->hpflags & IS_SERIAL_DEVICE)
            if (data_cnt != 0)
		{
	        get_serial_event (hil_info);
		if (!(hil_info->poll_hdr & BUTTON_DATA))
		    return;
		hil_info->dev_data[pending_index] += BUTTON_BASE; 
		}
	    else if (select (button_ds+1, &mask, NULL, NULL, &wait_time) <= 0)
		return;
	    else
		{
		pkt_ptr = buf;
		data_cnt = read (button_ds, buf, READ_SIZ);
		if (data_cnt <= 0)
		    return;
		get_serial_event (hil_info);
		if (!(hil_info->poll_hdr & BUTTON_DATA))
		    return;
		hil_info->dev_data[pending_index] += BUTTON_BASE; 
		}
	else
	    {
	    select (0, NULL, NULL, NULL, &wait_time);
	    if (!get_next_button (hil_info))
		return;
	    }
	}
#endif /* __apollo */

	if (hil_info->hil_dev->hpflags & DATA_IS_8_BITS)
	    nxt_button = (hil_info->dev_data)[pending_index++];
	else if (hil_info->hil_dev->hpflags & DATA_IS_16_BITS)
	    {
	    nxt_button = ((hil_info->dev_data)[pending_index+1] << 8) |
		(hil_info->dev_data)[pending_index];
	    pending_index += 2;
	    }
	else if (hil_info->hil_dev->hpflags & DATA_IS_32_BITS)
	    {
	    nxt_button = ((hil_info->dev_data)[pending_index+3] << 24) |
		(hil_info->dev_data)[pending_index+2] << 16 |
		(hil_info->dev_data)[pending_index+1] << 8 |
		(hil_info->dev_data)[pending_index];
	    pending_index += 4;
	    }
        }

    /*
     * We've read another button, now see if it's valid and check that it is
     * a combination that causes button chording.
     *
     */

    curstate = next_device_state (dev, *ev, nxt_button);
    if (curstate == ILLEGAL)
	{
        deallocate_event(*ev);
        return;
	}
    bd->button_state = curstate;

    if (curstate == 3 && num_buttons == 2)
        {
        *code = 0x84;
        bd->ignoremask = 0x03;
        bd->savebutton = 0x85;
	return;
        }
    else if (curstate == 3 && num_buttons == 3)
        {
        *code = 0x86;
        bd->ignoremask = 0x03;
        bd->savebutton = 0x87;
	return;
        }
    else if (curstate == 6 && num_buttons == 3)
        {
        *code = 0x88;
        bd->ignoremask = 0x06;
        bd->savebutton = 0x89;
	return;
        }
    else if (curstate == 5)
	{
        if (num_buttons == 4)
	    {
	    *code = 0x88;
	    bd->ignoremask = 0x05;
	    bd->savebutton = 0x89;
	    return;
	    }
	else if (button_latching==LATCHING_ON)
	    {
	    button_latch_enabled = ~button_latch_enabled;
	    for (i=1; i<=bd->hil_header.v_button_count; i++)
		if (button_latch_enabled)
		    LatchButton(bd,i);
		else
		    UnlatchButton(bd,i);
	    *code = 0;
	    bd->ignoremask = 0x05;
	    bd->savebutton = 0;
	    return;
	    }
        }

    put_button_event (dev, *ev, hil_info, bd, *code);
    *code = nxt_button;
    *ev = format_ev ((*ev)->u.u.type, *code, hil_info->timestamp, bd, 
	NULL);
    }

/****************************************************************
 *
 * next_device_state (button)
 *	
 *
 */

static int
next_device_state (dev, ev, code)
    DeviceIntPtr dev;
    xEvent *ev;
    int	code;
    {	
    int	illegal;
    int	button;
    int	mask;
    int	new_state = bd->button_state;

    button =  (code - BUTTON_1_OFFSET) / 2;
    mask = 1 << button-1;
    if (code & 1)
        illegal = !(new_state & mask);
    else
        illegal = new_state & mask;

    if (illegal)
        {
        generate_buttons (dev, ev);
        return (ILLEGAL);
        }

    if (code & 1)
        new_state &= ~mask;
    else
        new_state |= mask;
    return (new_state);
    }

/*************************************************************************
 * 
 * generate_buttons ()	
 *	If we get here, it is because the HIL driver has lost some data.
 *	This can happen if the server is busy and the driver's buffer
 *	overflows.  
 *	If we have lost a single button release, ignore the next press and the
 *	corresponding release will fix it.
 *      If both buttons are down, or the middle button is down, we can't tell
 *      if we lost one or both of the button releases.  We assume we lost both.
 *      
 */

static void
generate_buttons (dev, ev)
    DeviceIntPtr dev;
    xEvent *ev;
    {
    bd->button_state &= ~bd->ignoremask;
    if (bd->ignoremask != 0)
        put_button_event (dev, ev, devinfo, bd, bd->savebutton);
    if (bd->button_state & 1)
        put_button_event (dev, ev, devinfo, bd, 0x81);
    if (bd->button_state & 2)
        put_button_event (dev, ev, devinfo, bd, 0x83);
    if (bd->button_state & 4)
        put_button_event (dev, ev, devinfo, bd, 0x85);
    if (bd->button_state & 8)
        put_button_event (dev, ev, devinfo, bd, 0x87);
    bd->button_state = 0;
    }

/***********************************************************************
 *
 * put_button_event (hil_info)
 *
 * The event is on the server's internal queue and will be sent to DIX,
 * unless we "deallocate" it (remove it from that queue) here.  
 * We deallocate it if:
 *
 * 1). It's an up transition, the first of a chorded pair.  For example, if
 *     the left and middle mouse buttons have been chorded to generate button 4,
 *     and the left button goes up, we want to ignore it until the middle button
 *     also goes up, then send the up transition for button 4.
 *
 * 2). It has a code of 0.  This means that it was the left-right button
 *     combination used to turn on button latching.
 *
 * 3). It's an up transition and button latching is enabled.  We'll send the
 *     up transition the second time the button is pressed.
 *
 * 4). Some test process is stealing these buttons and doesn't want real clients
 *     to see them.
 *
 */

static void
put_button_event (dev, ev, hil_info, p, code)
    DeviceIntPtr	dev;
    xEvent 		*ev;
    struct dev_info	*hil_info;
    HPInputDevice	*p;
    int			code;
    {	

#ifdef	XTESTEXT1
    extern int	on_steal_input;		/* steal input mode is on.	*/
    extern int	exclusive_steal;
#endif  /* XTESTEXT1 */

    if (bd->sent_button)			/* sent a chorded button */
	if (bd->button_state & bd->ignoremask)	/* first of pair is going up */
	    {
	    deallocate_event(ev);		/* remove it from the queue */
            return;
	    }
	else if (bd->ignoremask != 0)		/* second of pair is going up */
	    {
	    bd->ignoremask = 0;
	    bd->sent_button = 0;
	    code = bd->savebutton;		/* use saved chorded code */
	    }
    if (bd->ignoremask != 0)			/* This is a chorded button */
	bd->sent_button = 1;

    if (code==0)				/* "enable latching" case   */
	{
	deallocate_event(ev);
	return;
	}
    ev->u.u.detail = button_map[(code-BUTTON_BASE)/2] + 1;

    if (ButtonIsLatched(hil_info->hil_dev, ev->u.u.detail))
	if (ButtonIsIgnored(hil_info->hil_dev,ev->u.u.detail))
	    {
	    if (ButtonDownEvent(ev))
	        UnignoreButton(hil_info->hil_dev,ev->u.u.detail);
	    deallocate_event (ev);
	    return;
	    }
	else if (ButtonDownEvent(ev))
	    IgnoreButton(hil_info->hil_dev,ev->u.u.detail);

    if (code & UP_MASK)			/* up event was generated */
	if (dev==inputInfo.pointer)
	    ev->u.u.type = ButtonRelease;
	else
	    ev->u.u.type = DeviceButtonRelease;
    else
	if (dev==inputInfo.pointer)
	    ev->u.u.type = ButtonPress;
	else
	    ev->u.u.type = DeviceButtonPress;

#ifdef	XTESTEXT1
    if (on_steal_input)
	XTestStealKeyData(ev->u.u.detail, ev->u.u.type, MOUSE, p->coords[0], 
		p->coords[1]);

    if (exclusive_steal)
	deallocate_event(ev);
#endif	/* XTESTEXT1 */
    }
