/* $Header: /host/debretts/disk2/X11R5/R5-hp300/mit/server/ddx/hpbsd/input/RCS/x_hil.c,v 1.2 1993/04/21 21:41:28 root Exp $ */

/*******************************************************************
**
**    *********************************************************
**    *
**    *  File:          ddx/hp/hp/x_hil.c
**    *
**    *  Contents:      Input event procedures for the 
**    *                 X/Starbase Merged Server
**    *
**    *  Created:       4/28/88
**    *
**    *  Last Change:   12/05/88
**    *
**    *  Last Release:  IC2
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

#include "stdio.h"

#ifndef hp9000
#ifndef __apollo
#include "termio.h"
#else
#include "/sys5/usr/include/sys/termio.h"
#endif	/* __apollo */
#endif

#define	 NEED_EVENTS
#include "X.h"
#include "Xproto.h"
#include "hildef.h"
#include "hppriv.h"
#include "windowstr.h"
#include "XHPproto.h"
#include "x_hil.h"
#include "x_serialdrv.h"
#include "hpkeys.h"
#include "inputstr.h"
#include "../../../os/osdep.h"
#include <sys/times.h>
#ifdef __apollo
#include "../apollo/xshscreenpriv.h"
#endif /* __apollo */
#ifdef __hp_osf
#include <hp/hilioctl.h>
extern	HILQ *hil_qp;
#endif /* __hp_osf */
#ifdef __hpux
#include <sys/hilioctl.h>
#endif /* __hpux */
#ifdef hp9000
#include <sys/ioctl.h>
#include <hilioctl.h>
#endif
#include <errno.h>

#define	FIRST_EXTENSION_EVENT	64

#define	MIN_KEYCODE		8	
#define REPEAT_ARROW		0x2

#ifdef XTESTEXT1
/*
 * defined in xtestext1di.c
 */
extern int	on_steal_input;
extern int	exclusive_steal;
#endif /* XTESTEXT1 */

/******************************************************************
 *
 * Externs and variables referenced from other files.
 *
 */

extern int num_serial_devices;
extern SerialProcs serialprocs[];

xEvent	*format_ev();

xHPEvent  xE;

#ifdef	XINPUT
extern	int		DeviceMotionNotify;
extern	int		DeviceKeyPress;
extern	int		DeviceKeyRelease;
extern	int		DeviceButtonPress;
extern	int		DeviceButtonRelease;
extern	int		DeviceValuator;
extern	int		ProximityIn;
extern	int		ProximityOut;
#endif	/* XINPUT */
int			*dheadmotionBuf[MAX_LOGICAL_DEVS];
int			*dpmotionBuf[MAX_LOGICAL_DEVS];
extern	int 		axes_changed;
extern	int 		keyboard_click;
extern	int 		screenIsSaved;
extern	int		lastEventTime;
extern	int		x_axis, y_axis;
extern	struct		inputs_selected valid_inputs;
extern	HPInputDevice	l_devs[MAX_LOGICAL_DEVS];
extern	HPInputDevice	*hpKeyboard;
extern	HPInputDevice	*hpPointer;
extern	HPInputDevice	*hptablet_extension;
extern	WindowPtr 	*WindowTable;
extern	InputInfo inputInfo;
extern	DeviceIntPtr	screen_change_dev;
extern	DeviceIntPtr	tablet_extension_device;

extern	unsigned	tablet_xorg;
extern	unsigned	tablet_yorg;
extern	unsigned	tablet_xlimit;
extern	unsigned	tablet_ylimit;
extern	u_int		tablet_width;

static	Bool		in_tablet_extension = FALSE;

u_char		pointer_amt_bits[3];
u_char		ptr_mods, mv_mods, rs_mods, bw_mods;
u_char		buf[BUF_SIZ];
u_char		*pkt_ptr = buf;
Bool		screen_was_changed = FALSE;
Bool		reset_enabled = TRUE;
int 		hpActiveScreen = 0; 		/* active screen ndx (Zaphod) */
int		queue_events_free = MAX_EVENTS;
int		data_cnt = 0;
int		data_fd = -1;
int		pending_index;
int		pending_bytes;
int		acceleration;
int		threshold;
struct		x11EventQueue *events_queue;	/* pointer to events queue.  */
xHPEvent	*allocate_event();
struct		dev_info hil_info;		/* holds hil_data */
Bool		display_borrowed = FALSE;
#ifdef XINPUT
DeviceIntPtr	LookupDeviceIntRec ();
#endif /* XINPUT */
#if defined(__hp9000s800) && !defined(__hp9000s700)
unsigned long 	timediff;
Bool		time_set = FALSE;
#endif /* __hp9000s800 */

/******************************************************************
 *
 * Variables global to this file.
 *
 */

static	DeviceIntPtr find_deviceintrec();
static	int  process_inputs();
static	void process_hil_data();
static	void process_serial_data();
static	check_subset_and_scale();
static	move_sprite();
static	send_motion();
static	send_button();
static	move_mouse();
static	u_char	last_direction;
static	u_char	last_key;
static	u_char	last_arrow = REPEAT_ARROW;/*keycode of arrow key pressed last */
static	int	k_down_flag[4];
static	int	k_down_incx[4];
static	int	k_down_incy[4];

/****************************************************************************
 *
 * Process all available data from the input devices and put it on the server's
 * internal events queue.  When this routine is invoked, that queue is empty 
 * since the server empties it each time through its main dispatch loop.
 * 
 * The server's internal queue can hold 256 events.  If the server is busy for
 * a long time, it is possible for the queue to fill up.  In that case we 
 * can return with unread data, or data that is left in a global buffer.
 * This routine must be prepared to handle such leftover data.
 *
 * After handling leftovers, this routine finds a file descriptor with data
 * ready to be read and calls process_inputs to handle it.
 *
 */

store_inputs(ready_inputs)
    long	ready_inputs[];
    {
    int	    i;
    int     checkfd = valid_inputs.max_fd;	/* max fd valid for input*/
    int     checkword = MASKIDX(checkfd);	/* max valid word of mask*/
    long    mask[mskcnt];
    long    checkmask[mskcnt];
#if defined(__hp9000s800) && !defined(__hp9000s700)  /* building for s800 */
    struct tms 		buffer;
    int newtime;
    struct timeval tv, get_tv();

    /**********************************************************************
     *
     * On s800 machines, the time reported in HIL events has its origin at
     * the time of machine power-up.  The time reported by gettimeofday(),
     * which will be called by UpdateCurrentTime when a grab is done, always
     * has its origin at Jan. 1, 1970.  On s800s, we must compute a difference
     * to be added to the HIL times to keep the two in sync.  We must also
     * check each time via get_tv (PA-RISC fast gettimeofday) to see if the
     * time reported by gettimeofday has been changed via the date() command.
     * If so, our time difference is invalid and must be recalculated.
     */

    tv = get_tv();
    newtime = (tv.tv_sec * 1000) + (tv.tv_usec / 1000);
    if (!time_set || newtime < lastEventTime)
	{
	time_set = 1;
	lastEventTime = GetTimeInMillis();
	timediff = lastEventTime - (times(&buffer)*10);
	}
#endif /* __hp9000s800 */

    for (i=0; i<mskcnt; i++)
	checkmask[i] = 0;

    if (data_cnt > 0)				/* we have leftover data*/
	{
	process_inputs (data_fd);		/* go process it	*/
	}

    MASKANDSETBITS (mask, ready_inputs, valid_inputs.input_mask);

    BITSET(checkmask, checkfd);			/* corresponding mask	*/

    for (i=checkword; i>=0; i--)		/* for all mask words   */
	{
	while (mask[i])				/* while input available*/
	    {
	    if (mask[i] & checkmask[i]) 	/* if current fd valid  */
		{
		mask[i] &= ~checkmask[i];
		process_inputs(checkfd);	/* process its input	*/
		}
	    checkfd--;
	    checkmask[i] = checkmask[i] >> 1;
            }
	if (i>0)
	    {
	    checkfd = (i-1) * 32 + 31;
	    BITSET(checkmask, checkfd);		/* corresponding mask	*/
	    }
        }
    }
		
/****************************************************************************
 *
 * Find the device data structure that matches the file descriptor from which
 * data will be read.  Read up to 2000 bytes (HIL driver buffer is only 512)
 * from that file descriptor.  From the data read, get 1 HIL data packet.
 * That packet may contain up to 8 keycodes and 1 motion event.  In the case
 * of a barcode reader in ASCII mode, each keycode may generate up to 6
 * X input events.  The worst case is therefore 49 X events from 1 HIL data
 * packet.
 *
 */

#define TIME_POLL_BYTES		5	/* bytes indicating time and poll*/

static int process_inputs (file_ds)
    int	file_ds;			/* file_ds to read from      	 */
    {
    int			i;
    u_char		*hil_ptr;
    HPInputDevice 	*indevice = NULL; 
    DeviceIntPtr 	dev;
    xHPEvent		*xHP;
    Bool 		done = FALSE;

    for (i=0; i<MAX_LOGICAL_DEVS; i++)
	{
	if (file_ds == l_devs[i].file_ds) 
	    {
	    indevice = &(l_devs[i]);
	    break;
	    }
	}
	
    dev = find_deviceintrec(indevice);

    if (data_cnt == 0 &&	 		/* no leftover data	    */
      !(indevice->hpflags & IS_SERIAL_DEVICE))
	{
	pkt_ptr = buf;
	data_cnt = read (file_ds, buf, READ_SIZ);
	/* We get here with nothing actually to read after a SIGHUP 
	   so ignore it */
	if (data_cnt == -1 && errno == EWOULDBLOCK)
	    {
	    data_cnt = 0;
	    return;
	    }
	}
    while (data_cnt > 0 || 			/* data yet to be processed */
      (indevice->hpflags & IS_SERIAL_DEVICE && !done))
	{
	if (queue_events_free <= MAXHILEVENTS)	/* no room on server queue  */
	    {
	    if (data_fd == -1)
		data_fd = file_ds;		/* save the file descriptor */
	    return;
	    }
	hil_ptr = (unsigned char *) &hil_info;/* place to copy packet to*/
	if (indevice->hpflags & IS_SERIAL_DEVICE)
	    {
	    done = get_serial_event (hil_ptr);
	    process_serial_data (dev, indevice, &(hil_info));
	    }
	else
	    {
	    get_hil_event (file_ds, hil_ptr);
#if defined(__hp9000s800) && !defined(__hp9000s700)  /* building for s800 */
	    hil_info.timestamp += timediff;
#endif /* __hp9000s800 */
	    process_hil_data (dev, indevice, &(hil_info));
	    }
	lastEventTime = hil_info.timestamp;	/* Used by ScreenSaver  */
	}
    data_fd = -1;
    if (xE.b.u.u.type != 0)			/* at least 1 motion event */
	{
        xHP = allocate_event();			/* get current queue pointer*/
        *xHP = xE;				/* copy from global struct  */
        xE.b.u.u.type = 0;			/* mark it as processed	    */
	}
    }

/***************************************************************************
 *
 * Given the HP device structure, find the DIX device structure that
 * logically corresponds to it.  There is a one-to-one correspondence,
 * expect when the keyboard is also the X pointer, a tablet is subsetted,
 * or an input device is merged with the X pointer or X keyboard.
 *
 * Callers: process_inputs(), read_shmhil().
 *
 */

static
DeviceIntPtr find_deviceintrec (indevice)
    HPInputDevice *indevice;
    {
    PtrFeedbackPtr	p;
    DeviceIntPtr dev = NULL;

    if (indevice != NULL)
	{
	hil_info.hil_dev = indevice;		/* input device struct ptr  */
	if (hptablet_extension &&
	    indevice->file_ds==hptablet_extension->file_ds && 
	    in_tablet_extension)
	    {
	    hil_info.hil_dev = hptablet_extension;
	    dev = tablet_extension_device;
	    }
	else if (indevice==hpKeyboard || (indevice->x_type==KEYBOARD && 
		(indevice->hpflags & MERGED_DEVICE)))
	    dev = inputInfo.keyboard;
	else if (indevice==hpPointer || (indevice->x_type==MOUSE && 
		(indevice->hpflags & MERGED_DEVICE)))
	    dev = inputInfo.pointer;
#ifdef XINPUT
	else
	    dev = LookupDeviceIntRec (indevice->dev_id);

    	p = dev->ptrfeed;
	if (p != NULL)
	    {
	    threshold = p->ctrl.threshold;
	    acceleration = p->ctrl.num / p->ctrl.den;
	    }
#endif /* XINPUT */
	if (acceleration == 0)
	    acceleration = 1;
	}
    else
	FatalError ("X server couldn't find current input device - Aborting.\n");
    return (dev);
    }

/****************************************************************************
 *
 * Get one HIL data packet from the data that was previously read.
 * If the buffer only contains a partial packet, read the rest.
 *
 * This function may also be called from x_threebut.c.
 *
 */

get_hil_event (fd, dest)
    int		fd;
    char	*dest;
    {
    int	packet_size;
    int	i;
    struct dev_info *info = (struct dev_info *) dest;

    packet_size = *pkt_ptr++;		/* 1st byte is size	    */
    if(data_cnt < packet_size)		/* We got a partial packet  */
	{
	data_cnt += read(fd,		/* get rest of packet */ 
			pkt_ptr + data_cnt, 
			packet_size - data_cnt);

	if(data_cnt != packet_size)
	    FatalError ("Unable to read all of an HIL data packet.  Server exiting! \n");
	}
    for (i=1; i<packet_size; i++)	/* copy the current packet  */
        *dest++ = *pkt_ptr++;

    info->timestamp = (info->timestamp - 1) * 10;
    info->poll_hdr &= HIL_POLL_HDR_BITS;/* zero nonsignifcant bits  */
    data_cnt -= packet_size;		/* fix unprocessed data cnt */
    pending_index = 0;
    pending_bytes = packet_size - 1  - TIME_POLL_BYTES;
    }

/****************************************************************************
 *
 * process the HIL data packet and generate X input events as needed.
 *
 */

#define UP_LEFT_ARROW		0xf8	/* HIL key codes for arrow keys. */
#define DOWN_LEFT_ARROW		0xf9
#define UP_DOWN_ARROW		0xfa
#define DOWN_DOWN_ARROW		0xfb
#define UP_UP_ARROW		0xfc
#define DOWN_UP_ARROW		0xfd
#define UP_RIGHT_ARROW		0xfe
#define DOWN_RIGHT_ARROW	0xff
#define HIL_PROXIMITY		0x4f

static u_char	code[2];

static void process_hil_data (dev, phys, info)
    DeviceIntPtr 	dev;
    HPInputDevice	*phys;
    struct		dev_info	*info;
    {
    xEvent		*ev;
    int			count;
    u_char		type, keyset, kcode, bcode, *hil_code;
    struct hil_desc_record *h = &phys->hil_header;

    while (pending_index < pending_bytes ) 
	{  
	if (info->poll_hdr & MOTION_MASK)
	    {
	    handle_motion_event (dev, phys, info);
	    info->poll_hdr &= ~MOTION_MASK;
	    }
    
	if (info->poll_hdr & KEY_DATA_MASK)
	    {  
	    keyset   = info->poll_hdr & HILPRH_KEYSET;
	    hil_code = code;
	    if (phys->hpflags & DATA_IS_8_BITS)
		*hil_code = (info->dev_data)[pending_index++];
	    else if (phys->hpflags & DATA_IS_16_BITS)
		{
		*hil_code = ((info->dev_data)[pending_index] << 8) |
		    (info->dev_data)[pending_index+1];
		pending_index += 2;
		}
	    /* Check if cursor keys are repeating. */
    
	    switch (*hil_code)
		{
		case DOWN_DOWN_ARROW	:
		case UP_DOWN_ARROW		:
		case UP_LEFT_ARROW		:
		case DOWN_LEFT_ARROW	:
		case UP_RIGHT_ARROW		:
		case DOWN_RIGHT_ARROW	:
		case UP_UP_ARROW		:
		case DOWN_UP_ARROW		:
		    last_arrow = *hil_code;
		    break;
		case	REPEAT_ARROW		:
		    if ((keyset==HILPRH_KEYSET1) && last_arrow!=REPEAT_ARROW)
		        *hil_code = last_arrow;
		    break;
		default:
		    break;
		}


	    if (phys->dev_type == BARCODE)
		hil_code = ascii_to_code[*hil_code];

	    for (count=0; (count==0 || *hil_code != 0); count++)
		{
#ifdef XINPUT
		/* proximity HIL codes cause a different event type.
		   However, proximity is not reported for devices being
		   used as the X pointer, unless they have no buttons
		   (like a touchscreen), in which case the proximity is
		   treated as button 1.
		   */

		kcode = ((u_char) *hil_code) >> 1;	/* same code up & down*/
		kcode += MIN_KEYCODE;		        /* avoid mouse codes. */

		/* Check to see if this is a "down" keycode for a key that is
		   already down.  If so, and autorepeat has been disabled for
		   this key, ignore the key and return.
		   */

		if (!(*hil_code % 2) && KeyIsDown(dev,kcode) &&
		    !KeyIsRepeating(dev,kcode))
		    return;

		bcode = *(hil_code++);
		if ((h->iob & HILIOB_PIO) && kcode == HIL_PROXIMITY)
		    if (dev!=inputInfo.pointer)
			{
			type = (bcode & UP_MASK) ? ProximityOut : ProximityIn;
			ev= format_ev (type, 0, info->timestamp, phys, NULL);
			return;
			}
		    else if (h->p_button_count == 0)
			bcode -= 0x0e;			/* make it button 1 */
		    else
			return;	/* proximity not reported for X pointer */
#endif /* XINPUT */
		if (bcode >= BUTTON_BASE && bcode < PROXIMITY_IN)
		    {
		    if (phys == hptablet_extension && phys->open_cnt == 0)
			return;
		    if (dev==inputInfo.pointer)
			if (bcode & UP_MASK) 
			    type = ButtonRelease;
			else
			    type = ButtonPress;
		    else
			if (bcode & UP_MASK) 
			    type = DeviceButtonRelease;
			else
			    type = DeviceButtonPress;
		    ev= format_ev (type, kcode, info->timestamp, phys, NULL);
		    process_button (ev, dev, info, bcode, h->p_button_count);
		    }
		else
		    {
		    if (dev==inputInfo.keyboard)
			if (bcode & UP_MASK) 
			    type = KeyRelease;
			else
			    type = KeyPress;
		    else
			if (bcode & UP_MASK) 
			    type = DeviceKeyRelease;
			else
			    type = DeviceKeyPress;
		    ev= format_ev (type, kcode, info->timestamp, phys, NULL);

		    parse_keycode (dev, phys, ev);
		    }
	        }
	    }
	}
   }

/****************************************************************************
 *
 * process the serial data packet and generate X input events as needed.
 *
 */

static u_int	s_code[2];

static void process_serial_data (dev, phys, info)
    DeviceIntPtr 	dev;
    HPInputDevice	*phys;
    struct		dev_info	*info;
    {
    xEvent		*ev;
    int			count;
    u_int		*hil_code;
    u_char		type, kcode;
    int button_count = phys->hil_header.p_button_count;

    while (pending_index < pending_bytes ) 
	{  
	if (info->poll_hdr & MOTION_DATA)
	    {
	    handle_motion_event (dev, phys, info);
	    }
    
	hil_code = s_code;

	if (phys->hpflags & DATA_IS_8_BITS)
	    *hil_code = (info->dev_data)[pending_index++];
	else if (phys->hpflags & DATA_IS_16_BITS)
	    {
	    *hil_code = ((info->dev_data)[pending_index+1] << 8) |
		(info->dev_data)[pending_index];
	    pending_index += 2;
	    }
	else if (phys->hpflags & DATA_IS_32_BITS)
	    {
	    *hil_code = ((info->dev_data)[pending_index+3] << 24) |
		(info->dev_data)[pending_index+2] << 16 |
		(info->dev_data)[pending_index+1] << 8 |
		(info->dev_data)[pending_index];
	    pending_index += 4;
	    }

	if (info->poll_hdr & KEY_DATA)
	    {
	    /* Check to see if this is a "down" keycode for a key that is
	       already down.  If so, and autorepeat has been disabled for
	       this key, ignore the key and return.
	       */

	    kcode = (u_char) (*hil_code >> 1); /* same code up & down */

	    if (*hil_code & UP_MASK) 
		if (dev==inputInfo.keyboard)
		    type = KeyRelease;
		else
		    type = DeviceKeyRelease;
	    else
		{
		if (KeyIsDown(dev,kcode) && !KeyIsRepeating(dev,kcode))
		    return;
		if (dev==inputInfo.keyboard)
		    type = KeyPress;
		else
		    type = DeviceKeyPress;
		}

	    ev= format_ev (type, kcode, info->timestamp, phys, NULL);
	    parse_keycode (dev, phys, ev);
	    }
	else if (info->poll_hdr & BUTTON_DATA)
	    {
	    if (*hil_code & UP_MASK) 
		if (dev==inputInfo.pointer)
		    type = ButtonRelease;
		else
		    type = DeviceButtonRelease;
	    else
		if (dev==inputInfo.pointer)
		    type = ButtonPress;
		else
		    type = DeviceButtonPress;

	    kcode = *hil_code + BUTTON_BASE;
	    if (dev==inputInfo.pointer && kcode > PROXIMITY_OUT)
		return;
	    if (phys == hptablet_extension && phys->open_cnt == 0)
	        return;
	    ev= format_ev (type,kcode,info->timestamp,phys,NULL);
	    process_button (ev, dev, info, kcode, button_count);
	    }
#ifdef XINPUT
	else if (info->poll_hdr & PROXIMITY_DATA)
	    {
	    /* proximity HIL codes cause a different event type.
	       However, proximity is not reported for devices being
	       used as the X pointer, unless they have no buttons
	       (like a touchscreen), in which case the proximity is
	       treated as button 1.
	       */
	    if (dev!=inputInfo.pointer)
		{
		type = (*hil_code & UP_MASK) ? ProximityOut : ProximityIn;
		ev= format_ev (type, 0, info->timestamp, phys, NULL);
		return;
		}
	    else if (button_count == 0)
		{
		kcode = 1;			/* make it button 1 */
	        ev= format_ev (type, kcode, info->timestamp, phys, NULL);
		if (phys == hptablet_extension && phys->open_cnt == 0)
		    return;
		process_button (ev, dev, info, kcode, button_count);
		}
	    else
		return;	/* proximity not reported for X pointer */
	    }
#endif /* XINPUT */
	}
   }

/*******************************************************************
 *
 * handle_motion_event()
 *
 */

handle_motion_event (dev, phys, info)
    DeviceIntPtr dev;
    HPInputDevice *phys;
    struct dev_info *info;
    {
    int			i, type, bytes_coord;
    int			tmp, coords[MAX_AXES];
    char  		*sdata;
    u_char  		*udata;
    HPInputDevice	*log;

    if (dev==inputInfo.pointer)
	{
	type = MotionNotify;
	log = hpPointer;
	}
    else
	{
	type = DeviceMotionNotify;
	log = phys;
	}
    if (phys->hpflags & DATA_IS_32_BITS)
	bytes_coord = 4;
    else if (phys->hpflags & DATA_IS_16_BITS)
	bytes_coord = 2;
    else
	bytes_coord = 1;

    pending_index += phys->hil_header.ax_num * bytes_coord;

    if (phys->hil_header.flags & HIL_ABSOLUTE)		/* absolute device */
	{
	udata = info->dev_data; 
	for (i=0; i < (u_char) phys->hil_header.ax_num; i++, udata+=bytes_coord)
	    if (bytes_coord == 1)
		coords[i] = *udata;
	    else if (bytes_coord == 2)
		coords[i] = *udata | *(udata+1) << 8;
	    else if (bytes_coord == 4)
		coords[i] = *udata | (*(udata+1) << 8) | (*(udata+2) << 16) | 
			    (*(udata+3) << 24);

	if (!check_subset_and_scale (&dev, phys, &log, coords))
	    return;
	}
    else
	{
	sdata = (char *) info->dev_data; 
	for (i=0; i < (u_char) phys->hil_header.ax_num; i++, sdata+=bytes_coord)
	    if (bytes_coord == 1)
		coords[i] = *sdata;
	    else if (bytes_coord == 2)
		coords[i] = *(sdata+1) << 8 | (*sdata & 0x0ff);
	    else if (bytes_coord == 2)
		coords[i] = (*(sdata+3) << 24) | ((*(sdata+2) << 16) & 0x0ff)|
			    ((*(sdata+1) << 8) & 0xff) | (*sdata & 0x0ff);
	}

    if (phys==hpPointer && axes_changed)
	{
	tmp = coords[0];
	coords[0] = coords[x_axis];
	if (y_axis==0)
	    coords[1] = tmp;
	else
	    coords[1] = coords[y_axis];
	}
    if (!(phys->hil_header.flags & HIL_ABSOLUTE) &&
	phys->dev_type != NINE_KNOB)
	coords[1] = -coords[1];

    process_motion (dev, phys, log, coords);
    (void) format_ev (type, 0, info->timestamp, log, &xE);
    }

/*******************************************************************
 *
 * check_subset_and_scale()
 * all we care about is the x and y coordinates.
 *
 */

static
check_subset_and_scale (dev, phys, log, c)
    DeviceIntPtr	*dev;
    HPInputDevice	*phys;
    HPInputDevice	**log;
    int			c[];
    {	
    extern u_char screen_change_amt;

    if (tablet_width)
	if (c[0]< tablet_xorg || c[0] > tablet_xlimit ||
	    c[1]> tablet_yorg || c[1] < tablet_ylimit)
	    {
	    in_tablet_extension = TRUE;
	    *dev = tablet_extension_device;
	    *log = hptablet_extension;
	    }
	else
	    {
	    in_tablet_extension = FALSE;
	    }
		
    if (*log == hpPointer)
	{
	if (*dev == screen_change_dev)
	    c[0] = (float) (c[0]-tablet_xorg) * phys->scaleX-screen_change_amt;
	else
	    c[0] = (float) (c[0]-tablet_xorg) * phys->scaleX;
        c[1] = (*log)->pScreen->height -
		((float) (c[1]-tablet_ylimit) * phys->scaleY);
	}
    else
	{
	if (*dev == screen_change_dev)
	    c[0] -= screen_change_amt;
        c[1] = phys->hil_header.size_y - c[1]; /* Y-coord  reversed.*/
	}
    if (c[0]==(*log)->coords[0] && c[1]==(*log)->coords[1])
	return (FALSE);
    c[0] -= (*log)->coords[0];
    c[1] -= (*log)->coords[1];
    return (TRUE);
    }

unsigned char lockcode = CAPSCODE;

/****************************************************************************
 *
 * parse_keycode (dev, phys, ev, x_type)
 *   Parse keycode information.
 *   Buttons from a three-button mouse also end up here.
 *
 */

#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
struct	 _LedCmd {
    int 	on;
    int		off;
    } LedCmd[] = {{HILP1,HILA1},{HILP2,HILA2},{HILP3,HILA3},{HILP3,HILA3}};
#endif

int parse_keycode (dev, phys, ev)
    DeviceIntPtr 	dev;
    HPInputDevice	*phys;
    xEvent		*ev;
    {
#ifdef	XTESTEXT1
    extern u_char	xtest_command_key;	 /* defined in xtestext1dd.c */
#endif	/* XTESTEXT1 */
    u_char	down_mods;
    char 	ioctl_data[12];

    if (hpPointer->x_type == KEYBOARD)
	if (hpKeyboard->hpflags & SECOND_LOGICAL_DEVICE &&
	    ((ev->u.keyButtonPointer.pad1==hpKeyboard->dev_id ||
	      ev->u.keyButtonPointer.pad1==hpPointer->dev_id ) &&
	     move_sprite (dev, hpPointer, ev)))
		return;
	else
	    if (ev->u.keyButtonPointer.pad1==hpPointer->dev_id &&
		(move_sprite (dev, hpPointer, ev)))
		return;
    
    /* allow borrow-mode switching on Domain/OS machines */
#ifdef __apollo

    if (ev->u.u.detail==borrow_mode)
	if ((hpKeyboard->hpflags & SECOND_LOGICAL_DEVICE &&
	    (ev->u.keyButtonPointer.pad1==hpKeyboard->dev_id ||
	     ev->u.keyButtonPointer.pad1==hpPointer->dev_id)) ||
	    ev->u.keyButtonPointer.pad1==hpKeyboard->dev_id)
	{
	get_down_modifiers (inputInfo.keyboard->key->down, &down_mods);
	if ((bw_mods & down_mods) == bw_mods)
	    {
	    extern unsigned char last_code;	/* in smd_input.c */
	    unsigned long timestamp;

	    timestamp = ev->u.keyButtonPointer.time;
	    deallocate_event (ev);	/* eat the borrow mode key */
	    ev = format_ev (KeyRelease, borrow_mode_mods[0]+MIN_KEYCODE,
		timestamp, phys, NULL);
	    ev = format_ev (KeyRelease, borrow_mode_mods[1]+MIN_KEYCODE,
		timestamp, phys, NULL);
	    last_code = 0;
	    leave_X();
	    }
	}
#endif /* __apollo */


    /* allow reset only from the X system keyboard,
	   and only if reset is enabled.		*/

    if (ev->u.u.detail==reset && reset_enabled)
	if ((hpKeyboard->hpflags & SECOND_LOGICAL_DEVICE &&
	    (ev->u.keyButtonPointer.pad1==hpKeyboard->dev_id ||
	     ev->u.keyButtonPointer.pad1==hpPointer->dev_id)) ||
	    ev->u.keyButtonPointer.pad1==hpKeyboard->dev_id)
	{
	get_down_modifiers (inputInfo.keyboard->key->down, &down_mods);
#ifdef NOT_DONE /* We don't have hp extensions, thus no ResetManager */
	if (((rs_mods & down_mods) == rs_mods) && !SendEventToResetManager())
	    GiveUp();
	}
#else
	if ((rs_mods & down_mods) == rs_mods)
	    GiveUp();
	}
#endif

#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)

    /* Special case handling for the Caps Lock modifier and LED.
       If a key is pressed that is bound to the Lock modifier,
       turn on the Caps Lock LED and treat the key as latched.
       However, do this only if a client has not overridden the
       default use of the Caps Lock LED via the HPConfigureInput
       protocol request.  */

    if (IsLockKey(dev, ev->u.u.detail))		/* lock modifier pressed */
	{
	if (ev->u.u.detail != lockcode)		/* was changed by xmodmap*/
	    {
	    UnlatchKey(phys, lockcode);
	    LatchKey(phys, ev->u.u.detail);
	    lockcode = ev->u.u.detail;
	    }
	}
    else if (ev->u.u.detail == lockcode) 	/* is former lock modifier */
	{
	UnlatchKey(phys, lockcode);
	lockcode = 0xff;
	}

    if (KeyIsLatched(phys, ev->u.u.detail))
	if (KeyIsIgnored(phys,ev->u.u.detail))
	    {
	    if (KeyDownEvent(ev))
	        UnignoreKey(phys,ev->u.u.detail);
	    deallocate_event (ev);
	    return;
	    }
	else if (KeyDownEvent(ev))
	    IgnoreKey(phys,ev->u.u.detail);

    if (DeviceHasLeds(phys) && KeyHasLed(dev,phys,ev->u.u.detail))
	if (KeyUpEvent(ev))
	    LedOff(dev, phys, ev->u.u.detail, ioctl_data);
	else
	    LedOn(dev, phys, ev->u.u.detail, ioctl_data);

#endif /* __hpux */

#ifdef	XTESTEXT1
    if (on_steal_input)
	{ 
	XTestStealKeyData(ev->u.u.detail, ev->u.u.type, phys->x_type, 
	    ev->u.keyButtonPointer.rootX, ev->u.keyButtonPointer.rootY);
	if (exclusive_steal)
	    { 
	    if (ev->u.u.detail != xtest_command_key)
		deallocate_event (ev);
	    }
	else if (ev->u.u.detail == xtest_command_key)
	    deallocate_event (ev);
	}
#endif /* XTESTEXT1 */
    }

/************************************************************************
 *
 * This routine checks to see if the key should be interpreted as a 
 * sprite movement or a button.
 *
 */

static move_sprite (dev, phys, ev)
    DeviceIntPtr	dev;
    HPInputDevice	*phys;
    xEvent		*ev;
    {
    u_char      down_mods;
    u_char      key = ev->u.u.detail;
    u_char      type = ev->u.u.type;
    int		inc;
    Bool	motion_mods;

    get_down_modifiers (dev->key->down, &down_mods);
    if (down_mods & (~ptr_mods & ~mv_mods))
	motion_mods = FALSE;
    else if ((down_mods & ptr_mods) == ptr_mods)
	motion_mods = TRUE;
    else
	motion_mods = FALSE;

    if (!(down_mods & mv_mods))
        inc = pointer_move;
    else if ((down_mods & mv_mods) == pointer_amt_bits[0])
        inc = pointer_mod1_amt;
    else if ((down_mods & mv_mods) == pointer_amt_bits[1])
        inc = pointer_mod2_amt;
    else if ((down_mods & mv_mods) == pointer_amt_bits[2])
        inc = pointer_mod3_amt;
    else
	motion_mods = FALSE;

    k_down_incy[DOWN] = inc;
    k_down_incx[RIGHT] = inc;
    k_down_incy[UP] = inc * -1;
    k_down_incx[LEFT] = inc * -1;

    if (key==cursor_down && type==KeyPress && motion_mods)
        return (send_motion (phys, ev, 0, inc, DOWN));
    else if (key==cursor_down && type==KeyRelease)
        {
        k_down_flag[DOWN] = 0;
        return (1);
        }
    else if (key==cursor_left && type==KeyPress && motion_mods)
        return (send_motion (phys, ev, inc * -1, 0, LEFT));
    else if (key==cursor_left && type==KeyRelease)
        {
        k_down_flag[LEFT] = 0;
        return (1);
        }
    else if (key==cursor_right && type==KeyPress && motion_mods)
        return (send_motion (phys, ev, inc, 0, RIGHT));
    else if (key==cursor_right && type==KeyRelease)
        {
        k_down_flag[RIGHT] = 0;
        return (1);
        }
    else if (key==cursor_up && type==KeyPress && motion_mods)
        return (send_motion (phys, ev, 0, inc * -1, UP));
    else if (key==cursor_up && type==KeyRelease)
        {
        k_down_flag[UP] = 0;
        return (1);
        }
    else 
	{
	if (type==KeyPress)
	    type = ButtonPress;
	if (type==KeyRelease)
	    type = ButtonRelease;

	if (key == button_1)
	    return (send_button (ev, type, 1));
	else if (key == button_2)
	    return (send_button (ev, type, 2));
	else if (key == button_3)
	    return (send_button (ev, type, 3));
	else if (key == button_4)
	    return (send_button (ev, type, 4));
	else if (key == button_5)
	    return (send_button (ev, type, 5));
	else if (key == button_6)
	    return (send_button (ev, type, 6));
	else if (key == button_7)
	    return (send_button (ev, type, 7));
	else if (key == button_8)
	    return (send_button (ev, type, 8));
	}
    return (0);
    }

/****************************************************************************
 *
 * Send motion information from the keyboard, when it is the pointer device.
 *
 */

static send_motion (phys, ev, x, y, which)
    HPInputDevice	*phys;
    xEvent		*ev;
    int 		x, y, which;
    {
    int 		coords[MAX_AXES];
    int	i;

    for (i=0; i<4; i++)
        if (i != which && k_down_flag[i] != 0)
            {
            x += k_down_incx[i];
            y += k_down_incy[i];
            }
    coords[0] = x;
    coords[1] = y;
    k_down_flag[which] = 1;
    deallocate_event(ev);
    process_motion (inputInfo.pointer, phys, hpPointer, coords);
    ev = format_ev (MotionNotify, 0, ev->u.keyButtonPointer.time,hpPointer,&xE);
    return (1);
    }

/****************************************************************************
 *
 * Send button information from the keyboard, when it is the pointer device.
 *
 */

static send_button (ev, direction, bcode)
    xEvent *ev;
    u_char direction, bcode;
    {

    if (bcode == last_key && direction == last_direction)
	deallocate_event(ev);
    else
	{
	ev->u.u.type = direction;
	ev->u.u.detail = bcode;
	last_key = bcode;
	last_direction = direction;
#ifdef XTESTEXT1
	if (on_steal_input)
	    XTestStealKeyData(ev->u.u.detail, ev->u.u.type, MOUSE, 
		ev->u.keyButtonPointer.rootX, ev->u.keyButtonPointer.rootY);
#endif /* XTESTEXT1 */
	}

    return (1);
    }

/****************************************************************************
 *
 * process_motion (hil_info)
 *
 * This function may also be called from x_threebut.c and x_tablet.c.
 * It requires the motion passed to be a relative amount.
 * dev_hp and dev are the logical devices, phys is the actual device.
 *
 */

#define DEF_ACCELERATION	1
#define EDGE_L			1 << 0
#define EDGE_R			1 << 1
#define EDGE_T			1 << 2
#define EDGE_B			1 << 3

#define OffRightEdge(log)  (log->coords[0] > (log->change_xmax + \
			    (int) log->change_amt) ? EDGE_R : 0)
#define OffLeftEdge(log)   (log->coords[0] < (log->change_xmin - \
			    (int) log->change_amt) ? EDGE_L : 0) 
#define OffTopEdge(log)    (log->coords[1] < (log->change_ymin - \
			    (int) log->change_amt) ? EDGE_T : 0) 
#define OffBottomEdge(log) (log->coords[1] > (log->change_ymax + \
			    (int) log->change_amt) ? EDGE_B : 0) 

process_motion (dev, phys, log, c)
    DeviceIntPtr dev;
    HPInputDevice *phys, *log;
    int	c[];
    {
    int		i;
    unsigned int state = 0;
#ifdef XTESTEXT1
    extern int playback_on;
#endif
   
    /* Compute x,y taking care of desired threshold and acceleration
     * No acceleration if we're playing back a recorded test script.
     * No acceleration for absolute pointing devices.
     * No acceleration if we're using the default (1) acceleration.
     */

#ifdef XTESTEXT1
    if (!playback_on)
#endif
	{
	if (!(phys->hil_header.flags & HIL_ABSOLUTE) && 
	    (acceleration > DEF_ACCELERATION))
	    {
	    for (i=0; i < (u_char) log->hil_header.ax_num; i++)
	        if ( (c[i] - threshold) > 0)
		    c[i] = threshold + (c[i] - threshold) * acceleration;
	        else if ( (c[i] + threshold) < 0)
		    c[i] = (c[i] + threshold) * acceleration - threshold;
	    }
	}

    /*
     * If this is the pointer or a device whose input is merged
     * with the pointer, accumulate the motion and maintain a current position.
     * If this is an relative device, save the current movement.
     */

    if (log == hpPointer || (phys->hil_header.flags & HIL_ABSOLUTE))
	for (i=0; i< (int) log->hil_header.ax_num; i++)
	    log->coords[i] = log->coords[i] + c[i];
    else
	for (i=0; i< (u_char) log->hil_header.ax_num; i++)
	    log->coords[i] = c[i];

    /*
     * Active Zaphod implementation:
     *    Change the screen if we have more than one screen,
     *	  and the screen change device has gone off one of the edges,
     *    and the device is not grabbed and confined.
     */


#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
    if ( screenInfo.numScreens > 1 && 
	 log->dev_id == screen_change_dev->id &&
        (!dev->grab || !dev->grab->confineTo))
	{
	if (state = (OffRightEdge(log) | OffLeftEdge(log) | 
	    OffTopEdge(log) | OffBottomEdge(log)))
	    {
	    if (!screen_was_changed)
		change_the_screen (dev, phys, log, state);
	    }
	else 
	    /*
	     * Needed for the case where a tablet is the X pointer device.
	     * Once we change screens, we want to avoid immediately changing
	     * back.  We change when we enter the screen change area and 
	     * do not change again until after we have left it.
	     */
	    screen_was_changed = FALSE;
	}
#endif /* __hpux */
    if (phys == hptablet_extension && log->open_cnt == 0)
	return;
    /*
     * Clip the cursor to stay within the bound of screen.
     */
    if (log == hpPointer &&
        (!hpConstrainXY (&log->coords[0], &log->coords[1])))
	   return;
    move_mouse (log, lastEventTime);
    }

/****************************************************************************
 *
 * change_the_screen()
 * We have more than one screen, and the screen_change_device has been moved 
 * off one of the edges.  Change to another screen.
 *
 */

#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)

#define INCREMENT_SCREEN_BY_ONE(p,l) (screenInfo.screens[(p->myNum+1) % \
    screenInfo.numScreens])

#define DECREMENT_SCREEN_BY_ONE(p,l) (p->myNum != 0 ? \
    screenInfo.screens[p->myNum-1] : \
    screenInfo.screens[screenInfo.numScreens - 1])

#define INCREMENT_SCREEN_BY_TWO(p,l) (screenInfo.screens[(p->myNum+2) % \
    screenInfo.numScreens])

#define DECREMENT_SCREEN_BY_TWO(p,l) (p->myNum > 1 ? \
    screenInfo.screens[p->myNum-2] : \
    screenInfo.screens[p->myNum + screenInfo.numScreens - 2])

change_the_screen (dev, phys, log, state)
    DeviceIntPtr dev;
    HPInputDevice *phys, *log;				/* logical device */
    unsigned int state;
    {
    ScreenPtr  	pScreen;
    WindowPtr	pRootWin;
    int		tx, ty;

    if (screen_col_wrap == DEFAULT)
	{
	if (screen_orientation == VERTICAL)
	    screen_col_wrap = WRAP;
	else
	    screen_col_wrap = NOWRAP;
	}

    if (screen_row_wrap == DEFAULT)
	{
	if (screen_orientation == HORIZONTAL)
	    screen_row_wrap = WRAP;
	else
	    screen_row_wrap = NOWRAP;
	}

    pScreen = log->pScreen;

    switch (state)
	{
	case EDGE_L:
	    if (screen_row_wrap == NOWRAP &&
		(screen_orientation == VERTICAL ||
	        (pScreen->myNum == 0  ||
	        (pScreen->myNum == 2 && screen_orientation == MATRIX ))))
		return;

	    if (screen_orientation == VERTICAL)
		{
		if (screen_row_wrap == CHANGE_BY_TWO)
		    {
		    log->pScreen = DECREMENT_SCREEN_BY_TWO(pScreen,log);
		    }
		}
	    else if (screen_orientation == HORIZONTAL)
		{
		log->pScreen = DECREMENT_SCREEN_BY_ONE(pScreen,log);
		}
	    else if (screen_orientation == MATRIX)
		{
		if (pScreen->myNum % 2)
		    {
		    log->pScreen = DECREMENT_SCREEN_BY_ONE(pScreen,log);
		    }
	        else if (screen_row_wrap == WRAP)
		    {
		    if (!(screenInfo.numScreens == 3 && pScreen->myNum == 2))
	    	        {
		        log->pScreen = INCREMENT_SCREEN_BY_ONE(pScreen,log);
		        }
		    }
		else
		    break;
		}
    
	    if (!(log->hil_header.flags & HIL_ABSOLUTE))
		log->coords[0] += (log->pScreen->width - log->change_xmin);
	    break;
	case EDGE_R:
	    if (screen_row_wrap == NOWRAP &&
		(screen_orientation == VERTICAL ||
	        (pScreen->myNum == 3  ||
	        (pScreen->myNum == 1 && screen_orientation == MATRIX ))))
		return;

	    if (screen_orientation == VERTICAL)
		{
		if (screen_row_wrap == CHANGE_BY_TWO)
		    {
		    log->pScreen = INCREMENT_SCREEN_BY_TWO(pScreen,log);
		    }
		}
	    else if (screen_orientation == HORIZONTAL)
		{
		log->pScreen = INCREMENT_SCREEN_BY_ONE(pScreen,log);
		}
	    else if (screen_orientation == MATRIX)
		{
	        if (pScreen->myNum % 2)
		    {
	            if (screen_row_wrap == WRAP)
		        {
		        log->pScreen = DECREMENT_SCREEN_BY_ONE(pScreen,log);
		        }
		    else
			break;
		    }
		else if (!(screenInfo.numScreens == 3 && pScreen->myNum == 2))
		    {
		    log->pScreen = INCREMENT_SCREEN_BY_ONE(pScreen,log);
		    }
		else if (screen_row_wrap != WRAP)
		    break;
		}

	    if (!(log->hil_header.flags & HIL_ABSOLUTE))
		log->coords[0] -= (pScreen->width);
	    break;
	case EDGE_T:
	    if (screen_col_wrap == NOWRAP &&
		(screen_orientation == HORIZONTAL ||
	        (pScreen->myNum == 3  ||
	        (pScreen->myNum == 2 && screen_orientation == MATRIX ))))
		return;

	    if (screen_orientation == HORIZONTAL)
		{
		if (screen_col_wrap == CHANGE_BY_TWO)
		    {
		    log->pScreen = INCREMENT_SCREEN_BY_TWO(pScreen,log);
		    }
		}
	    else if (screen_orientation == VERTICAL)
		{
		log->pScreen = INCREMENT_SCREEN_BY_ONE(pScreen,log);
		}
	    else if (screen_orientation == MATRIX)
	        {
		if (pScreen->myNum >= 2) 
		    {
		    if (screen_col_wrap == WRAP)
		        {
		        log->pScreen = DECREMENT_SCREEN_BY_TWO(pScreen,log);
		        }
		    else
			break;
		    }
		else if (!(screenInfo.numScreens == 3 && pScreen->myNum == 1))
		    {
		    log->pScreen = INCREMENT_SCREEN_BY_TWO(pScreen,log);
		    }
		else if (screen_col_wrap != WRAP)
		    break;
		}
    
	    if (!(log->hil_header.flags & HIL_ABSOLUTE))
		log->coords[1] += (pScreen->height);
	    break;
	case EDGE_B:
	    if (screen_col_wrap == NOWRAP &&
		(screen_orientation == HORIZONTAL ||
	        (pScreen->myNum == 0  ||
	        (pScreen->myNum == 1 && screen_orientation == MATRIX))))
		return;

	    if (screen_orientation == HORIZONTAL)
		{
		if (screen_col_wrap == CHANGE_BY_TWO)
		    {
		    log->pScreen = DECREMENT_SCREEN_BY_TWO(pScreen,log);
		    }
		}
	    else if (screen_orientation == VERTICAL)
		{
		log->pScreen = DECREMENT_SCREEN_BY_ONE(pScreen,log);
		}
	    else if (screen_orientation == MATRIX)
	        {
		if (pScreen->myNum >= 2) 
		    {
		    log->pScreen = DECREMENT_SCREEN_BY_TWO(pScreen,log);
		    }
		else if (screen_col_wrap == WRAP)
		    {
		    if (! (screenInfo.numScreens == 3 && pScreen->myNum == 1))
		        {
		        log->pScreen = INCREMENT_SCREEN_BY_TWO(pScreen,log);
		        }
		    }
		else
		    break;
		}

	    if (!(log->hil_header.flags & HIL_ABSOLUTE))
		log->coords[1] -= (pScreen->height);
	    break;
	}

    getPrivScreenPtr(pScreen)->CursorOff(pScreen);
    pScreen = log->pScreen;
    screen_was_changed = TRUE;
    set_scale_and_screen_change (log);
    getPrivScreenPtr(pScreen)->ChangeScreen(pScreen);
    if (phys == hptablet_extension)
	{
	tx = phys->coords[0] < tablet_xorg ? 0 : pScreen->width;
        ty = (float) phys->coords[1] * phys->scaleY;
	NewCurrentScreen(pScreen, tx, ty);
	}
    else
        NewCurrentScreen(pScreen, log->coords[0], log->coords[1]);

    hpActiveScreen = pScreen->myNum;
    if (dev->grab && dev->grab->cursor)
	pScreen->DisplayCursor(pScreen,dev->grab->cursor);
    else if (!(pRootWin = WindowTable[pScreen->myNum]))
	pScreen->DisplayCursor(pScreen,(CursorPtr) NULL);
    else
	pScreen->DisplayCursor(pScreen,pRootWin->optional->cursor);
    }
#endif /* __hpux || __hp_osf */

/****************************************************************************
 *
 * move_mouse ()
 * move the sprite, if the device is the pointer.
 * Also move it if some other device is sending MotionNotify events.
 * In any case, send a motion event to dix.
 *
 * This routine may also be called from xtest1dd.c
 *
 */

static
move_mouse (log, event_time)
    HPInputDevice	*log;			/* logical device  */
    int	event_time;				/* event timestamp */
    {
    int			i;
    int			id 	= log->dev_id;
    int			axes = log->hil_header.ax_num;
   
#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
     register 		hpPrivScreenPtr phpPriv =
	 			getPrivScreenPtr(log->pScreen);
    if (log == hpPointer)
#ifdef SPECIAL_68K_OSF
	miPointerMoveCursor(log->pScreen, log->coords[0], log->coords[1], 1);
#else
        (*phpPriv->MoveMouse) (log->pScreen, log->coords[0], log->coords[1], 1);
#endif
#endif /* __hpux */

#ifdef __apollo
    if (log == hpPointer)
	{
	xshScreenPrivPtr pScreenPriv;

	pScreenPriv = XSH_SCREEN_PRIV (log->pScreen);
	(*pScreenPriv->MoveCursor) (pScreenPriv,log->coords[0],log->coords[1]);
#ifdef XTESTEXT1
	if (on_steal_input)
	    check_for_motion_steal (log->coords[0], log->coords[1]);
#endif /* XTESTEXT1 */
	}
#endif /* __apollo */

    *dpmotionBuf[id]++ = event_time;
    for (i=0; i<axes; i++)
	*dpmotionBuf[id]++ = log->coords[i];
    
    if((dheadmotionBuf[id] + 100 * (axes+1)) == dpmotionBuf[id])
	dpmotionBuf[id] = dheadmotionBuf[id];
    }

/**************************************************************************
 *
 * Called by: hpMouseProc during device initialization, process_motion
 * 	whenever we change screens.
 *
 * This routine sets the scaling factor to be used for absolute pointing
 * devices like graphics tablets.  Input from these devices is scaled to
 * the screen size.  If we have a multi-screen environment, the scaling
 * factor must be changed whenever the screen changes.
 *
 * This routine also sets the margin at the screen edge that will be used
 * to change screens.  For tablets, this is initially 0, allowing the 
 * entire tablet surface to be used by the application.  If a tablet is 
 * the X pointer and a multi-screen environment is being used, the 
 * screen_change_amt variable should be initialized to some value (like 30)
 * to define a area at the tablet edges that will cause the screen to change.
 *
 * Tablet subsetting adds more complications.  The user can define a subset
 * area that used as the X pointer, while the remainder of the tablet surface
 * is treated as a second logical device.  It is this second logical device
 * that controls screen changes.
 *
 */
set_scale_and_screen_change (d)
    HPInputDevice *d;
    {
    int tmp, resx_mm, resy_mm;

    /* Absolute device: graphics tablet or touchscreen */

    if (d->hil_header.flags & HIL_ABSOLUTE)
	{
	resx_mm = d->hil_header.resx / 1000;
	resy_mm = d->hil_header.resy / 1000;

    	/* Tablet subsetting enabled and this is the pointer region.
    	   This is called only during initialization, since when
    	   we change screens, the device is the second logical device. */

	if (tablet_width && d->dev_id == inputInfo.pointer->id)
	    {
	    tablet_xorg = tablet_xorigin * resx_mm;
	    tablet_xlimit = tablet_xorg + tablet_width * resx_mm;
	    tmp  = d->hil_header.size_y - (tablet_yorigin * resy_mm);
	    tablet_yorg = tmp > 0 ? tmp : 0;
	    tmp = tablet_yorg - (tablet_height * resy_mm);
	    if (tmp > 0)
		tablet_ylimit = tmp;
	    else
		{
		tablet_ylimit = 0;
		tablet_height = tablet_yorg / resy_mm;
		}
	    d->scaleX = ((float) d->pScreen->width) /
	        ((float)tablet_width * resx_mm );
	    d->scaleY = ((float) d->pScreen->height) /
		((float)tablet_height * resy_mm );
	    d->change_xmin = 0;
	    d->change_xmax = d->pScreen->width;
	    d->change_amt = 0;
	    }
	else
	
	/* This code is called if we are initializing the second logical
	   device, or if tablet subsetting is not enabled.  It is also
	   called when we are changing screens with a tablet as the X
	   pointer device.

	 */
	    {
	    /* 
	      Set scale for the case where the tablet is the X pointer.
	      The scale is also returned to clients via XHPListInputDevices.
	     */
	    d->scaleX = ((float) (d->pScreen->width+2*screen_change_amt)) /
		((float)d->hil_header.size_x);
	    d->scaleY = ((float)d->pScreen->height) /
		((float)d->hil_header.size_y);
	    if (tablet_width)
		{
		/* If this is the second logical device, we must also
		   change the scale of the X pointer.  Since input from
		   absolute extension devices is not scaled, the
		   screen change amounts units are tablet counts.
		 */
		hpPointer->scaleX = ((float) d->pScreen->width) /
	            ((float)tablet_width * resx_mm );
		hpPointer->scaleY = ((float) d->pScreen->height) /
		    ((float)tablet_height * resy_mm );
		d->change_xmin =  resx_mm * screen_change_amt;
		d->change_xmax = d->hil_header.size_x - d->change_xmin;
		d->change_ymin =  resy_mm * screen_change_amt;
		d->change_ymax = d->hil_header.size_y - d->change_xmin;
		d->change_amt = 0;
		}
	    else
		/* The tablet is the X pointer.  Screen change units
		   are in pixels, since the input will be scaled.
		 */
		{
		d->change_xmin =  1;
		d->change_xmax = d->pScreen->width - 2;
		d->change_ymin =  1;
		d->change_ymax = d->pScreen->height - 2;
		d->change_amt = 0;
		}
	    }
	}
    else

    /* This code is called when a relative device is initialized,
       and when we are changing screens with a relative device.
       These devices  (mice, trackballs, dialboxes, spaceballs)
       cause us to change screens by generating values that are
       beyond the edge of the screen.
     */

	{
	d->change_xmin = 0;
	d->change_xmax = d->pScreen->width;
	d->change_ymin = 0;
	d->change_ymax = d->pScreen->height;
	d->change_amt = screen_change_amt;
	}
    }

/****************************************************************************
 *
 *  queue_motion_event ()
 *  This is a convenience routine for xosSetCursorPosition.
 *  It is used to artifically generate a motion event when WarpPointer
 *  request is made.
 *
 */

queue_motion_event (dev_p)
    HPInputDevice	*dev_p;
    {
    int			coords[MAX_AXES];
    extern		TimeStamp currentTime;

    coords[0] = 0;
    coords[1] = 0;
    process_motion (inputInfo.pointer, dev_p, dev_p, coords);
    (void) format_ev (MotionNotify, 0, currentTime.milliseconds, dev_p, NULL);
    xE.b.u.u.type = 0;
    }

/****************************************************************************
 *
 * format_ev ( )
 *	format one or more key, button, motion or proximity xEvents.
 *	This routine assumes devices have less than 6 axes, or report only
 *      one axis per event.
 */

#define AXES_PER_EVENT 6

xEvent *
format_ev (type, detail, event_time, log, event)
    u_char		type;
    u_char		detail;
    unsigned  int 	event_time;
    HPInputDevice	*log;
    xHPEvent		*event;
    {
    int i, j;
    int n_axes = log->hil_header.ax_num;
    INT32 *ip;
    xEvent *ret = NULL;

    for (i=0; i<=n_axes/AXES_PER_EVENT; i++)
	{
	if (event==NULL)
	    {
	    if (xE.b.u.u.type != 0)	/* we have a previous motion event  */
		{
        	event = allocate_event();/* queue it before the new event    */
        	*event = xE;
        	xE.b.u.u.type = 0;
		}
	    event = allocate_event();
	    }
	if (!ret)
	    ret = (xEvent *) event;

	event->b.u.u.type = type;
	event->b.u.u.detail = detail;
	event->b.u.keyButtonPointer.time = event_time;
	event->b.u.keyButtonPointer.rootX = hpPointer->coords[0];
	event->b.u.keyButtonPointer.rootY = hpPointer->coords[1];
	event->b.u.keyButtonPointer.pad1 = log->dev_id;
#ifdef XINPUT
	if (type >= FIRST_EXTENSION_EVENT)
	    {
	    event->b.u.keyButtonPointer.pad1 |= MORE_EVENTS;
	    event->x.type = DeviceValuator;
	    event->x.deviceid = log->dev_id;

	    if (log->hpflags & NON_CONTIGUOUS_DATA)
		for (j=0; j < (u_char) log->hil_header.ax_num; j++)
		    {
		    if (log->coords[j]!=0)
			{
			event->x.num_valuators = 1;
			event->x.first_valuator = j;
			event->x.valuator0 = log->coords[j];
			return (ret);
			}
		    }
	    else
		{
		event->x.num_valuators = log->hil_header.ax_num;
		event->x.first_valuator = 0;
		ip = &event->x.valuator0;
		for (j=0; j<6; j++)
		    if ( i < (u_char) log->hil_header.ax_num)
			*ip++ =  log->coords[j];
		    else
			*ip++ =  0;
		}
	    }
#endif /* XINPUT */
	}

    return (ret);
    }

/********************************************************************
 *
 * ProcessInputEvents()
 * This routine is invoked from the dispatcher to route events.  
 * It invokes the dix routines to do this.
 *
 */

#define CLICK_VOICE 		2

ProcessInputEvents()
    {
    int	click, id, i;
    INT32 *ip;
    int	count;
    xHPEvent	*event;
    DeviceIntPtr	dev;
    Bool checkedscreensave = FALSE;

#if defined(__hp_osf)
    if (hil_qp->hil_evqueue.head != hil_qp->hil_evqueue.tail)
	read_shmhil();
#endif /* __hp_osf */

    while ( events_queue->head != events_queue->tail) 
	{
	if (!checkedscreensave)
	    {
	    if (screenIsSaved==SCREEN_SAVER_ON && !display_borrowed)
		SaveScreens (SCREEN_SAVER_OFF, ScreenSaverReset);
	    checkedscreensave = TRUE;
	    }
        event =  &((events_queue->events)[(events_queue->head)]);

	switch (event->b.u.u.type) 
	    {
	    case KeyPress:
	        if (keyboard_click)
		    beep(CLICK_VOICE,800,keyboard_click,1);
	    case KeyRelease:
		dev = (DeviceIntPtr) LookupKeyboardDevice ();
		(*dev->public.processInputProc) (event, dev, 1);
	        break;
	    case ButtonPress:
	    case ButtonRelease:
	    case MotionNotify:
		dev = (DeviceIntPtr) LookupPointerDevice ();
		(*dev->public.processInputProc) (event, dev, 1);
	        break;
	    default:
#ifdef XINPUT
		id = event->b.u.keyButtonPointer.pad1 & DEVICE_BITS;
		if (!(event->b.u.keyButtonPointer.pad1 & MORE_EVENTS))
		    count=1;
		else
		    count=2;
		dev = LookupDeviceIntRec (id);
		if (dev == NULL)
		    break;
		if (event->b.u.u.type == DeviceKeyPress)
		    {
		    if (dev->kbdfeed)
		        click = (int)((double)(dev->kbdfeed->ctrl.click) * 
				15.0 / 100.0);
		    if (click)
		        beep(CLICK_VOICE,800,click,1);
		    }
		else if (event->b.u.u.type == DeviceMotionNotify)
		    {
		    ip = &event->x.valuator0;
		    for (i=0; i < (u_char) event->x.num_valuators; i++)
			dev->valuator->axisVal[i] = *(ip+i);
		    }
		(*dev->public.processInputProc) (event, dev, count);
#endif /* XINPUT */
	    break;
	    }

	if (events_queue->head == WR_EVENTS)
	    events_queue->head = 0;
	else
	    events_queue->head++;

        }
    queue_events_free	= WR_EVENTS;
    }

#ifdef __hp_osf
/******************************************************************
 * 
 * This routine removes data from the HIL shared memory event queue,
 * and processes it through the server ddx input event processing code.
 *
 */

#define		NONDATA_BYTES	7
#define		MAXNAMLEN	255

read_shmhil()
    {
    int			i, head;
    char		dev_name[MAXNAMLEN];
    u_char		*buf;
    DeviceIntPtr 	dev;
    void		process_hil_data();
    xHPEvent		*xHP;

    while (hil_qp->hil_evqueue.head != hil_qp->hil_evqueue.tail)
        {
	head = hil_qp->hil_evqueue.head;
	sprintf (dev_name, "/dev/hil%d", hil_qp->hil_event[head].dev);
	for (i=0; i<MAX_LOGICAL_DEVS; i++)
	    if (strcmp (l_devs[i].dev_name, dev_name) == 0)
		break;

	if (i==MAX_LOGICAL_DEVS)
	   	    FatalError ("Can't find input device %s\n queue head = %d\n queue tail = %d\n event timestamp = 0x%x\n event pollheader = 0x%x\n event size = %d\n",
			dev_name,
			head,hil_qp->hil_evqueue.tail,
			hil_qp->hil_event[head].tstamp,
			hil_qp->hil_event[head].poll_hdr,
			hil_qp->hil_event[head].size);
	dev = find_deviceintrec (&l_devs[i]);
	buf = (u_char *) &hil_qp->hil_event[head].tstamp;
	hil_info.timestamp = ((*buf & 0x0ff) << 24) |
			     ((*(buf+1) & 0x0ff) << 16) |
			     ((*(buf+2) & 0x0ff) << 8) |
			     ( *(buf+3) & 0x0ff);

	hil_info.timestamp = (hil_info.timestamp - 1) * 10;
	hil_info.poll_hdr = hil_qp->hil_event[head].poll_hdr & HIL_POLL_HDR_BITS;

	pending_bytes = hil_qp->hil_event[head].size - NONDATA_BYTES;
	pending_index = 0;
	for (i=0; i < pending_bytes; i++)
	    hil_info.dev_data[i] = hil_qp->hil_event[head].dev_data[i];

	lastEventTime = hil_info.timestamp;	/* Used by ScreenSaver  */
	process_hil_data (dev, hil_info.hil_dev, &(hil_info));
        hil_qp->hil_evqueue.head = (hil_qp->hil_evqueue.head + 1) % 
		hil_qp->hil_evqueue.size;	/* MUST use real head pointer,
						   process_button may have
						   incremented it.      */
	}

    if (xE.b.u.u.type != 0)			/* at least 1 motion event */
	{
        xHP = allocate_event();			/* get current queue pointer*/
        *xHP = xE;				/* copy from global struct  */
        xE.b.u.u.type = 0;			/* mark it as processed	    */
	}
    }
#endif /* __hp_osf */

Bool
get_serial_event (hil_ptr)
    struct dev_info *hil_ptr;				/* holds hil_data */
    {
    int i, status;

    hil_ptr->timestamp = GetTimeInMillis();
    hil_ptr->poll_hdr = 0;
    pending_index=0;
    pending_bytes=0;
    bzero (hil_ptr->dev_data, 36);
    for (i=0; i<num_serial_devices; i++)
	if (hil_ptr->hil_dev->file_ds==serialprocs[i].fd)
	    {
 	    status = (*(serialprocs[i].read))
		(hil_ptr->hil_dev->file_ds,
	    	hil_ptr->dev_data, 
		&hil_ptr->poll_hdr, 
		&pending_bytes);
	    break;
	    }
    if (status==READ_SUCCESS)
	return(FALSE);
    else
	return(TRUE);
    }
