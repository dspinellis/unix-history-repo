#ifndef HILDEF_H
#define HILDEF_H
/* $Header: /host/kaukau/disk2/X11R5/R5-hp300/mit/server/ddx/hpbsd/input/RCS/hildef.h,v 1.1 1992/09/30 03:14:10 root Exp $ */
/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/
/*
** 	File: hildefs.h
**
**	 defines for hil devices to the X environment.
**
*/

#include  "sys/param.h"
#include  "X.h"
#include  "scrnintstr.h"
#include "misc.h"
#include "dixstruct.h"

/***************************************************************/
/* KEEP THE FOLLOWING IN SYNC WITH THE DIX DEFINITION          */
/***************************************************************/

#if (NOFILE <= 128) /* 128 is value of MAXCLIENTS in dix layer */
#define MAXSOCKS (NOFILE - 1)
#else
#define MAXSOCKS 128
#endif
#define mskcnt ((MAXSOCKS + 31) / 32)	/* size of bit array */

#define READ_SIZ	     	2000	/* leave room for partial packets*/
#define BUF_SIZ			2048	/* size of static buffer to use  */

# define MAX_X_NAMELEN		64
# define MAX_AXES		8
# define ILLEGAL  		-1
# define UP_MASK   		1 << 0
# define HIL_POLL_HDR_BITS	0xE3
# define MOTION_MASK 		0x0F
# define KEY_DATA_MASK 		0x70
# define SET1_KEY_MASK 		1 << 6
# define PROXIMITY_IN   	0x8e
# define PROXIMITY_OUT		0x8f
# define BUTTON_BASE 		0x80
# define BUTTON_1_OFFSET 	0x7e


#define VERTICAL	0
#define HORIZONTAL	1
#define MATRIX		2

#define NOWRAP		0
#define WRAP		1
#define DEFAULT		2
#define SAMESCREEN	3
#define CHANGE_BY_TWO	4

# define CHORDING_OFF		0
# define CHORDING_ON		1
# define CHORDING_DEFAULT	2

# define LATCHING_OFF		0
# define LATCHING_ON		1

# define SCREEN_CHANGE_DEFAULT	255

# define IS_SERIAL_DEVICE	0x40
# define OPEN_THIS_DEVICE	0x20
# define SECOND_LOGICAL_DEVICE	0x10
# define MERGED_DEVICE		0x08

# define DATA_SIZE_BITS		0x07

#define	HIL_ABSOLUTE	0x40	/* Device has absolute positioning data */
#define HIL_16_BITS 	0x20	/* Device has position data 16 bit accuracy */
#define HIL_IOB		0x10	/* Device has I/O description byte */
#define HIL_NUM_AXES	0x03	/* Number of axes supported */

#define HAS_LEDS	0xf0	/* Device has leds                        */
#define HILIOB_PAA	0x80	/* Device supports prompt and acknowledge */
#define HILIOB_NPA	0x70	/* Number of prompts & acknowledges supported */
#define HILIOB_PIO	0x08	/* Device supports Proximity In/Out */
#define HILIOB_BUTTONS	0x07	/* Number of buttons on device */

#define HILPRH_KEYSET   0x60    /* Keycode set bits            */
#define HILPRH_KEYSET1  0x40    /* Keycode set 1 data          */

#define NLOCK		3
#define CAPSCODE	0x37
#define KBSIZE		32	/* bytes to hold 256 bits (1 per key/button */
#define LedOn(dev, d,cd,data) \
(dev->key->modifierMap[cd] & d->led[0] ? ioctl(d->file_ds,LedCmd[0].on,data) : \
 dev->key->modifierMap[cd] & d->led[1] ? ioctl(d->file_ds,LedCmd[1].on,data) : \
 dev->key->modifierMap[cd] & d->led[2] ? ioctl(d->file_ds,LedCmd[2].on,data) : \
 ioctl(d->file_ds, LedCmd[3].on, data))
#define LedOff(dev, d,cd,data) \
(dev->key->modifierMap[cd] & d->led[0] ? ioctl(d->file_ds,LedCmd[0].off,data): \
 dev->key->modifierMap[cd] & d->led[1] ? ioctl(d->file_ds,LedCmd[1].off,data): \
 dev->key->modifierMap[cd] & d->led[2] ? ioctl(d->file_ds,LedCmd[2].off,data): \
 ioctl(d->file_ds, LedCmd[3].off, data))
#define LatchKey(d,code) (d->kb_latched[code>>3] |= (1<<(code & 7)))
#define LatchButton(d,code) (LatchKey(d,code))
#define UnlatchKey(d,code) (d->kb_latched[code>>3] &= ~(1<<(code & 7)))
#define UnlatchButton(d,code) (UnlatchKey(d,code))
#define DeviceHasLeds(d) (d->hil_header.iob & HILIOB_NPA)
#define KeyHasLed(dev,d,cd) ((dev->key->modifierMap[cd] & d->led[0]) || \
(dev->key->modifierMap[cd] & d->led[1]) || \
(dev->key->modifierMap[cd] & d->led[2]) || \
(dev->key->modifierMap[cd] & d->led[3]))

#define KeyIsLatched(d,code) (d->kb_latched[code>>3] & (1<<(code & 7)))
#define KeyIsIgnored(d,code) (d->kb_ignore[code>>3] & (1<<(code & 7)))
#define IgnoreKey(d,code) (d->kb_ignore[code>>3] |= (1<<(code & 7)))
#define UnignoreKey(d,code) (d->kb_ignore[code>>3] &= ~(1<<(code & 7)))
#define ButtonIsLatched(d,code) (KeyIsLatched(d,code))
#define ButtonIsIgnored(d,code) (KeyIsIgnored(d,code))
#define IgnoreButton(d,code) (IgnoreKey(d,code))
#define UnignoreButton(d,code) (UnignoreKey(d,code))

#define KeyDownEvent(ev) (ev->u.u.type==KeyPress | ev->u.u.type==DeviceKeyPress)
#define ButtonDownEvent(ev) (ev->u.u.type==ButtonPress | \
			     ev->u.u.type==DeviceButtonPress)
#define KeyUpEvent(ev) (ev->u.u.type==KeyRelease | \
			     ev->u.u.type==DeviceKeyRelease)
#define IsLockKey(dev,code) (dev->key->modifierMap[code] & LockMask)

#define KeyIsDown(dev, code) (dev->key && \
    (dev->key->down[code >> 3] & (1 << (code & 7))))
#define KeyIsRepeating(dev, code) (dev->kbdfeed && \
    (dev->kbdfeed->ctrl.autoRepeat || \
    (dev->kbdfeed->ctrl.autoRepeats[code >> 3] & (1 << (code & 7)))))

struct	hil_desc_record {
    int		resx;		/* x-axis counts / meter	*/
    int		resy;		/* x-axis counts / meter	*/
    int		size_x;		/* maximum x value   		*/
    int		size_y;		/* maximum y value   		*/
    u_char	flags; 		/* device characteristics	*/
    u_char	ax_num;		/* number of axes		*/
    u_short 	min_kcode;	/* minimum keycode           	*/
    u_short 	max_kcode;	/* maximum keycode           	*/
    u_char	id;		/* device HIL id		*/
    u_char	iob;		/* I/O descriptor Byte 		*/
    u_char	p_button_count;	 /* count of physical buttons 	*/
    u_char	v_button_count;	 /* count of virtual buttons  	*/
    u_char 	num_keys; 	 /* number of keys            	*/
    u_char 	num_leds; 	 /* number of leds            	*/
} ;

typedef struct _DeviceClients *DeviceClientsPtr;

typedef struct _DeviceClients {
    DeviceClientsPtr	next;
    ClientPtr		client;	  /* which client wants this device       */
    XID			resource; /* id for putting into resource manager */
    int			mode;
    int			count;	  /* # of open requests for this client   */
} DeviceClients;

typedef  struct	 _indevices {
    struct  	hil_desc_record hil_header;  /* HIL hdr 	*/
    float	scaleX; 	/* Tablet scaling 		*/
    float	scaleY; 	/* Tablet scaling 		*/
    int	        file_ds;        /* file descriptor              */
    DeviceClientsPtr clients;	/* clients using device 	*/
    ScreenPtr	pScreen;  	/* Screen pointer is on         */
    int   	repeat_rate;	/* keyboard repeat rate         */
    int   	coords[MAX_AXES];/* current coords of device    */
    Atom	x_atom;		/* atom for x type		*/
    u_int	button_state;   /* device button state          */
    int		change_xmax;
    int		change_ymax;
    int		change_ymin;
    int  	change_xmin;
    short	change_amt;
    short	id_detail;
    u_char	dev_type;	/* HIL device type		*/
    u_char  	sent_button;	/* flag for button sent		*/
    u_char  	ignoremask;	/* for button emulation         */
    u_char  	savebutton;	/* saved button			*/
    char	x_type;		/* MOUSE or KEYBOARD		*/ 
    u_char	dev_id;	  	/* device X id			*/ 
    u_char	mode;     	/* abs or rel movement  	*/
    u_char	pad0;	/* X event on button or key up  */
    u_char	pad1;	/* X event on button or key dn  */
    u_char	pad2;	/* X event on motion		*/
    char	open_cnt;	/* # clients accessing device   */
    char	dev_name[MAX_X_NAMELEN];
    char	x_name[MAX_X_NAMELEN];
    u_char	bell1[4];	/* arrays for the s300 beeper params */
    u_char	bell2[4];
    u_char	bell3[4];
    u_char	hpflags;	/* hp-specific feature flags    */
    u_char	led[NLOCK+1];
    u_char	kb_latched[KBSIZE];
    u_char	kb_ignore[KBSIZE];
}  HPInputDevice;

struct	dev_info {
    unsigned int	timestamp;
    unsigned char	poll_hdr;
    unsigned char	dev_data[36];
    HPInputDevice	*hil_dev;
}; 

struct	inputs_selected
	{ 
	long	input_mask[mskcnt];
	int	max_fd;
 	};
#endif
