/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell and Rick Macklem.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dtopreg.h	7.1 (Berkeley) %G%
 */

/* 
 * Mach Operating System
 * Copyright (c) 1992 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie Mellon
 * the rights to redistribute these changes.
 */
/*
 * HISTORY
 * $Log:	dtop.h,v $
 * Revision 2.3  92/03/05  17:08:17  rpd
 * 	Define how many buttons and coordinates we can take.
 * 	[92/03/05            af]
 * 
 * Revision 2.2  92/03/02  18:32:17  rpd
 * 	Created from DEC specs:
 * 	"DESKTOPinterconnect Description and Protocol Specification"
 * 	Version 0.9, Jun 17 1991
 * 	"Open Desktop Bus, Locator Device Protocol Specification"
 * 	Version 0.4, Dec 13 1990
 * 	"Open Desktop Bus, Keyboard Device Protocol Specification"
 * 	Version 0.7, Jan 9 1991
 * 	[92/01/19            af]
 * 
 */
/*
 *	File: dtop.h
 * 	Author: Alessandro Forin, Carnegie Mellon University
 *	Date:	1/92
 *
 *	Definitions for the Desktop serial bus (i2c aka ACCESS).
 */

#ifndef	_DTOP_H_
#define	_DTOP_H_

#define	DTOP_MAX_DEVICES	14
#define	DTOP_MAX_MSG_SIZE	36	/* 3 hdr + 32 data + 1 checksum */

typedef struct {

	unsigned char	dest_address;	/* low bit is zero */
	unsigned char	src_address;	/* ditto */
	union {
	    struct {
		unsigned char	len : 5, /* message byte len */
				sub : 2, /* sub-address */
				P : 1;	 /* Control(1)/Data(0) marker */
	    } val;
	    unsigned char	bits;	/* quick check */
	} code;

	/* varzise, checksum byte at end */
	unsigned char	body[DTOP_MAX_MSG_SIZE-3];

} dtop_message, *dtop_message_t;

/*
 * Standard addresses
 */

#define	DTOP_ADDR_HOST		0x50	/* address for the (only) host */
#define	DTOP_ADDR_DEFAULT	0x6e	/* power-up default address */
#define	DTOP_ADDR_FIRST		0x52	/* first assignable address */
#define	DTOP_ADDR_LAST		0x6c	/* last, inclusive */

/*
 * Standard messages
 */

/* from host to devices */

#define	DTOP_MSG_RESET		0xf0	/* preceeded by 0x81: P,len 1 */

#define	DTOP_MSG_ID_REQUEST	0xf1	/* preceeded by 0x81: P,len 1 */

#define	DTOP_MSG_ASSIGN_ADDRESS	0xf2	/* preceeded by 0x9e: P,len 30 */
					/* followed by a dtop_id_reply_t */
					/* and by the new_IC_address */

#define	DTOP_MSG_CAP_REQUEST	0xf3	/* preceeded by 0x83: P,len 3 */
					/* followed by a 16 bit u_offset */

#define	DTOP_MSG_APPL_TEST	0xb1	/* preceed by P, sub, len 1 */

/* from devices to host */

#define	DTOP_MSG_ATTENTION	0xe0	/* preceeded by P, len */
#	define DTOP_ATN_OK_STATUS	0x00	/* anything else bad */
					/* followed by 0-30 bytes */

#define	DTOP_MSG_ID_REPLY	0xe1	/* preceeded by P,len (29..32) */

typedef struct {
	unsigned char	module_revision[8];	/* ascii, blank padded */
	unsigned char	vendor_name[8];
	unsigned char	module_name[8];
	int		device_number;	/* 32 bits cpl-2 */
	/* 0-3 optional bytes follow, ignore */
} dtop_id_reply_t;

#define	DTOP_MSG_CAP_REPLY	0xe3	/* preceeded by P,len (3..32) */
					/* followed by 16 bit u_offset */
					/* followed by data */

#define	DTOP_MSG_APPL_SIGNAL	0xa0	/* application level signal */
#	define DTOP_SIG_ATTENTION	0x00
#	define DTOP_SIG_RESET		0x01
#	define DTOP_SIG_HALT		0x02

#define	DTOP_MSG_APPL_TREPLY	0xa1	/* followed by status (0-->ok) */
					/* and 0..30 bytes of result data  */

/* reserved message codes (testing, manifacturing) */

#define	DTOP_MSG_RES0		0xc0
#define	DTOP_MSG_RES1		0xc1
#define	DTOP_MSG_RES2		0xc2
#define	DTOP_MSG_RES3		0xc3


/*
 *	Device specific definitions:  Keyboard
 */

/* from host to keyboard */

#define	DTOP_KMSG_CLICK		0x01	/* preceeded by P, sub len 2 */
#	define	DTOP_CLICK_VOLUME_MAX	0x7	/* followed by one byte */

#define	DTOP_KMSG_BELL		0x02	/* preceeded by P, sub len 2 */
					/* same as above */

#define	DTOP_KMSG_LED		0x03	/* preceeded by P, sub len 2 */
					/* four lower bits turn leds on */

#define	DTOP_KMSG_POLL		0x04	/* preceeded by P, sub len 1 */

/* keyboard sends up to 11 codes in a data message, distinguished values: */
#define	DTOP_KBD_EMPTY		0x00
#define	DTOP_KBD_OUT_ERR	0x01
#define	DTOP_KBD_IN_ERR		0x02

#define	DTOP_KBD_KEY_MIN	0x08
#define	DTOP_KBD_KEY_MAX	0xff

/* powerup status values: 0 ok, else.. */
#define	DTOP_KBD_ROM_FAIL	0x01
#define	DTOP_KBD_RAM_FAIL	0x02
#define	DTOP_KBD_KEY_DOWN	0x03


/*
 *	Device specific definitions:  Locators (mouse)
 */

/* locator sends this type of report data */

typedef struct {
	unsigned short	buttons;	/* 1->pressed */
	short		x;
	short		y;
	short		z;
	/* possibly 3 more dimensions for gloves */
} dtop_locator_msg_t;

#define	DTOP_LMSG_SET_RATE	0x01	/* preceeded by P,sub, len 2 */
					/* followed by sampling interval,
					   from 8 to 25 msecs (0->polled */

#define	DTOP_LMSG_POLL		0x02	/* preceeded by P,sub, len 1 */

/* Powerup codes same as keyboard */


/*
 * Implementation specific definitions
 */

typedef	union {

	dtop_message	unknown_report;

	struct {
		char		last_codes_count;
		unsigned char	last_codes[11];	/* max as per specs */
		unsigned int	last_msec;	/* autorepeat state */
		unsigned short	poll_frequency;
		unsigned char	k_ar_state;
#		define		K_AR_IDLE	0	/* quiescent, no polling */
#		define		K_AR_OFF	4	/* turn off polling pls */
#		define		K_AR_ACTIVE	2	/* polling, no autos yet */
#		define		K_AR_TRIGGER	1	/* sent one autorepeat */
	} keyboard;

	struct {
		unsigned char	type : 7, /* DEV_MOUSE, DEV_TABLET, .. */
				relative : 1;
		unsigned char	n_coords;
		unsigned short	prev_buttons;
#		define		L_BUTTON_MAX	16
		unsigned char	button_code[L_BUTTON_MAX];
#		define		L_COORD_MAX	6
		unsigned int	coordinate[L_COORD_MAX];	/* max 6D */
	} locator;

	/* add more as they come along */

} dtop_device, *dtop_device_t;


#define	DTOP_EVENT_RECEIVE_PACKET	1
#define	DTOP_EVENT_BAD_PACKET		2
#define	DTOP_EVENT_PUTC			4
#define	DTOP_EVENT_POLL			8


/*
 * Device numbers.
 */
#define	DTOPKBD_PORT	0
#define	DTOPMOUSE_PORT	1
#endif	/* _DTOP_H_ */
